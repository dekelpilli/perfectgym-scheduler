(ns perfectgym-scheduler.core
  (:require
    [babashka.http-client :as http]
    [cheshire.core :as json]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.stacktrace :as stacktrace]
    [clojure.string :as str])
  (:import
    (java.time LocalDate LocalDateTime)
    (java.time.format DateTimeFormatter)))

(defn login! [{:keys                    [url]
               {:keys [email password]} :credentials}]
  (let [url (str url "/clientportal2/Auth/Login")]
    (http/request {:method  :post
                   :uri     url
                   :headers {"content-type" "application/json"}
                   :body    (json/generate-string {:RememberMe true
                                                   :Password   password
                                                   :Login      email})})))

(defn fetch-clubs [{:keys [url]}]
  (let [url (str url "/clientportal2/Clubs/GetAvailableClassesClubs")]
    (-> (http/request {:method :get
                       :uri    url})
        :body
        (json/parse-string true))))

(defn fetch-day-club-classes [{:keys [url]} {:keys [club-id ^LocalDate date]}]
  (let [url (str url "/clientportal2/Classes/ClassCalendar/DailyClasses")]
    (-> (http/request {:method  :post
                       :uri     url
                       :headers {"content-type" "application/json"}
                       :body    (json/generate-string {:clubId club-id
                                                       :date   (str date)})})
        :body
        (json/parse-string true)
        :CalendarData
        (->> (mapv :Classes))
        flatten)))

(def day-club-classes (memoize fetch-day-club-classes))

(defn read-config []
  (-> (io/resource "config.json")
      io/reader
      (json/parse-stream true)))

(defn add-class-details [clubs {:keys [classes]}]
  (let [clubs (mapv (fn [club] (update club :Name str/lower-case)) clubs)]
    (mapv
      (fn [{:keys [club start]
            :as   class}] (assoc class :class-details
                                       {:club-id (some
                                                   (fn [{:keys [Id Name]}]
                                                     (when (str/includes? Name (str/lower-case club))
                                                       Id))
                                                   clubs)
                                        :date    (-> (LocalDateTime/parse start)
                                                     (.toLocalDate)
                                                     str)}))
      classes)))

(defn match-class [{:keys [start name trainer]
                    :as   class}
                   day-classes]
  (let [matching-classes (filterv
                           (fn [{:keys [StartTime Name Trainer]}]
                             (and (= StartTime start)
                                  (str/includes? (str/lower-case Name) (str/lower-case name))
                                  (or (nil? trainer)
                                      (str/includes? (str/lower-case Trainer) (str/lower-case trainer)))))
                           day-classes)]
    (case (count matching-classes)
      1 (let [[{:keys [Id Status StatusReason]
                :as   matched}] matching-classes]
          (cond
            (= "FullBooked" Status) (throw (ex-info "Class is already fully booked"
                                                    {:class         class
                                                     :matched-class matched}))
            (and (= "Unavailable" Status)
                 (not= StatusReason "Too soon to book") (throw (ex-info "Class is unavailable."
                                                                        {:class         class
                                                                         :matched-class matched})))
            (-> (assoc-in class [:class-details :id] Id)
                (assoc-in [:class-details :status] Status)
                (assoc-in [:class-details :status-reason] StatusReason))))
      0 (throw (ex-info "Could not find class." {:class       class
                                                 :day-classes day-classes}))
      (throw (ex-info "Insufficient information to match class." {:class       class
                                                                  :day-classes day-classes})))))

(defn- book-class! [{:keys [url]} auth-headers {{:keys [id club-id]} :class-details}]
  (let [url (str url "/clientportal2/Classes/ClassCalendar/BookClass")]
    (http/request {:method  :post
                   :uri     url
                   :headers (assoc auth-headers
                              "content-type" "application/json")
                   :body    (json/generate-string {:clubId  (str club-id)
                                                   :classId id})})))

(defn- schedule-class! [{:keys [run-id url]}
                        auth-headers
                        {:keys                [name trainer start]
                         {:keys [id club-id]} :class-details}]
  ;currently only intended for use with Windows
  ;https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/schtasks-create
  (let [req-name-prefix (str (str/replace name #"[^a-zA-Z0-9]" "_")
                             "_" start
                             (when trainer (str/replace trainer #"[^a-zA-Z0-9]" "_")))
        curl-request (as-> ["curl.exe"
                            "--request" "POST"
                            (str url "/clientportal2/Classes/ClassCalendar/BookClass")
                            "--data-raw"
                            (json/generate-string {:clubId  (str club-id)
                                                   :classId id})] request-bits
                           (into request-bits
                                 (mapcat (fn [[k v]] ["-H" (str k ": " v)]))
                                 (assoc auth-headers
                                   "content-type" "application/json"))
                           (str/join " " request-bits))
        start-local-datetime (LocalDateTime/parse start)]
    (shell/sh "schtasks" "/create"
              "/sc" "ONCE"
              "/tr" curl-request
              "/st" (-> (.toLocalTime start-local-datetime)
                        (.plusMinutes 1) ;avoid clock differences
                        (.format (DateTimeFormatter/ofPattern "HH:mm")))
              "/sd" (-> (.toLocalDate start-local-datetime)
                        (.minusDays 2)
                        (.format (DateTimeFormatter/ofPattern "MM/dd/yyyy")))
              "/tn" (str req-name-prefix "_" run-id))))

(defn schedule-or-book-class! [config auth-headers {{:keys [status]} :class-details
                                                    :as              class}]
  (case status
    ("Bookable" "Awaitable") (book-class! config auth-headers class)
    "Unavailable" (schedule-class! config auth-headers class)
    (throw (ex-info ("Unknown class status") {:class class}))))

(defn schedule-classes! [config]
  (let [{{auth "jwt-token"} :headers
         :as                resp} (login! config)
        _ (when (str/blank? auth)
            (throw (ex-info "Bad login response" resp)))
        auth-headers {"authorization" (str "Bearer " auth)}
        clubs (fetch-clubs config)
        classes (add-class-details clubs config)
        matched-classes (mapv (fn [{:keys [class-details]
                                    :as   class}]
                                (->> (day-club-classes config class-details)
                                     (match-class class)))
                              classes)]
    (run!
      (partial schedule-or-book-class! config auth-headers)
      matched-classes)))

(when (= *file* (System/getProperty "babashka.file"))
  (try
    (let [config (read-config)
          run-id (str (random-uuid))]
      (try
        (println "RUN ID:" run-id)
        (-> (str "runs/" run-id)
            (io/file)
            (.mkdirs))
        (-> (assoc config :run-id run-id)
            schedule-classes!)
        ;TODO write success report.json (config minus creds?)
        (catch Exception e
          ;TODO write error report.json
          )))
    (catch Exception e
      (println "Error while setting up program (e.g. while reading config) - "
               (ex-message e)
               (stacktrace/print-stack-trace e)))))

(comment
  (fetch-clubs (read-config))
  (login! (read-config))
  (active-club-days (read-config))
  (schedule-classes!)
  (-> (read-config)
      ;(update-in [:credentials :password] str "abc")
      (login!)
      json/generate-string))
