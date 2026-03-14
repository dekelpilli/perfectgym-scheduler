(ns perfectgym-scheduler.core
  (:require
    [babashka.cli :as cli]
    [babashka.fs :as fs]
    [babashka.http-client :as http]
    [babashka.http-client.interceptors :as interceptors]
    [cheshire.core :as json]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.stacktrace :as stacktrace]
    [clojure.string :as str])
  (:import
    (java.io File)
    (java.time LocalDate LocalDateTime)
    (java.time.format DateTimeFormatter)))

(defn login! [{:keys                    [url]
               {:keys [login password]} :credentials}]
  (let [url (str url "/clientportal2/Auth/Login")]
    (http/request {:method  :post
                   :uri     url
                   :headers {"content-type" "application/json"}
                   :body    (json/generate-string {:RememberMe false
                                                   :Password   password
                                                   :Login      login})})))

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

(defn read-json-file [config-file]
  (-> (io/reader config-file)
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
                :as   matched}] matching-classes
              class-action (case Status
                             "FullBooked" (throw (ex-info "Class is already fully booked"
                                                          {:class         class
                                                           :matched-class matched}))
                             "Unavailable" (if (= StatusReason "Too soon to book")
                                             ::schedule
                                             (throw (ex-info "Class is unavailable."
                                                             {:class         class
                                                              :matched-class matched})))
                             ::book)]
          (-> (assoc-in class [:class-details :id] Id)
              (assoc-in [:class-details :action] class-action)))
      0 (throw (ex-info "Could not find class." {:class       class
                                                 :day-classes day-classes}))
      (throw (ex-info "Insufficient information to match class." {:class       class
                                                                  :day-classes day-classes})))))

(defn- book-class! [{:keys [url]} auth-headers {:keys [id club-id]} & http-config]
  (let [url (str url "/clientportal2/Classes/ClassCalendar/BookClass")
        {:keys [body]} (http/request (assoc http-config
                                       :method :post
                                       :uri url
                                       :headers (assoc auth-headers
                                                  "content-type" "application/json")
                                       :body (json/generate-string {:clubId  (str club-id)
                                                                    :classId id})))]
    {:booked (json/parse-string body true)}))

(defn- schedule-class! [{:keys [run-id url credentials]}
                        {:keys                [index start]
                         {:keys [id club-id]} :class-details
                         :as                  class}]
  ;currently only intended for use with Windows
  ;https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/schtasks-create
  (let [task-name (str "perfectgym-" run-id "-" index)
        config-file-path (-> (str "runs/" run-id "/" index ".json")
                             io/file
                             fs/absolutize
                             str)
        _ (->> (json/generate-string {:booking-details {:club-id  club-id
                                                        :class-id id}
                                      :url             url
                                      :credentials     credentials})
               (spit config-file-path))
        schedule-datetime (-> (LocalDateTime/parse start)
                              (.minusHours 46)
                              (.minusMinutes 1))
        {:keys [exit]
         :as   result} (shell/sh "schtasks" "/create"
                                 "/sc" "ONCE"
                                 "/tr" (as-> (str/join " " ["bb" *file* "--book" config-file-path]) command
                                             (str \" command \"))
                                 "/st" (.format schedule-datetime (DateTimeFormatter/ofPattern "HH:mm"))
                                 "/sd" (.format schedule-datetime (DateTimeFormatter/ofPattern "dd/MM/yyyy")) ;TODO add config override?
                                 "/tn" task-name)]
    (if-not (zero? exit)
      (throw (ex-info "Failed to schedule task" {:class  class
                                                 :result result}))
      {:scheduled task-name})))

(defn schedule-or-book-class! [config auth-headers {{:keys [action]
                                                     :as   class-details} :class-details
                                                    :as                   class}]
  (case action
    ::book (book-class! config auth-headers class-details)
    ::schedule (schedule-class! config class)))

(defn schedule-classes! [config]
  (let [{{auth "jwt-token"} :headers
         :as                resp} (login! config)
        _ (when (str/blank? auth)
            (throw (ex-info "Bad login response" resp)))
        auth-headers {"authorization" (str "Bearer " auth)}
        clubs (fetch-clubs config)
        classes (add-class-details clubs config)
        matched-classes (map-indexed (fn [idx {:keys [class-details]
                                               :as   class}]
                                       (-> (assoc class :index idx)
                                           (match-class (day-club-classes config class-details))))
                                     classes)]
    (mapv
      (partial schedule-or-book-class! config auth-headers)
      matched-classes)))

(defn- write-report! [{:keys [run-id]
                       :as   config}]
  (let [report-content (-> (dissoc config :credentials)
                           (json/generate-string {:pretty true}))]
    (-> (str "runs/" run-id "/report.json")
        io/file
        fs/absolutize
        str
        (spit report-content))))

(defn execute-scheduling! [file run-id]
  (try
    (let [config (assoc (read-json-file file) :run-id run-id)]
      (try
        (println "RUN ID:" run-id)
        (-> (str "runs/" run-id)
            (io/file)
            (.mkdirs))
        (-> (assoc config :results (schedule-classes! config))
            write-report!)
        (catch Exception e
          (-> (assoc config :error {:type       (str (class e))
                                    :message    (ex-message e)
                                    :data       (-> (ex-data e)
                                                    (dissoc :uri)
                                                    (update :request dissoc :uri))
                                    :stacktrace (with-out-str (stacktrace/print-stack-trace e))})
              write-report!))))
    (println (str "Done. Report is available at runs/" run-id "/report.json"))
    (catch Exception e
      (println "Error while setting up program (e.g. while reading config) - "
               (ex-message e)
               (with-out-str (stacktrace/print-stack-trace e))))))

(defn execute-booking! [file]
  (let [{:keys [booking-details]
         :as   config} (read-json-file file)
        {{auth "jwt-token"} :headers
         :as                resp} (login! config)
        _ (when (str/blank? auth)
            (throw (ex-info "Bad login response" resp)))
        auth-headers {"authorization" (str "Bearer " auth)}]
    (loop []
      (let [retry? (try
                     (let [{:keys [status body]} (book-class!
                                                   config auth-headers booking-details
                                                   :interceptors
                                                   (filterv (comp not #{::interceptors/throw-on-exceptional-status-code} :name)
                                                            interceptors/default-interceptors))]
                       (cond
                         (interceptors/unexceptional-statuses status) (println "Class booked successfully.")
                         (= status 409) (do (Thread/sleep 10000) true) ;TODO check what response is actually given for trying to book too early. Make sure it's different from trying to book a full class.
                         :else (println "Failed to book class." status body)))
                     (catch Exception e
                       (println "Error while attempting to book class"
                                (ex-message e)
                                (with-out-str (stacktrace/print-stack-trace e)))))]
        (when retry?
          (recur))))))

(def cli-spec
  {:spec {:schedule {:coerce   fs/file
                     :desc     "Path to the config file for scheduling a future bookings."
                     :validate fs/exists?}
          :book     {:coerce   fs/file
                     :desc     "Path to the config file for booking classes."
                     :validate fs/exists?}}})

(defn -main [args]
  (let [{:keys [help schedule book]} (cli/parse-opts args cli-spec)]
    (cond
      help (println (cli/format-opts cli-spec))
      schedule (->> (random-uuid) str (execute-scheduling! schedule))
      book (execute-booking! book)))
  (Thread/sleep 5000))

(when (= *file* (System/getProperty "babashka.file"))
  (-main *command-line-args*))

(comment
  (read-json-file (fs/file "resources/config.json"))
  (fetch-clubs (read-json-file (fs/file "resources/config.json")))
  (login! (read-json-file (fs/file "resources/config.json")))
  (active-club-days (read-json-file (fs/file "resources/config.json")))
  (schedule-classes! (read-json-file (fs/file "resources/config.json")))
  (-> (read-json-file (fs/file "resources/config.json"))
      ;(update-in [:credentials :password] str "abc")
      (login!)
      json/generate-string))
