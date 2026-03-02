(ns perfectgym-scheduler.core
  (:require
    [babashka.http-client :as http]
    [cheshire.core :as json]
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:import
    (java.time LocalDate LocalDateTime)))

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
                                                       :date   (str date)
                                                       #_#_#_#_#_#_#_#_:categoryId nil
                                                               :timeTableId nil
                                                               :trainerId nil
                                                               :zoneId nil})})
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
      1 (let [[{:keys [Id Status StatusReason]}] matching-classes]
          (if (= "FullBooked" Status)
            (throw (ex-info "Class is already fully booked" {:class class}))
            (-> (assoc-in class [:class-details :class-id] Id)
                (assoc-in [:class-details :class-status] Status)
                (assoc-in [:class-details :class-status-reason] StatusReason))))
      0 (throw (ex-info "Could not find class." {:class       class
                                                 :day-classes day-classes}))
      (throw (ex-info "Insufficient information to match class." {:class       class
                                                                  :day-classes day-classes})))))

(defn schedule-classes []
  (let [config (read-config)
        {{auth "jwt-token"} :headers
         :as                resp} (login! config) ;TODO keep member id from response body?
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
    matched-classes))

(comment
  (fetch-clubs (read-config))
  (login! (read-config))
  (active-club-days (read-config))
  (schedule-classes)
  (-> (read-config)
      ;(update-in [:credentials :password] str "abc")
      (login!)
      json/generate-string))
