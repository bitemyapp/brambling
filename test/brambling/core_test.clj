(ns brambling.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as time]
            [clj-time.coerce :as tc]
            [datomic.api :as d]
            [brambling.moves.ya.bits.db :refer [gen-schema get-db
                                                get-db-conn inject-id
                                                results->entities]]
            [brambling.moves.ya.bits.migrate :refer :all]))

(defn reset-db
  [db-uri]
  (d/delete-database db-uri)
  (d/create-database db-uri))

(def origin "datomic:dev://localhost:4334/origin")
(defn o-conn [] (d/connect origin))
(defn o-db [] (d/db (o-conn)))

(def target "datomic:dev://localhost:4334/target")
(defn t-conn [] (d/connect target))
(defn t-db [] (d/db (t-conn)))

;; (def origin "datomic:mem://origin")
;; (def target "datomic:mem://target")

(def base-schema
  [[:message/uuid :db.type/uuid :db.cardinality/one :db/unique :db.unique/identity]
   [:message/timestamp :db.type/instant]])

(def origin-schema (conj base-schema [:message/action :db.type/string]))
(def target-schema (conj base-schema [:message/action :db.type/long]))

(defn origin-s []
  (mapv #(apply gen-schema %) origin-schema))

(defn target-s []
  (mapv #(apply gen-schema %) target-schema))

(defn example-data [dbid]
  [[:db/add dbid :message/uuid      #uuid "52583763-ffb7-4c68-b7ba-2bef1b4807ff"]
   [:db/add dbid :message/timestamp #inst "2010-11-12T13:14:15.666"]
   [:db/add dbid :message/action    "insert"]])

(defn second-data [dbid]
  [{:message/uuid      #uuid "52583763-ffb7-4c68-b7ba-2bef1b4807ff"
    :message/timestamp #inst "2010-11-12T13:14:15.666"
    :db/id dbid
    :message/action "delete"}])

(defn prepare-db []
  (reset-db origin)
  (reset-db target)
  (let [origin-conn (get-db-conn origin)
        dbid (d/tempid :db.part/user)
        _ @(d/transact origin-conn (origin-s))
        {:keys [db-after tempids]} @(d/transact origin-conn (example-data dbid))
        perm-id (d/resolve-tempid db-after tempids dbid)]
    @(d/transact origin-conn (second-data perm-id))))

(defn get-messages []
  (d/q '[:find ?e :where [?e :message/uuid]] (get-db origin)))

(defn action-string->long [x]
  ({"insert"   0
    "update"   1
    "retrieve" 2
    "delete"   3} x))

(defn action-to-number [v]
  (let [[action id attr val] v]
    (if (= attr :message/action)
      [action id attr (action-string->long val)]
      v)))

(defn migrate-action-to-number [tx]
  (map action-to-number tx))

(def expected-messages
  '({:message/uuid #uuid "52583763-ffb7-4c68-b7ba-2bef1b4807ff"
     :message/timestamp #inst "2010-11-12T13:14:15.666-00:00"
     :message/action 3}))

(deftest ^:migration datomic-migrate-test
  (prepare-db)
  (testing "Add data and get it back"
    (let [existing-message (get-messages)]
      (is (= 1 (count existing-message)))))
  (testing "Can transfer data from origin to destination"
    (let [;; [_ transactee] (transactions-with-schema origin target [migrate-action-to-number] (target-s))
          ;; _              (transact->target (t-conn) transactee)
          _ (migrate->target origin target (target-s) :mappers [migrate-action-to-number])
          messages       (map (partial into {})
                              (results->entities (t-db)
                                (d/q '[:find ?e :where [?e :message/uuid]] (t-db))))]
      (is (= expected-messages messages)))))

(defn gen-action []
  (let [actions ["insert" "update" "retrieve" "delete"]]
    (nth actions (rand-int 4))))

(defn gen-timestamp []
  (-> (time/now)
      (tc/to-long)
      (rand)
      (long)
      (tc/from-long)
      (.toDate)))

(defn gen-uuid []
  (java.util.UUID/randomUUID))

(defn gen-message []
  {:message/uuid      (gen-uuid)
   :message/timestamp (gen-timestamp)
   :message/action    (gen-action)})

(def message-stream (repeatedly gen-message))

(defn transact-message [conn n]
  (when (= (mod n 100) 0)
    (println n))
  (let [messages    (take 1000 message-stream)
        injected    (map (partial inject-id :db.part/user) messages)]
    (d/transact-async conn injected)))

(defn load-into [conn]
  (doall (map deref (pmap #(future ((partial transact-message conn) %)) (range 1e4)))))

(deftest ^:incremental migrating-large-datasets
  (prepare-db)
  (testing "Can migrate a large dataset"
    (let [_       (load-into (o-conn))
          _       (time (migrate->target origin target (target-s) :mappers [migrate-action-to-number]))
          actions (d/q '[:find ?a :where [?e :message/action ?a]] (t-db))]
      (is (= (set (mapcat concat actions)) #{0 1 2 3})))))
