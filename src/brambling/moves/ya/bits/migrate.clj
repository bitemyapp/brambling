(ns brambling.moves.ya.bits.migrate
  (:require [datomic.api :as d]
            [clj-time.core :as time]
            [brambling.moves.ya.bits.db :refer [db->schema-map eid->entity get-db-conn]]
            [brambling.moves.ya.bits.util :refer [exception]]))

(defn beginning-of-time []
  (.toDate (time/epoch)))

(defn da-future []
  (.toDate (time/plus (time/today) (time/days 2))))

(defn tx-id->tx-entity [db tx-id]
  (eid->entity db (d/t->tx tx-id)))

(defn eid->part-ident [db eid]
  (:db/ident (eid->entity db (d/part eid))))

(defn eid->temp-id [db eid]
  (d/tempid (eid->part-ident db eid)))

(defn tx-log [db-conn start end]
  (d/tx-range (d/log db-conn) start end))

(defn data->tx
  "#Datum{:e 13194139537299 :a 50
   :v #inst \"2013-10-11T17:36:38.900-00:00\" :tx 13194139537299 :added true}
   => [:db/add 13194139537299 :db/txInstant #inst \"2013-10-11T17:36:38.900-00:00\"]"
  [schema datum]
  (let [verb         ({true :db/add false :db/retract} (:added datum))
        attribute-id (:a datum)
        attribute    (:db/ident (schema attribute-id))]
    (when-not verb
      (exception "Dude. :added in the log item has to be true or false. Wtf is going on?"))
    (when-not attribute
      (exception "attribute-id was nil or couldn't be found in the attribute id->map schema"))
    [verb (:e datum) attribute (:v datum)]))

(defn log->tx [schema log-item]
  (map (partial data->tx schema) (:data log-item)))

(defn datum->part-id [id datum]
  (let [[verb _ attr value] datum]
    [verb id attr value]))

(defn conn-and-db [uri]
  (let [db-conn (get-db-conn uri)
        db      (d/db db-conn)]
    [db-conn db]))

(defn temp-or-extant [n]
  (let [klass (class n)]
    (if (and (= klass java.lang.Long) (pos? n))
      :extant
      (when n
        :tempid))))

(defn groupings->temp-ids
  "Can't directly transact primary keys from one database into another.
  Have to use a temporary id intermediary. Groupings is the datoms
  in a given transaction grouped by entity id (primary key). If the
  primary key hasn't been seen before, a temp-id is generated and the
  mapping is updated.

  [:db/add 1 :attr :muh-data] =>
  [:db/add #db/id[:db.part/user -10000199] :attr :muh-data] &&
  {1 #db/id[:db.part/user -10000199]}

  Using the above mapping, we can translate all future datoms
  related to entity 1"
  [db mapping groupings]
  (reduce (fn [[mapping results] [id data]]
            (let [id?     (mapping id)
                  id-type (temp-or-extant id?)
                  use-id? (contains? #{:extant :tempid} id-type)
                  tempid  (and (not use-id?)
                               (eid->temp-id db id))
                  new-mapping (or (and (not use-id?)
                                       (assoc mapping id tempid)) mapping)
                  new-id  (or tempid (and use-id? id?))
                  new-results (concat results
                                (mapv (partial datum->part-id new-id) data))]
              [new-mapping new-results]))
          [mapping []] groupings))

(defn translate-transactions
  "Origin and dest should be URIs, mappers should be [fn]"
  ([origin mappers]
     (let [start           (beginning-of-time)
           end             (da-future)]
       (translate-transactions origin mappers start end)))

  ([origin mappers start end]
     (let [[conn db]   (conn-and-db origin)
           muh-log         (tx-log conn start end)
           schema          (db->schema-map db)]
       (translate-transactions conn db mappers schema muh-log {})))

  ([conn db mappers schema muh-log id-map]
     (if-let [log-item   (first muh-log)]
       (let [translated (log->tx schema log-item)
             grouped    (group-by second translated)
             [new-mapping new-results] (groupings->temp-ids db id-map grouped)
             mapped-results (reduce (fn [c v] (v c)) new-results mappers)]
         (cons [new-mapping mapped-results]
               (lazy-seq (translate-transactions
                          conn db mappers schema
                          (next muh-log) new-mapping))))
       nil)))

(defn v->val [v]
  (nth v 3))

(defn v->attr [v]
  (nth v 2))

(defn v->id [v]
  (nth v 1))

(defn tx-time [tx]
  (filter #(= (v->attr %) :db/txInstant) (filter vector? tx)))

(defn tx-item->id [i]
  (cond
   (vector? i) (v->id i)
   (map? i)    (:db/id i)))

(defn tx-item-replace-id [i id]
  (cond
   (vector? i) (let [[verb _ attr val] i] [verb id attr val])
   (map?    i) (assoc i :db/id id)))

(defn is-schema [m-or-v]
  (cond
   (vector? m-or-v) (= (:part (v->id m-or-v)) :db.part/db)
   (map? m-or-v) (= (:part (:db/id m-or-v)) :db.part/db)))

(defn drop-schema [[mapping tx]]
  [mapping (remove is-schema tx)])

(defn transactions-with-schema [origin dest mappers new-schema]
  (let [translated   (translate-transactions origin mappers)
        [_ first-tx] (first translated)
        first-time   (v->val (first (tx-time first-tx)))
        timed-schema (conj new-schema [:db/add (d/tempid :db.part/tx) :db/txInstant first-time])]
    (concat [[{} timed-schema]] (map drop-schema translated))))

(defn transact->target [target-conn txes]
  (reduce (fn [mapping tx]
            (let [temps-in-tx   (into {} (map vector (map tx-item->id tx) (repeat nil)))
                  new-mapping   (merge temps-in-tx mapping)
                  unbound-temps (map first (filter #(nil? (second %)) new-mapping))
                  tx-with-extant-ids (map
                                      (fn [tx-item]
                                        (let [id (tx-item->id tx-item)
                                              new-id (or (new-mapping id) id)]
                                          (tx-item-replace-id tx-item new-id)))
                                      tx)
                  {:keys [db-after tempids]} @(d/transact target-conn tx-with-extant-ids)]
              (reduce (fn [m u]
                        (assoc m u (d/resolve-tempid db-after tempids u)))
                      new-mapping unbound-temps)))
            {} txes))

(defn migrate->target
  "origin dburi, target dburi, vector of mapper fns,
  vector of transactable new schema entities"
  [origin target new-schema & {:keys [mappers]
                               :or   {mappers []}}]
  (let [t-conn (d/connect target)
        txes-and-mappings (transactions-with-schema origin target mappers new-schema)
        txes (map second txes-and-mappings)]
    (transact->target t-conn txes)))
