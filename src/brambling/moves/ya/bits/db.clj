(ns brambling.moves.ya.bits.db
  (:require [datomic.api :as d]))

(defn gen-schema
  "Schema doc given an ident, value type, and maybe cardinality
   ident is generally :namespace/attribute-name such as :person/name
   Type can be :db.type/(string|boolean|long|bigint|double|bigdec|
   instant|uuid|uri|bytes|ref)
   Cardinality can be one or many, such as :db.cardinality/one or
   :db.cardinality/many"
   ;; :db/doc "A person's name"
   ;; Future: :db/noHistory, :db/unique, :db.unique/identity,
   ;; :db/isComponent, :db/index, :db/fulltext
  ([ident type]
     (gen-schema ident type :db.cardinality/one))
  ([ident type & [cardinality & rest]]
     (let [mapped (apply hash-map rest)]
       (merge {:db/id (d/tempid :db.part/db)
               :db/ident ident
               :db/valueType type
               :db/cardinality cardinality
               :db.install/_attribute :db.part/db}
              mapped))))

(defn get-db-conn
  "Gets database connecton, ensures db is created"
  [db-uri]
  (d/create-database db-uri)
  (d/connect db-uri))

(defn get-db
  "Gets database instance, ensures db is created"
  [db-uri]
  (d/create-database db-uri)
  (d/db (get-db-conn db-uri)))

(defn eid->entity
  "Takes a db value and an entity id and returns the touched entity."
  [db eid]
  (d/touch (d/entity db eid)))

(defn results->entities
  "takes a db value and a set of datalog results and converts them to touched entities.
  Entity id must be first in the constituent result vectors."
  [db results]
  (map #(eid->entity db (first %)) results))

(defn db->schema [db]
  (map #(eid->entity db %) (mapcat concat (d/q '[:find ?e :where [?e :db/ident]] db))))

(defn db->schema-map [db]
  (into {} (map #((juxt :db/id identity) %) (db->schema db))))

(defn inject-id
  "Given an entity and a database partition, injects a tempid"
  [part entity]
  (assoc entity :db/id (d/tempid part)))
