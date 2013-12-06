# brambling

!["Brambling (bird)"](brambling.jpg)

A (Clojure) toolkit for managing Datomic migrations

## Leiningen

!["Leiningen version"](https://clojars.org/brambling/latest-version.svg)

## Usage

Given a simple use-case like wanting to turn strings into an integral mapping:

```clojure
(def origin "datomic:dev://localhost:4334/origin")
(def target "datomic:dev://localhost:4334/target")

(def new-schema [
  {:db/unique :db.unique/identity,
   :db/id #db/id[:db.part/db -1000000],
   :db/ident :message/uuid,
   :db/valueType :db.type/uuid,
   :db/cardinality :db.cardinality/one,
   :db.install/_attribute :db.part/db}

  {:db/id #db/id[:db.part/db -1000001],
   :db/ident :message/timestamp,
   :db/valueType :db.type/instant,
   :db/cardinality :db.cardinality/one,
   :db.install/_attribute :db.part/db}

   {:db/id #db/id[:db.part/db -1000002],
    :db/ident :message/action,
    :db/valueType :db.type/long,
    :db/cardinality :db.cardinality/one,
    :db.install/_attribute :db.part/db}
]

;; Don't def resources with side-effects at the top-level of a namespace.
(defn t-conn [] (d/connect target))

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

(defn run-migration []
  ;; transactions-with-schema generates the transactions (in case you want to inspect first)
  (let [[_ transactee] (transactions-with-schema origin target [migrate-action-to-number] new-schema)]
    ;; transact->target, given a connection and the output of transactions-with-schema, will
    ;; incrementally transact each tx in the transactee, starting with the schema originally provided.
    (transact->target (t-conn) transactee)))
        
```

## Supported

Per transaction mapping functions that can choose to translate or drop datoms.

Lazy sequence of origin translated transactions so that the whole history isn't retained in memory. This should suffice as an incremental/streaming migration implementation.

## Not Supported, possibly coming later

Reader/writer ring-buffer/core.async backed parallelization

Differential migrations

Folding migrations (not just map, would allow collapsing transactions)

## Never going to be supported

Migrating between from/to databases that aren't Datomic.

## License

Copyright © 2013 Chris Allen

Thanks to InVitae for being an awesome company where I can open source things. :)

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
