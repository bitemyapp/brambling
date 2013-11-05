(ns brambling.moves.ya.bits.util)

(defmacro exception [& [param & more :as params]]
 (if (class? param)
   `(throw (new ~param (str ~@(interpose " " more))))
   `(throw (Exception. (str ~@(interpose " " params))))))
