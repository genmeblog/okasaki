(ns okasaki.protocols
  (:refer-clojure :exclude [empty? conj merge take drop last reverse sort inc dec update]))

(defprotocol Structure
  (->structure [t]))

(defprotocol Seq
  (->seq [_]))

(defprotocol Empty
  (empty? [s]))

(defprotocol Stack
  (conj [s v])
  (head [s])
  (tail [s]))

(defprotocol Queue
  (snoc [q v]))

(defprotocol Deque
  (last [q])
  (init [q]))

(defprotocol Insert
  (insert [s v]))

(defprotocol Set
  (member? [s v]))

(defprotocol Ordered
  (eq? [a b])
  (lt? [a b])
  (leq? [a b]))

(defprotocol Lookup
  (lookup [m k]))

(defprotocol FiniteMap
  (bind [m k v]))

(defprotocol Heap
  (merge [h1 h2])
  (find-min [h])
  (delete-min [h]))

(defprotocol Reverse
  (reverse [s]))

(defprotocol Stream
  (++ [s1 s2])
  (take [s n])
  (drop [s n]))

(defprotocol Additive
  (add [s t]))

(defprotocol Sortable
  (sort [s]))

(defprotocol Numbers
  (inc [n])
  (dec [n]))

(defprotocol RandomAccess
  (update [ra idx v]))

;;

(extend-type nil
  Empty
  (empty? [_] true)
  Structure
  (->structure [_] nil)
  Seq
  (->seq [_] nil))

(extend-type Object
  Ordered
  (eq? [a b] (= a b))
  (lt? [a b] (neg? (compare a b)))
  (leq? [a b] (not (pos? (compare a b))))
  Structure
  (->structure [t] t))

(extend-type clojure.lang.Delay
  Structure
  (->structure [d] [:lazy (->structure @d)])
  Seq
  (->seq [d] (->seq @d)))

