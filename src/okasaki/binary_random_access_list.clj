(ns okasaki.binary-random-access-list
  (:require [okasaki.common :as c]
            [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]))

(defprotocol InternalNodeProto
  (node-size [n])
  (lookup-tree [n i])
  (update-tree [n i x]))

(c/make-type Leaf [x] {}
  InternalNodeProto
  (node-size [_] 1)
  (lookup-tree [_ i]
    (if (zero? i)
      x
      (throw (Exception. "Index out of bounds."))))
  (update-tree [_ i y]
    (if (zero? i)
      (->Leaf y)
      (throw (Exception. "Index out of bounds.")))))

(c/make-type Node [w left right] {}
  InternalNodeProto
  (node-size [_] w)
  (lookup-tree [_ i]
    (let [s2 (quot w 2)]
      (if (< i s2)
        (lookup-tree left i)
        (lookup-tree right (- i s2)))))
  (update-tree [_ i y]
    (let [s2 (quot w 2)]
      (if (< i s2)
        (->Node w (update-tree left i y) right)
        (->Node w left (update-tree right (- i s2) y))))))

(defn link
  [t1 t2]
  (->Node (+ (node-size t1) (node-size t2)) t1 t2))

(c/make-type Zero [] {})
(c/make-type One [tree] {})
(def zero (->Zero))

(defprotocol InternalRandomAccessListProto
  (conj* [l v]) ;; names clash...
  (tail* [l])
  (cons-tree [l t])
  (uncons-tree [l]))

(declare empty-list)

(c/make-type BinaryRandomAccessList [lst] {:empty? (p/empty? lst)}
  InternalRandomAccessListProto
  (conj* [_ v] (->BinaryRandomAccessList (p/conj lst v)))
  (tail* [_] (->BinaryRandomAccessList (p/tail lst)))
  (cons-tree [l t]
    (cond
      (p/empty? lst) (conj* empty-list (->One t))
      (instance? Zero (p/head lst)) (conj* (tail* l) (->One t))
      :else (conj* (cons-tree (tail* l) (link t (:tree (p/head lst)))) zero)))
  (uncons-tree [l]
    (cond
      (p/empty? lst) (throw (Exception. "Empty list"))
      (and (p/empty? (p/tail lst))
           (instance? One (p/head lst))) [(:tree (p/head lst)) empty-list]
      (instance? One (p/head lst)) [(:tree (p/head lst)) (conj* (tail* l) zero)]
      :else (let [[t ts'] (uncons-tree (tail* l))]
              [(:left t) (conj* ts' (->One (:right t)))])))
  p/Stack
  (conj [l x] (cons-tree l (->Leaf x)))
  (head [l] (:x (first (uncons-tree l))))
  (tail [l] (second (uncons-tree l)))
  p/Lookup
  (lookup [l i]
    (cond
      (p/empty? lst) (throw (Exception. "Index out of bounds."))
      (instance? Zero (p/head lst)) (p/lookup (tail* l) i)
      :else (let [t (:tree (p/head lst))]
              (if (< i (node-size t))
                (lookup-tree t i)
                (p/lookup (tail* l) (- i (node-size t)))))))
  p/RandomAccess
  (update [l i y]
    (cond
      (p/empty? lst) (throw (Exception. "Index out of bounds."))
      (instance? Zero (p/head lst)) (conj* (p/update (tail* l) i y) zero)
      :else (let [t (:tree (p/head lst))]
              (if (< i (node-size t))
                (conj* (tail* l) (->One (update-tree t i y)))
                (conj* (p/update (tail* l) (- i (node-size t)) y) (p/head lst))))))
  p/Seq
  (->seq [l] (if (p/empty? l)
               nil
               (conj (p/->seq (p/tail l)) (p/head l)))))

(def empty-list (->BinaryRandomAccessList cs/empty-stack))

(defn ->list
  [xs]
  (reduce p/conj empty-list (reverse xs)))

(p/lookup (->list [11 2 22 211 44 44 55 66]) 2)

(p/update (->list [11 2 22 211 44 44 55 66]) 2 -999)
