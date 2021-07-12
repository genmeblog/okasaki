(ns okasaki.skew-binary-random-access-list
  (:require [okasaki.common :as c]
            [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]))

(defprotocol InternalNodeProto
  (lookup-tree [n w i])
  (update-tree [n w i x]))

(c/make-type Leaf [x] {}
  InternalNodeProto
  (lookup-tree [_ _ i]
    (if (zero? i)
      x
      (throw (Exception. "Index out of bounds."))))
  (update-tree [_ _ i y]
    (if (zero? i)
      (->Leaf y)
      (throw (Exception. "Index out of bounds.")))))

(c/make-type Node [x left right] {}
  InternalNodeProto
  (lookup-tree [_ w i]
    (if (zero? i)
      x
      (let [s2 (quot w 2)]
        (if (<= i s2)
          (lookup-tree left s2 (dec i))
          (lookup-tree right s2 (dec (- i s2)))))))
  (update-tree [_ w i y]
    (if (zero? i)
      (->Node y left right)
      (let [s2 (quot w 2)]
        (if (< i s2)
          (->Node x (update-tree left s2 (dec i) y) right)
          (->Node x left (update-tree right s2 (dec (- i s2)) y)))))))

(defprotocol InternalSkewRandomAccessListProto
  (conj* [l v]) ;; names clash...
  (tail* [l]))

(c/make-type Pair [w t] {})

(c/make-type SkewBinaryRandomAccessList [lst] {:empty? (p/empty? lst)}
  InternalSkewRandomAccessListProto
  (conj* [_ v] (->SkewBinaryRandomAccessList (p/conj lst v)))
  (tail* [_] (->SkewBinaryRandomAccessList (p/tail lst)))
  p/Stack
  (conj [l x]
    (if-not (p/empty? (p/tail lst))
      (let [p1 (p/head lst)
            p2 (p/head (p/tail lst))]
        (if (= (:w p1) (:w p2))
          (conj* (tail* (tail* l)) (->Pair (inc (+ (:w p1) (:w p2)))
                                           (->Node x (:t p1) (:t p2))))
          (conj* l (->Pair 1 (->Leaf x)))))
      (conj* l (->Pair 1 (->Leaf x)))))
  (head [_]
    (if (p/empty? lst)
      nil
      (:x (:t (p/head lst)))))
  (tail [l]
    (if (p/empty? lst)
      nil
      (let [{:keys [w t]} (p/head lst)]
        (if (= 1 w)
          (tail* l)
          (let [s2 (quot w 2)]
            (-> (tail* l)
                (conj* (->Pair s2 (:right t)))
                (conj* (->Pair s2 (:left t)))))))))
  p/Lookup
  (lookup [l i]
    (if (p/empty? lst)
      (throw (Exception. "Index out of bounds."))
      (let [p (p/head lst)]
        (if (< i (:w p))
          (lookup-tree (:t p) (:w p) i)
          (p/lookup (tail* l) (- i (:w p)))))))
  p/RandomAccess
  (update [l i y]
    (if (p/empty? lst)
      (throw (Exception. "Index out of bounds."))        
      (let [p (p/head lst)]
        (if (< i (:w p))
          (conj* (tail* l) (->Pair (:w p) (update-tree (:t p) (:w p) i y)))
          (conj* (p/update (tail* l) (- i (:w p)) y) p)))))
  p/Seq
  (->seq [l] (if (p/empty? l)
               nil
               (conj (p/->seq (p/tail l)) (p/head l)))))

(def empty-list (->SkewBinaryRandomAccessList cs/empty-stack))

(defn ->list
  [xs]
  (reduce p/conj empty-list (reverse xs)))

(p/->structure (->list [11 22 33]))

(p/lookup (->list [11 22 33]) 2)

(p/lookup (p/update (->list [11 2 22 211 44 44 55 66]) 2 -999) 7)

(p/tail (p/tail (->list [333 111 4 2 1])))
