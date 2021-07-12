(ns okasaki.binomial-heap
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(defprotocol InternalNodeProto
  (link [n1 n2])
  (insert-tree [n l]))

(c/make-type Node [rank elem lst] {}
  InternalNodeProto
  (link [n1 n2]
    (if (p/leq? elem (:elem n2))
      (->Node (inc rank) elem (p/conj lst n2))
      (->Node (inc rank) (:elem n2) (p/conj (:lst n2) n1))))
  (insert-tree [n {:keys [head tail] :as l}]
    (if (or (p/empty? l)
            (< rank (:rank head)))
      (p/conj l n)
      (insert-tree (link n head) tail)))
  p/Seq
  (->seq [_] (conj (flatten (p/->seq lst)) elem)))

(defprotocol InternalBinomialHeapProto
  (remove-min-tree [t]))

(c/make-type BinomialHeap [head tail] {:empty-name empty-heap}
  p/Insert
  (insert [t x] (insert-tree (->Node 0 x empty-heap) t))
  InternalBinomialHeapProto
  (remove-min-tree [_]
    (if (p/empty? tail) 
      [head empty-heap]
      (let [[head' tail'] (remove-min-tree tail)]
        (if (p/leq? (:elem head)
                    (:elem head'))
          [head tail]
          [head' (p/conj tail' head)]))))
  p/Stack
  (conj [h v] (->BinomialHeap v h))
  (head [_] head)
  (tail [_] tail)
  p/Reverse
  (reverse [s] (c/reverse-helper s empty-heap))
  p/Heap
  (merge [{t1h :head t1t :tail :as t1}
          {t2h :head t2t :tail :as t2}]
    (if (p/empty? t2)
      t1
      (let [r1 (:rank t1h)
            r2 (:rank t2h)]
        (cond
          (< r1 r2) (p/conj (p/merge t1t t2) t1h)
          (< r2 r1) (p/conj (p/merge t1 t2t) t2h)
          :else (insert-tree (link t1h t2h)
                             (p/merge t1t t2t))))))
  (find-min [t] (:elem (first (remove-min-tree t))))
  (delete-min [t]
    (let [[ts1 ts2] (remove-min-tree t)]
      (p/merge (p/reverse (:lst ts1)) ts2)))
  p/Seq
  (->seq [_] (flatten (conj (p/->seq tail) (p/->seq head)))))

(extend-type EmptyBinomialHeap
  p/Insert
  (insert [t x] (insert-tree (->Node 0 x empty-heap) empty-heap))
  p/Stack
  (conj [h v] (->BinomialHeap v h))
  (head [_] nil)
  (tail [_] empty-heap)
  p/Reverse
  (reverse [h] h)
  p/Heap
  (merge [_ t2] t2)
  (find-min [_] nil)
  (delete-min [t] t))

(defn ->heap
  [xs]
  (reduce p/insert empty-heap xs))


(p/->structure (->heap [1 2 -10 22 12]))

(p/insert (p/insert empty-heap 1) -2)
