(ns okasaki.skew-binomial-heap
  (:require [okasaki.common :as c]
            [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]))

(defprotocol InternalNodeProto
  (link [t1 t2]))

(c/make-type Node [r x xs c] {}
  InternalNodeProto
  (link [t1 t2]
    (if (p/leq? x (:x t2))
      (->Node (inc r) x xs (p/conj c t2))
      (->Node (inc r) (:x t2) (:xs t2) (p/conj (:c t2) t1)))))

(defn skew-link
  [t1 t2 v]
  (let [{:keys [r x xs c]} (link t1 t2)]
    (if (p/leq? v x)
      (->Node r v (p/conj xs x) c)
      (->Node r x (p/conj xs v) c))))

(defprotocol InternalSkewBinomialHeapProto
  (ins-tree [h t])
  (merge-trees [h1 h2])
  (normalize [h])
  (remove-min-tree [h]))

(declare empty-heap)

(c/make-type SkewBinomialHeap [lst] {:empty? (p/empty? lst)}
  p/Stack
  (conj [_ v] (->SkewBinomialHeap (p/conj lst v)))
  (head [_] (p/head lst))
  (tail [_] (->SkewBinomialHeap (p/tail lst)))
  p/Reverse
  (reverse [_] (->SkewBinomialHeap (p/reverse lst)))
  InternalSkewBinomialHeapProto
  (ins-tree [h t1]
    (if (p/empty? lst)
      (p/conj empty-heap t1)
      (let [t2 (p/head h)]
        (if (< (:r t1) (:r t2))
          (p/conj h t1)
          (ins-tree (p/tail h) (link t1 t2))))))
  (merge-trees [h1 h2]
    (cond
      (p/empty? h1) h2
      (p/empty? h2) h1
      :else (let [t1 (p/head h1)
                  t2 (p/head h2)]
              (cond
                (< (:r t1) (:r t2)) (p/conj (merge-trees (p/tail h1) h2) t1)
                (< (:r t2) (:r t1)) (p/conj (merge-trees h1 (p/tail h2)) t2)
                :else (ins-tree (merge-trees (p/tail h1) (p/tail h2)) (link t1 t2))))))
  (normalize [h]
    (if (p/empty? h) h (ins-tree (p/tail h) (p/head h))))
  (remove-min-tree [h]
    (cond
      (p/empty? h) (throw (Exception. "Empty heap."))
      (p/empty? (p/tail h)) [(p/head h) empty-heap]
      :else (let [t (p/head h)
                  ts (p/tail h)
                  [t' ts'] (remove-min-tree ts)]
              (if (p/leq? (:x t) (:x t'))
                [t ts]
                [t' (p/conj ts' t)]))))
  p/Insert
  (insert [h x]
    (let [tl (p/tail h)]
      (if-not (p/empty? tl)
        (let [t1 (p/head h)
              t2 (p/head tl)]
          (if (= (:r t1) (:r t2))
            (p/conj (p/tail tl) (skew-link t1 t2 x))
            (p/conj h (->Node 0 x cs/empty-stack empty-heap))))
        (p/conj h (->Node 0 x cs/empty-stack empty-heap)))))
  p/Heap
  (merge [h1 h2]
    (merge-trees (normalize h1) (normalize h2)))
  (find-min [h]
    (:x (first (remove-min-tree h))))
  (delete-min [h]
    (let [[{:keys [xs c]} c2] (remove-min-tree h)
          insert-all (fn [xs ts]
                       (if (p/empty? xs)
                         ts
                         (recur (p/tail xs) (p/insert ts (p/head xs)))))]
      (insert-all xs (p/merge (p/reverse c) c2))))
  p/Seq
  (->seq [h] (if (p/empty? h)
               nil
               (conj (p/->seq (p/delete-min h)) (p/find-min h)))))


(def empty-heap (->SkewBinomialHeap cs/empty-stack))

(defn ->heap
  [xs]
  (reduce p/insert empty-heap xs))


(p/->structure (p/merge (->heap [1 2 4 0 -21])
                        (->heap [98 7 6])))


(p/find-min (p/delete-min (p/merge (->heap [1 2 4 0 -21 -100])
                                   (->heap [98 7 6]))))


