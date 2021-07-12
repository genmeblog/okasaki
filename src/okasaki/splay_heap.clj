(ns okasaki.splay-heap
  (:refer-clojure :exclude [partition])
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(defprotocol InternalSplayHeapProto
  (partition [h p]))

(c/make-type SplayHeap [left elem right] {:empty-name empty-heap}
  InternalSplayHeapProto
  (partition [h pivot]
    (if (p/leq? elem pivot)
      (if (p/empty? right)
        [h empty-heap]
        (if (p/leq? (:elem right) pivot)
          (let [[small big] (partition (:right right) pivot)]
            [(->SplayHeap (->SplayHeap left elem (:left right)) (:elem right) small) big])
          (let [[small big] (partition (:left right) pivot)]
            [(->SplayHeap left elem small) (->SplayHeap big (:elem right) (:right right))])))
      (if (p/empty? left)
        [empty-heap h]
        (if (p/leq? (:elem left) pivot)
          (let [[small big] (partition (:right left) pivot)]
            [(->SplayHeap (:left left) (:elem left) small) (->SplayHeap big elem right)])
          (let [[small big] (partition (:left left) pivot)]
            [small (->SplayHeap big (:elem left) (->SplayHeap (:right left) elem right))])))))
  p/Insert
  (insert [h v]
    (let [[a b] (partition h v)]
      (->SplayHeap a v b)))
  p/Heap
  (merge [_ h2]
    (let [[ta tb] (partition h2 elem)]
      (->SplayHeap (p/merge ta left) elem (p/merge tb right))))
  (find-min [_]
    (if (p/empty? left)
      elem
      (p/find-min left)))
  (delete-min [_]
    (cond
      (p/empty? left) right
      (p/empty? (:left left)) (->SplayHeap (:right left) elem right)
      :else (->SplayHeap (p/delete-min (:left left)) (:elem left) (->SplayHeap (:right left) elem right))))
  p/Seq
  (->seq [_] (conj (concat (p/->seq left) (p/->seq right)) elem)))

(extend-type EmptySplayHeap
  InternalSplayHeapProto
  (partition [_ _] [empty-heap empty-heap])
  p/Insert
  (insert [_ v] (->SplayHeap empty-heap v empty-heap))
  p/Heap
  (merge [_ h] h)
  (find-min [_] nil)
  (delete-min [_] empty-heap))

(defn ->heap
  [xs]
  (reduce p/insert empty-heap xs))


(p/find-min (p/delete-min (p/delete-min (p/merge
                                         (->heap (repeatedly 200 #(- (rand-int 20) 10)))
                                         (p/merge (->heap (shuffle (range 10)))
                                                  (->heap (shuffle (range 20 30))))))))
