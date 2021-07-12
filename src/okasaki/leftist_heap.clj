(ns okasaki.leftist-heap
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(declare make-t)

(c/make-type LeftistHeap [rank left elem right] {:empty-name empty-heap
                                                 :empty-values {:rank 0}}
  p/Insert
  (insert [h v] (p/merge (p/insert empty-heap v) h))
  p/Heap
  (merge [h1 h2]
    (if (p/empty? h2)
      h1
      (if (p/leq? elem (:elem h2))
        (make-t left elem (p/merge right h2))
        (make-t (:left h2) (:elem h2) (p/merge h1 (:right h2))))))
  (find-min [_] elem)
  (delete-min [_] (p/merge left right))
  p/Seq
  (->seq [_] (conj (concat (p/->seq left) (p/->seq right)) elem)))

(extend-type EmptyLeftistHeap
  p/Insert
  (insert [h v]
    (->LeftistHeap 1 empty-heap v empty-heap))
  p/Heap
  (merge [_ h2] h2)
  (find-min [_] nil)
  (delete-min [h] h))

(defn make-t
  [a x b]
  (if (>= (:rank a) (:rank b))
    (->LeftistHeap (inc (:rank b)) a x b)
    (->LeftistHeap (inc (:rank a)) b x a)))

(defn ->heap
  [xs]
  (reduce p/insert empty-heap xs))
