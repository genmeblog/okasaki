(ns okasaki.bottom-up-merge-sort
  (:require [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]
            [okasaki.common :as c])
  (:import [okasaki.custom_stack CustomStack EmptyCustomStack]))


(defprotocol InternalBottomUpMergeSortProto
  (mrg [xs ys]))

(c/make-type BottomUpMergeSort [size segs] {:empty? (zero? size)}
  p/Additive
  (add [_ x]
    (letfn [(add-seg [seg segs size]
              (if (even? size)
                (p/conj segs seg)
                (recur (mrg seg (:head segs)) (:tail segs) (quot size 2))))]
      (->BottomUpMergeSort (inc size) (delay (add-seg (p/conj cs/empty-stack x)
                                                      @segs size)))))
  p/Sortable
  (sort [_]
    (letfn [(mrg-all [xs ys]
              (if (p/empty? ys)
                xs
                (recur (mrg xs (:head ys)) (:tail ys))))]
      (mrg-all cs/empty-stack @segs))))

(extend-type CustomStack
  InternalBottomUpMergeSortProto
  (mrg [xs ys]
    (if (p/empty? ys)
      xs
      (let [{x :head xs' :tail} xs
            {y :head ys' :tail} ys]
        (if (p/leq? x y)
          (p/conj (mrg xs' ys) x)
          (p/conj (mrg xs ys') y))))))

(extend-type EmptyCustomStack
  InternalBottomUpMergeSortProto
  (mrg [_ ys] ys))

(def empty-sort (->BottomUpMergeSort 0 (delay cs/empty-stack)))

;;

(p/empty? empty-sort)

(defn ->sort
  [xs]
  (p/sort (reduce p/add empty-sort xs)))

(->sort (shuffle (concat (range 100)
                         (range 20))))
