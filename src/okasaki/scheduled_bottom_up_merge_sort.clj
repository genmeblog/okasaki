(ns okasaki.scheduled-bottom-up-merge-sort
  (:require [okasaki.protocols :as p]
            [okasaki.stream :as s]
            [okasaki.common :as c]
            [okasaki.custom-stack :as cs]
            [okasaki.utils :as u])
  (:import [okasaki.stream LazyStream EmptyStream]))

(defprotocol InternalScheduledBottomUpMergeSortProto
  (mrg [s1 s2]))

(extend-protocol InternalScheduledBottomUpMergeSortProto
  LazyStream
  (mrg [s1 s2]
    (if (p/empty? s2)
      s1
      (s/$ (if (p/leq? (:elem @s1) (:elem @s2))
             (s/->Stream (:elem @s1) (mrg (:cons @s1) s2))
             (s/->Stream (:elem @s2) (mrg s1 (:cons @s2)))))))
  EmptyStream
  (mrg [_ s] s))

(defn- exec1
  [xs]
  (cond
    (p/empty? xs) xs
    (p/empty? (p/head xs)) (exec1 (p/tail xs))
    :else (let [s (p/head xs)]
            (p/conj (p/tail xs) (:cons @s)))))

(defn- exec2
  [[xs sched]]
  [xs (exec1 (exec1 sched))])

(c/make-type ScheduledBottomUpMergeSort [size segs] {:empty? (zero? size)}
  p/Additive
  (add [_ x]
    (letfn [(add-seg [xs segs size rsched]
              (if (even? size)
                (p/conj segs [xs (p/reverse rsched)])
                (let [xs'' (mrg xs (first (p/head segs)))]
                  (recur xs'' (p/tail segs) (quot size 2) (p/conj rsched xs'')))))]
      (let [segs' (add-seg (p/insert s/empty-stream x)
                           segs size cs/empty-stack)]
        (->ScheduledBottomUpMergeSort (inc size) (u/map-stack exec2 segs')))))
  p/Sortable
  (sort [_]
    (letfn [(mrg-all [xs ys]
              (if (p/empty? ys)
                xs
                (recur (mrg xs (first (p/head ys))) (p/tail ys))))]
      (u/stream->stack (mrg-all s/empty-stream segs)))))

(def empty-sort (->ScheduledBottomUpMergeSort 0 cs/empty-stack))

(defn ->sort
  [xs]
  (p/sort (reduce p/add empty-sort xs)))

(->sort (shuffle (range 50)))

