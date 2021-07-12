(ns okasaki.scheduled-binomial-heap
  (:require [okasaki.protocols :as p]
            [okasaki.stream :as s]
            [okasaki.custom-stack :as cs]
            [okasaki.common :as c]
            [okasaki.utils :as u]))

(c/make-type Zero [] {}
  p/Seq
  (->seq [_] nil))
(c/make-type One [tree] {}
  p/Seq
  (->seq [_] (p/->seq tree)))

(def zero (->Zero))

(defprotocol InternalNodeProto
  (link [n1 n2])
  (ins-tree [n s]))

(c/make-type Node [elem trees] {}
  InternalNodeProto
  (link [n1 n2]
    (if (p/leq? elem (:elem n2))
      (->Node elem (p/conj trees n2))
      (->Node (:elem n2) (p/conj (:trees n2) n1))))
  (ins-tree [n s]
    (cond
      (p/empty? s) (p/insert s/empty-stream (->One n))
      (instance? Zero (:elem @s)) (p/insert (:cons @s) (->One n))
      :else (p/insert (ins-tree (link n (:tree (:elem @s))) (:cons @s)) zero)))
  p/Seq
  (->seq [_] (conj (mapcat p/->seq (p/->seq trees)) elem)))

(defn- mrg
  [s1 s2]
  (cond
    (p/empty? s1) s2
    (p/empty? s2) s1
    (instance? Zero (:elem @s1)) (p/insert (mrg (:cons @s1) (:cons @s2)) (:elem @s2))
    (instance? Zero (:elem @s2)) (p/insert (mrg (:cons @s1) (:cons @s2)) (:elem @s1))
    :else (p/insert (ins-tree (link (:tree (:elem @s1))
                                    (:tree (:elem @s2))) (mrg (:cons @s1) (:cons @s2))) zero)))

(defn- normalize
  "Force all delays"
  [ds]
  (if (p/empty? ds)
    ds
    (do (normalize (:cons @ds)) ds)))

(defn- exec
  [lst]
  (cond
    (p/empty? lst) lst
    (instance? Zero (:elem @(p/head lst))) (p/conj (p/tail lst) (:cons @(p/head lst)))
    :else (p/tail lst)))

(defn- remove-min-tree
  [s]
  (cond
    (p/empty? s) nil
    (and (p/empty? (:cons @s))
         (instance? One (:elem @s))) [(:tree (:elem @s)) s/empty-stream]
    (instance? Zero (:elem @s)) (let [[t' ds'] (remove-min-tree (:cons @s))]
                                  [t' (p/insert ds' zero)])
    :else (let [t (:tree (:elem @s))
                ds (:cons @s)
                [t' ds'] (remove-min-tree ds)]
            (if (p/leq? (:elem t) (:elem t'))
              [t (p/insert ds zero)]
              [t' (p/insert ds' (->One t))]))))

(c/make-type ScheduledBinomialHeap [digits schedule] {:empty? (p/empty? digits)}
  p/Insert
  (insert [_ x] 
    (let [ds (ins-tree (->Node x cs/empty-stack) digits)]
      (->ScheduledBinomialHeap ds (exec (exec (p/conj schedule ds))))))
  p/Heap
  (merge [_ h2]
    (->ScheduledBinomialHeap (normalize (mrg digits (:digits h2))) cs/empty-stack))
  (find-min [_]
    (let [n (first (remove-min-tree digits))]
      (when n (:elem n))))
  (delete-min [_]
    (let [[n ds' :as t] (remove-min-tree digits)]
      (when t
        (let [ds'' (mrg (u/stack->stream (u/map-stack ->One (p/reverse (:trees n)))) ds')]
          (->ScheduledBinomialHeap (normalize ds'') cs/empty-stack)))))
  p/Seq
  (->seq [_] (mapcat p/->seq (concat (p/->seq digits)
                                     (p/->seq schedule)))))

(def empty-heap (->ScheduledBinomialHeap s/empty-stream cs/empty-stack))

(defn ->heap
  [xs]
  (reduce p/insert empty-heap xs))


;;

(p/empty? empty-heap)


(p/find-min (->heap [1 2 -1 -2 1 1 -2]))


(p/find-min (p/merge (->heap [1 2 5 -1])
                     (->heap (shuffle (range -100 100 22)))))



