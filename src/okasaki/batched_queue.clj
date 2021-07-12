(ns okasaki.batched-queue
  (:require [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]
            [okasaki.common :as c]))

(defprotocol InternalBatchedQueueProto
  (checkf [q]))

(declare empty-queue)

(c/make-type BatchedQueue [front rear] {:empty? (p/empty? front)}
  InternalBatchedQueueProto
  (checkf [q] (if (p/empty? front)
                (BatchedQueue. (p/reverse rear) cs/empty-stack)
                q))
  p/Queue
  (snoc [_ x]
    (checkf (BatchedQueue. front (p/conj rear x))))
  p/Stack
  (head [_]
    (if (p/empty? front)
      nil
      (p/head front)))
  (tail [_]
    (if (p/empty? front)
      empty-queue
      (checkf (BatchedQueue. (p/tail front) rear))))
  p/Seq
  (->seq [_] (concat (p/->seq front) (p/->seq (p/reverse rear)))))

(def empty-queue (->BatchedQueue cs/empty-stack cs/empty-stack))

(defn ->queue
  [xs]
  (reduce p/snoc empty-queue xs))

(p/empty? empty-queue)

(->queue [1 2 3 4])

(p/->structure (-> (p/snoc empty-queue 1)
                   (p/snoc 22)
                   (p/snoc 33)
                   (p/snoc 444)))
