(ns okasaki.batched-deque
  (:require [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]
            [okasaki.common :as c]))

(defprotocol InternalBatchedDequeProto
  (checkf [q])
  (checkr [q]))

(declare empty-deque)

(c/make-type BatchedDeque [front rear] {:empty? (and (p/empty? front)
                                                     (p/empty? rear))}
  InternalBatchedDequeProto
  (checkf [q] (if (p/empty? front)
                (BatchedDeque. (p/reverse rear) cs/empty-stack)
                q))
  (checkr [q] (if (p/empty? rear)
                (BatchedDeque. cs/empty-stack (p/reverse front))
                q))
  p/Queue
  (snoc [_ x] (checkf (BatchedDeque. front (p/conj rear x))))
  p/Stack
  (conj [_ x] (checkr (BatchedDeque. (p/conj front x) rear)))
  (head [_] (if (p/empty? front) nil (p/head front)))
  (tail [_] (if (p/empty? front) empty-deque (checkf (BatchedDeque. (p/tail front) rear))))
  p/Deque
  (last [_] (if (p/empty? rear) nil (p/head rear)))
  (init [_] (if (p/empty? rear) empty-deque (checkr (BatchedDeque. front (p/tail rear)))))
  p/Seq
  (->seq [_] (concat (p/->seq front) (p/->seq (p/reverse rear)))))

(def empty-deque (->BatchedDeque cs/empty-stack cs/empty-stack))

(defn ->queue
  [xs]
  (reduce p/snoc empty-deque xs))

(p/->structure (-> (p/snoc empty-deque 1)
                   (p/snoc 22)
                   (p/snoc 33)
                   (p/snoc 444)
                   (p/conj 999)
                   (p/conj 1000)))



