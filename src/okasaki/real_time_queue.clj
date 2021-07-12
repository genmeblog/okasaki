(ns okasaki.real-time-queue
  (:require [okasaki.protocols :as p]
            [okasaki.stream :as s]
            [okasaki.custom-stack :as cs]
            [okasaki.common :as c]))

(defprotocol InternalRealTimeQueueProto
  (rotate [q])
  (exec [q]))

(c/make-type RealTimeQueue [front rear susp] {:empty? (p/empty? front)}
  InternalRealTimeQueueProto
  (rotate [_]
    (if (p/empty? front)
      (p/insert susp (p/head rear))
      (p/insert (rotate (->RealTimeQueue (:cons @front)
                                         (p/tail rear)
                                         (p/insert susp (p/head rear))))
                (:elem @front))))
  (exec [_]
    (if-not (p/empty? susp)
      (->RealTimeQueue front rear (:cons @susp))
      (let [f' (rotate (->RealTimeQueue front rear s/empty-stream))]
        (->RealTimeQueue f' cs/empty-stack f'))))
  p/Queue
  (snoc [_ v]
    (exec (->RealTimeQueue front (p/conj rear v) susp)))
  p/Stack
  (head [_]
    (if (p/empty? front)
      nil
      (:elem @front)))
  (tail [_]
    (if (p/empty? front)
      nil
      (exec (->RealTimeQueue (:cons @front) rear susp))))
  p/Seq
  (->seq [_] (concat (p/->seq front)
                     (p/->seq (p/reverse rear)))))

(def empty-queue (->RealTimeQueue s/empty-stream cs/empty-stack s/empty-stream))

(defn ->queue
  [xs]
  (reduce p/snoc empty-queue xs))

(p/head (p/tail (->queue [1 2 3 3 2 3 9 2 1 -1 3 4 2 1 2 3 4])))


(p/->structure (->queue (shuffle (range 10))))
