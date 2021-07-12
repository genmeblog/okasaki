(ns okasaki.bootstrapped-queue
  (:require [okasaki.common :as c]
            [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]))

(defprotocol InternalBootstrappedQueueProto
  (checkq [q])
  (checkf [q]))

(c/make-type BootstrappedQueue [lenfm f m lenr r] {:empty-name empty-queue}
  InternalBootstrappedQueueProto
  (checkq [q]
    (if (<= lenr lenfm)
      (checkf q)
      (checkf (->BootstrappedQueue (+ lenfm lenr) f (p/snoc m (delay (p/reverse r))) 0 cs/empty-stack))))
  (checkf [q]
    (if (p/empty? f)
      (if (p/empty? m)
        m
        (->BootstrappedQueue lenfm @(p/head m) (p/tail m) lenr r))
      q))
  p/Queue
  (snoc [_ v]
    (checkq (->BootstrappedQueue lenfm f m (inc lenr) (p/conj r v))))
  p/Stack
  (head [_] (p/head f))
  (tail [_] (checkq (->BootstrappedQueue (dec lenfm) (p/tail f) m lenr r)))
  p/Seq
  (->seq [q]
    (conj (p/->seq (p/tail q)) (p/head q))))

(extend-type EmptyBootstrappedQueue
  p/Queue
  (snoc [_ v] (->BootstrappedQueue 1 (p/conj cs/empty-stack v) empty-queue 0 cs/empty-stack))
  p/Stack
  (head [_] nil)
  (tail [_] nil))

(defn ->queue
  [xs]
  (reduce p/snoc empty-queue xs))

(p/snoc empty-queue 1)

(p/tail (p/tail (->queue [11 22 33 -11 22])))
