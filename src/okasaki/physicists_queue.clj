(ns okasaki.physicists-queue
  (:require [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]
            [okasaki.common :as c]))

(defprotocol InternalPhysicistsQueueProto
  (check [q])
  (checkw [q]))

(c/make-type PhysicistsQueue [w lenf front lenr rear] {:empty? (zero? lenf)}
  InternalPhysicistsQueueProto
  (checkw [q]
    (if (p/empty? w)
      (->PhysicistsQueue @front lenf front lenr rear)
      q))
  (check [q]
    (if (<= lenr lenf)
      (checkw q)
      (->PhysicistsQueue @front (+ lenf lenr) (delay (p/++ @front (p/reverse rear))) 0 cs/empty-stack)))
  p/Queue
  (snoc [_ x]
    (check (->PhysicistsQueue w lenf front (inc lenr) (p/conj rear x))))
  p/Stack
  (p/head [_]
    (if (p/empty? w)
      nil
      (p/head w)))
  (p/tail [_]
    (if (p/empty? w)
      nil
      (check (->PhysicistsQueue w (dec lenf) (delay (p/tail @front)) lenr rear))))
  p/Seq
  (->seq [_] (concat (p/->seq @front) (p/->seq (p/reverse rear)))))

(def empty-queue (->PhysicistsQueue cs/empty-stack 0 (delay cs/empty-stack) 0 cs/empty-stack))

(defn ->queue
  [xs]
  (reduce p/snoc empty-queue xs))

(p/empty? empty-queue)

(p/empty? (p/snoc empty-queue 11))


(p/->seq (p/tail (->queue [1 2 3 -11 2 3])))
