(ns okasaki.hood-melville-queue
  (:require [okasaki.common :as c]
            [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]))

(defprotocol InternalRotationStateProto
  (exec [rs])
  (invalidate [rs]))

(c/make-type Idle [] {})
(c/make-type Reversing [ok f f' r r'] {})
(c/make-type Appending [ok f' r'])
(c/make-type Done [f])

(def idle (->Idle))

(extend-protocol InternalRotationStateProto
  Idle
  (exec [rs] rs)
  (invalidate [rs] rs)
  Done
  (exec [rs] rs)
  (invalidate [rs] rs)
  Reversing
  (exec [{:keys [ok f f' r r']}]
    (if (p/empty? f)
      (->Appending ok f' (p/conj r' (p/head r)))
      (->Reversing (inc ok) (p/tail f) (p/conj f' (p/head f)) (p/tail r) (p/conj r' (p/head r)))))
  (invalidate [{:keys [ok f f' r r']}]
    (->Reversing (dec ok) f f' r r'))
  Appending
  (exec [{:keys [ok f' r']}]
    (if (zero? ok)
      (->Done r')
      (->Appending (dec ok) (p/tail f') (p/conj r' (p/head f')))))
  (invalidate [{:keys [ok f' r']}]
    (if (zero? ok)
      (->Done (p/tail r'))
      (->Appending (dec ok) f' r'))))

(defprotocol InternalHoodMelvilleQueueProto
  (exec2 [q])
  (check [q]))

(c/make-type HoodMelvilleQueue [lenf front state lenr rear] {:empty? (zero? lenf)}
  InternalHoodMelvilleQueueProto
  (exec2 [_]
    (let [newstate (exec (exec state))]
      (if (instance? Done newstate)
        (->HoodMelvilleQueue lenf (:f newstate) idle lenr rear)
        (->HoodMelvilleQueue lenf front newstate lenr rear))))
  (check [q]
    (if (<= lenr lenf)
      (exec2 q)
      (let [newstate (->Reversing 0 front cs/empty-stack rear cs/empty-stack)]
        (exec2 (->HoodMelvilleQueue (+ lenr lenf) front newstate 0 cs/empty-stack)))))
  p/Queue
  (snoc [_ x]
    (check (->HoodMelvilleQueue lenf front state (inc lenr) (p/conj rear x))))
  p/Stack
  (head [_]
    (if (p/empty? front)
      nil
      (p/head front)))
  (tail [_]
    (if (p/empty? front)
      nil
      (check (->HoodMelvilleQueue (dec lenf) (p/tail front) (invalidate state) lenr rear))))
  p/Seq
  (->seq [q] (conj (p/->seq (p/tail q)) (p/head q))))

(def empty-queue (->HoodMelvilleQueue 0 cs/empty-stack idle 0 cs/empty-stack))

(defn ->queue
  [xs]
  (reduce p/snoc empty-queue xs))


(->queue [1 2 3 4 5 6 7 9])
