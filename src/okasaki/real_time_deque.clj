(ns okasaki.real-time-deque
  (:require [okasaki.common :as c]
            [okasaki.stream :as s]
            [okasaki.protocols :as p]))

(defprotocol InternalRealTimeDequeProto
  (check [d]))

(declare empty-deque)

(defn- rotate-rev
  [c s r a]
  (if (p/empty? s)
    (p/++ (p/reverse r) a)
    (p/insert (rotate-rev c (:cons @s) (p/drop r c) (p/++ (p/reverse (p/take r c)) a)) (:elem @s))))

(defn- rotate-drop
  [c f j r]
  (if (< j c)
    (rotate-rev c f (p/drop r j) s/empty-stream)
    (p/insert (rotate-drop c (:cons @f) (- j c) (p/drop r c)) (:elem @f))))

(defn- exec1 [s] (if (p/empty? s) s (:cons @s)))
(defn- exec2 [s] (exec1 (exec1 s)))

(c/make-type RealTimeDeque [c lenf front sf lenr rear sr] {:empty? (zero? (+ lenf lenr))}
  InternalRealTimeDequeProto
  (check [d]
    (cond
      (> lenf (inc (* c lenr))) (let [i (quot (+ lenf lenr) 2)
                                      j (- (+ lenf lenr) i)
                                      f' (p/take front i)
                                      r' (rotate-drop c rear i front)]
                                  (->RealTimeDeque c i f' f' j r' r'))
      (> lenr (inc (* c lenf))) (let [j (quot (+ lenf lenr) 2)
                                      i (- (+ lenf lenr) j)
                                      r' (p/take rear j)
                                      f' (rotate-drop c front j rear)]
                                  (->RealTimeDeque c i f' f' j r' r'))
      :else d))
  p/Stack
  (conj [_ x] (check (->RealTimeDeque c (inc lenf) (p/insert front x) (exec1 sf) lenr rear (exec1 sr))))
  (head [_]
    (cond
      (and (p/empty? front)
           (p/empty? rear)) nil
      (p/empty? front) (:elem @rear)
      :else (:elem @front)))
  (tail [_]
    (cond
      (and (p/empty? front)
           (p/empty? rear)) nil
      (p/empty? front) (empty-deque c)
      :else (check (->RealTimeDeque c (dec lenf) (:cons @front) (exec2 sf) lenr rear (exec2 sr)))))
  p/Queue
  (snoc [d x] (check (->RealTimeDeque c lenf front (exec1 sf) (inc lenr) (p/insert rear x) (exec1 sr))))
  p/Deque
  (last [_]
    (cond
      (and (p/empty? front)
           (p/empty? rear)) nil
      (p/empty? rear) (:elem @front)
      :else (:elem @rear)))
  (init [_]
    (cond
      (and (p/empty? front)
           (p/empty? rear)) nil
      (p/empty? rear) (empty-deque c)
      :else (check (->RealTimeDeque c lenf front (exec2 sf) (dec lenr) (:cons @rear) (exec2 sr)))))

  p/Seq
  (->seq [q]
    (when (p/head q)
      (conj (p/->seq (p/tail q)) (p/head q)))))

(defn empty-deque
  [c]
  (assert (#{2 3} c) "c must be 2 or 3")
  (->RealTimeDeque c 0 s/empty-stream s/empty-stream 0 s/empty-stream s/empty-stream))

(defn ->deque
  ([xs]
   (->deque xs 2))
  ([xs c]
   (reduce p/snoc (empty-deque c) xs)))

(p/last (->deque (range 10) 2))


