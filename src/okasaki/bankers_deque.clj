(ns okasaki.bankers-deque
  (:require [okasaki.common :as c]
            [okasaki.stream :as s]
            [okasaki.protocols :as p]))

(defprotocol InternalBankersDequeProto
  (check [d]))

(declare empty-deque)

(c/make-type BankersDeque [c lenf front lenr rear] {:empty? (zero? (+ lenf lenr))}
  InternalBankersDequeProto
  (check [d]
    (cond
      (> lenf (inc (* c lenr))) (let [i (quot (+ lenf lenr) 2)
                                      j (- (+ lenf lenr) i)
                                      f' (p/take front i)
                                      r' (p/++ rear (p/reverse (p/drop front i)))]
                                  (->BankersDeque c i f' j r'))
      (> lenr (inc (* c lenf))) (let [j (quot (+ lenf lenr) 2)
                                      i (- (+ lenf lenr) j)
                                      r' (p/take rear j)
                                      f' (p/++ front (p/reverse (p/drop rear j)))]
                                  (->BankersDeque c i f' j r'))
      :else d))
  p/Stack
  (conj [_ x] (check (->BankersDeque c (inc lenf) (p/insert front x) lenr rear)))
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
      :else (check (->BankersDeque c (dec lenf) (:cons @front) lenr rear))))
  p/Queue
  (snoc [d x] (check (->BankersDeque c lenf front (inc lenr) (p/insert rear x))))
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
      :else (check (->BankersDeque c lenf front (dec lenr) (:cons @rear)))))

  p/Seq
  (->seq [q]
    (when (p/head q)
      (conj (p/->seq (p/tail q)) (p/head q)))))

(defn empty-deque
  [c]
  (->BankersDeque c 0 s/empty-stream 0 s/empty-stream))

(defn ->deque
  ([xs]
   (->deque xs 2))
  ([xs c]
   (reduce p/snoc (empty-deque c) xs)))

(p/->structure (->deque (range 10) 5))

