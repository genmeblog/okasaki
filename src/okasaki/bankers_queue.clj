(ns okasaki.bankers-queue
  (:require [okasaki.protocols :as p]
            [okasaki.stream :as s]
            [okasaki.common :as c]))

(defprotocol InternalBankersQueueProto
  (check [q]))

(c/make-type BankersQueue [lenf front lenr rear] {:empty? (zero? lenf)}
  InternalBankersQueueProto
  (check [q]
    (if (<= lenr lenf)
      q
      (->BankersQueue (+ lenf lenr) (p/++ front (p/reverse rear)) 0 s/empty-stream)))
  p/Queue
  (snoc [_ x]
    (check (->BankersQueue lenf front (inc lenr) (p/insert rear x))))
  p/Stack
  (p/head [_]
    (if (p/empty? front)
      nil
      (:elem @front)))
  (p/tail [_]
    (if (p/empty? front)
      nil
      (check (->BankersQueue (dec lenf) (:cons @front) lenr rear))))
  p/Seq
  (->seq [_] (concat (p/->seq front) (p/->seq (p/reverse rear)))))

(def empty-queue (->BankersQueue 0 s/empty-stream 0 s/empty-stream))

(defn ->queue
  [xs]
  (reduce p/snoc empty-queue xs))

(p/empty? empty-queue)

(p/empty? (p/snoc empty-queue 11))


(p/head (p/tail (->queue [1 2 3 -11 2 3])))
