(ns okasaki.dense
  (:require [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]
            [okasaki.common :as c]))

(c/make-type Dense [lst] {:empty? (p/empty? lst)}
  p/Stack
  (conj [_ v] (->Dense (p/conj lst v)))
  (head [_] (p/head lst))
  (tail [_] (->Dense (p/tail lst)))
  p/Numbers
  (inc [s]
    (cond
      (p/empty? s) (p/conj s :one)
      (= :zero (p/head s)) (p/conj (p/tail s) :one)
      :else (p/conj (p/inc (p/tail s)) :zero)))
  (dec [s]
    (cond
      (p/empty? s) (throw (Exception. "Negative values not allowed."))
      (= :one (p/head s)) (p/conj (p/tail s) :zero)
      :else (p/conj (p/dec (p/tail s)) :one)))
  p/Additive
  (add [n1 n2]
    (cond
      (p/empty? n1) n2
      (p/empty? n2) n1
      (= :zero (p/head n2)) (p/conj (p/add (p/tail n1) (p/tail n2)) (p/head n1))
      (= :zero (p/head n1)) (p/conj (p/add (p/tail n1) (p/tail n2)) (p/head n2))
      :else (p/conj (p/inc (p/add (p/tail n1) (p/tail n2))) :zero)))
  p/Seq
  (->seq [_] (p/->seq lst)))

(def zero (->Dense cs/empty-stack))

(defn ->number
  [c]
  (if
      (zero? c) zero
      (p/conj (->number (unsigned-bit-shift-right c 1))
              (if (zero? (bit-and c 0x1)) :zero :one))))

(defn ->int
  [c]
  (if (p/empty? c)
    0
    (let [n (bit-shift-left (->int (p/tail c)) 1)]
      (if (= :one (p/head c))
        (bit-or n 0x1)
        n))))

(->int (p/add (p/inc (->number 31))
              (p/dec (->number 32))))
