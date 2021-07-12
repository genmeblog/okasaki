(ns okasaki.sparse-by-weight
  (:require [okasaki.common :as c]
            [okasaki.protocols :as p]
            [okasaki.custom-stack :as cs]))

(defprotocol InternalSparseByWeightProto
  (carry [ws w])
  (borrow [ws w]))

(c/make-type SparseByWeight [lst] {:empty? (p/empty? lst)}
  p/Stack
  (conj [_ v] (->SparseByWeight (p/conj lst v)))
  (head [_] (p/head lst))
  (tail [_] (->SparseByWeight (p/tail lst)))
  InternalSparseByWeightProto
  (carry [ws w]
    (if (or (p/empty? ws)
            (< w (p/head ws)))
      (p/conj ws w)
      (carry (p/tail ws) (* w 2))))
  (borrow [ws w]
    (if (= w (p/head ws))
      (p/tail ws)
      (p/conj (borrow ws (* w 2)) w)))
  p/Numbers
  (inc [ws] (carry ws 1))
  (dec [ws] (borrow ws 1))
  p/Additive
  (add [m n]
    (cond
      (p/empty? m) n
      (p/empty? n) m
      (< (p/head m) (p/head n)) (p/conj (p/add (p/tail m) n) (p/head m))
      (< (p/head n) (p/head m)) (p/conj (p/add m (p/tail n)) (p/head n))
      :else (carry (p/add (p/tail m) (p/tail n)) (* 2 (p/head m)))))
  p/Seq
  (->seq [_] (p/->seq lst)))

(def zero (->SparseByWeight cs/empty-stack))

(defn ->number
  ([c] (->number c 1))
  ([c curr]
   (cond
     (zero? c) zero
     (zero? (bit-and c 0x1)) (recur (unsigned-bit-shift-right c 1) (* curr 2))
     :else (p/conj (->number (unsigned-bit-shift-right c 1) (* curr 2)) curr))))

(defn ->int
  [c]
  (reduce + (p/->seq c)))

(->int (reduce p/add (map ->number [1 22 3 99 4 128])))
