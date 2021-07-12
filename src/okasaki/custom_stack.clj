(ns okasaki.custom-stack
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(c/make-type CustomStack [head tail] {:empty-name empty-stack}
  p/Stack
  (conj [s v] (->CustomStack v s))
  (head [_] head)
  (tail [_] tail)
  p/Reverse
  (reverse [s] (c/reverse-helper s empty-stack))
  p/Stream
  (++ [xs ys] (p/conj (p/++ (p/tail xs) ys) (p/head xs)))
  p/Seq
  (->seq [_] (conj (p/->seq tail) head)))

(extend-type EmptyCustomStack
  p/Stack
  (conj [s v] (->CustomStack v s))
  (head [_] nil)
  (tail [_] nil)
  p/Stream
  (++ [_ ys] ys)
  p/Reverse
  (reverse [t] t))

(defn ->stack 
  [xs]
  (reduce p/conj empty-stack (reverse xs)))

(->stack [1 2 3 4])
