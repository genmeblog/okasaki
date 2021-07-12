(ns okasaki.ch2
  (:require [okasaki.protocols :as p]
            [okasaki.list]
            [okasaki.custom-stack :as cs]
            [okasaki.unbalanced-set :as us])
  (:refer-clojure :exclude [update]))

;; concat

(defn ++
  "Concat two stacks"
  [xs ys]
  (if (p/empty? xs)
    ys
    (p/conj (++ (p/tail xs) ys) (p/head xs))))

(++ (cs/->stack [1 2 3])
    (cs/->stack [5 6 7]))
;; => CustomStack: (1 2 3 5 6 7)

(++ '(1 2 3) '(5 6 7))
;; => (1 2 3 5 6 7)

;; update

(defn update
  "Replace nth element"
  [xs i y]
  (assert (not (neg? i)) "Negative subscript")
  (cond
    (p/empty? xs) (throw (Exception. "Wrong subscript"))
    (zero? i) (p/cons (p/tail xs) y)
    :else (p/cons (update (p/tail xs) (dec i) y) (p/head xs))))

(update (cs/->stack (range 10)) 5 "a")
;; => CustomStack: (0 1 2 3 4 "a" 6 7 8 9)

(update (range 10) 5 "a")
;; => (0 1 2 3 4 "a" 6 7 8 9)

;; Exercise 2.1

(defn suffixes
  [xs]
  (p/cons (if (p/empty? xs)
            cs/empty-stack
            (suffixes (p/tail xs))) xs))

(suffixes (cs/->stack [1 2 3 4]))
;; => CustomStack: (CustomStack: (1 2 3 4) CustomStack: (2 3 4) CustomStack: (3 4) CustomStack: (4) nil)

(suffixes '(1 2 3 4))
;; => CustomStack: ((1 2 3 4) (2 3 4) (3 4) (4) nil)

;; Exercise 2.2, 2.3 and 2.4

nil

;; Exercise 2.5a

(defn complete
  [x d]
  (if (<= d 1)
    (us/->UnbalancedSet nil x nil)
    (let [n (complete x (dec d))]
      (us/->UnbalancedSet n x n))))

(p/->structure (complete 1 4))
;; => [[[[nil 1 nil] 1 [nil 1 nil]] 1 [[nil 1 nil] 1 [nil 1 nil]]] 1 [[[nil 1 nil] 1 [nil 1 nil]] 1 [[nil 1 nil] 1 [nil 1 nil]]]]
