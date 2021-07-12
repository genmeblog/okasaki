(ns okasaki.list
  (:refer-clojure :exclude [cons empty?])
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c])
  (:import [clojure.lang ISeq]))

(extend-type ISeq
  p/Empty
  (empty? [s] (clojure.core/empty? s))
  p/Stack
  (conj [s v] (clojure.core/conj s v))
  (head [s] (first s))
  (tail [s] (next s))
  p/Seq
  (->seq [s] (seq s))
  p/Reverse
  (reverse [s] (c/reverse-helper s ()))
  p/Structure
  (->structure [s] {:head (p/->structure (first s))
                    :tail (p/->structure (next s))}))



(def empty-list ())

