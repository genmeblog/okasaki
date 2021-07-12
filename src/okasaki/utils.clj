(ns okasaki.utils
  (:require [okasaki.protocols :as p]
            [okasaki.stream :as s]
            [okasaki.custom-stack :as cs]))

(defn map-stack
  [f xs]
  (if (p/empty? xs)
    xs
    (p/conj (map-stack f (p/tail xs)) (f (p/head xs)))))

(defn stack->stream
  [xs]
  (if (p/empty? xs)
    s/empty-stream
    (p/insert (stack->stream (p/tail xs)) (p/head xs))))

(defn stream->stack
  [xs]
  (if (p/empty? xs)
    cs/empty-stack
    (p/conj (stream->stack (:cons @xs)) (:elem @xs))))
