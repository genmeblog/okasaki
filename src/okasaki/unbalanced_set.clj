(ns okasaki.unbalanced-set
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(c/make-type UnbalancedSet [left elem right] {:empty-name empty-set}
  p/Set
  (member? [_ v]
    (cond
      (p/lt? v elem) (p/member? left v)
      (p/lt? elem v) (p/member? right v)
      :else true))
  p/Insert
  (insert [s v]
    (cond
      (p/lt? v elem) (->UnbalancedSet (p/insert left v) elem right)
      (p/lt? elem v) (->UnbalancedSet left elem (p/insert right v))
      :else s))
  p/Seq
  (->seq [_] (conj (concat (p/->seq left) (p/->seq right)) elem)))

(extend-type EmptyUnbalancedSet
  p/Set
  (member? [_ _] false)
  p/Insert
  (insert [_ v] (->UnbalancedSet empty-set v empty-set)))

(defn ->set
  [xs]
  (reduce p/insert empty-set xs))

