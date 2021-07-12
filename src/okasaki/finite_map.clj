(ns okasaki.finite-map
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(c/make-type FiniteMap [left k v right] {:empty-name empty-map}
  p/Lookup
  (lookup [_ kk]
    (cond
      (p/lt? kk k) (p/lookup left kk)
      (p/lt? k kk) (p/lookup right kk)
      :else v))
  p/FiniteMap
  (bind [fm kk vv]
    (cond
      (p/lt? kk k) (->FiniteMap (p/bind left kk vv) k v right)
      (p/lt? k kk) (->FiniteMap left k v (p/bind right kk vv))
      :else fm))
  p/Seq
  (->seq [_] (conj (concat (p/->seq left) (p/->seq right)) [k v])))

(extend-type EmptyFiniteMap
  p/FiniteMap
  (bind [_ k v] (->FiniteMap empty-map k v empty-map))
  (lookup [m k] (throw (Exception. "Key not found")))  )

(defn ->finite-map [m]
  (reduce (fn [fm [k v]]
            (p/bind fm k v)) empty-map m))

