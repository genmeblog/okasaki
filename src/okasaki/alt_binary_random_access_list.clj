(ns okasaki.alt-binary-random-access-list
  (:require [okasaki.common :as c]
            [okasaki.protocols :as p]))

(defprotocol InternalBinaryRandomAccessListProto
  (cns [l x])
  (uncns [l])
  (fupdate [l i f]))

(declare ->One)

(c/make-type Nil [] {:empty? true}
  InternalBinaryRandomAccessListProto
  (cns [_ x] (->One x (->Nil)))
  (uncns [_] (throw (Exception. "Empty list")))
  (fupdate [_ _ _] (throw (Exception. "Index out of bounds.")))
  p/Lookup
  (lookup [_ _] (throw (Exception. "Index out of bounds."))))

(c/make-type Zero [ps] {}
  InternalBinaryRandomAccessListProto
  (cns [_ x] (->One x ps))
  (uncns [_] (let [[[x y] ps'] (uncns ps)]
               [x (->One y ps')]))
  (fupdate [_ i f]
    (letfn [(f' [[x y]]
              (if (even? i) [(f x) y] [x (f y)]))]
      (->Zero (fupdate ps (quot i 2) f'))))
  p/Lookup
  (lookup [_ i]
    (let [[x y] (p/lookup ps (quot i 2))]
      (if (even? i) x y))))

(c/make-type One [x ps] {}
  InternalBinaryRandomAccessListProto
  (cns [_ v] (->Zero (cns ps [v x])))
  (uncns [_] (if (p/empty? ps)
               [x (->Nil)]
               [x (->Zero ps)]))
  (fupdate [_ i f]
    (if (zero? i)
      (->One (f x) ps)
      (cns (fupdate (->Zero ps) (dec i) f) x)))
  p/Lookup
  (lookup [_ i]
    (if (zero? i)
      x
      (p/lookup (->Zero ps) (dec i)))))

(c/make-type AltBinaryRandomAccessList [lst] {:empty? (p/empty? lst)}
  p/Stack
  (conj [_ x] (->AltBinaryRandomAccessList (cns lst x)))
  (head [_] (first (uncns lst)))
  (tail [_] (->AltBinaryRandomAccessList (second (uncns lst))))
  p/Lookup
  (lookup [_ i] (p/lookup lst i))
  p/RandomAccess
  (update [_ i v] (->AltBinaryRandomAccessList (fupdate lst i (constantly v))))
  p/Seq
  (->seq [l]
    (if (p/empty? l)
      nil
      (conj (p/->seq (p/tail l)) (p/head l)))))

(def empty-list (->AltBinaryRandomAccessList (->Nil)))

(defn ->list
  [xs]
  (reduce p/conj empty-list (reverse xs)))

(p/tail (p/conj (->list [1 2 3 4 5]) 22))

(p/lookup (p/tail (->list [1 2 3 4 5])) 0)

(p/update (p/tail (->list [1 2 3 4 5])) 2 -999)
