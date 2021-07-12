(ns okasaki.red-black-set
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(declare balance)

(c/make-type RedBlackSet [color left elem right] {:empty-name empty-set}
  p/Set
  (member? [_ v]
    (cond
      (p/lt? v elem) (p/member? left v)
      (p/lt? elem v) (p/member? right v)
      :else true))
  p/Insert
  (insert [s v]
    (letfn [(ins [{:keys [color left elem right] :as s}]
              (if (p/empty? s)
                (p/insert s v)
                (cond
                  (p/lt? v elem) (balance color (ins left) elem right)
                  (p/lt? elem v) (balance color left elem (ins right))
                  :else s)))]
      (let [{:keys [left elem right]} (ins s)]
        (->RedBlackSet :black left elem right))))
  p/Seq
  (->seq [_] (conj (concat (p/->seq left) (p/->seq right)) elem)))

(extend-type EmptyRedBlackSet
  p/Set
  (member? [_ _] false)
  p/Insert
  (insert [_ v]
    (->RedBlackSet :red empty-set v empty-set)))

(defn balance
  [c {lc :color ll :left le :elem lr :right :as l}
   e {rc :color rl :left re :elem rr :right :as r}]
  (let [{llc :color lll :left lle :elem llr :right} ll
        {lrc :color lrl :left lre :elem lrr :right} lr
        {rlc :color rll :left rle :elem rlr :right} rl
        {rrc :color rrl :left rre :elem rrr :right} rr]
    (condp = [:black :red :red]
      ;; '(B (R (R a x b) y c) z d)
      [c lc llc] (->RedBlackSet :red (->RedBlackSet :black lll lle llr)
                                le (->RedBlackSet :black lr e r))
      ;; '(B (R a x (R b y c)) z d) 
      [c lc lrc] (->RedBlackSet :red (->RedBlackSet :black ll le lrl)
                                lre (->RedBlackSet :black lrr e r))
      ;; '(B a x (R (R b y c) z d))
      [c rc rlc] (->RedBlackSet :red (->RedBlackSet :black l e rll)
                                rle (->RedBlackSet :black rlr re rr))
      ;; '(B a x (R b y (R c z d)))
      [c rc rrc] (->RedBlackSet :red (->RedBlackSet :black l e rl)
                                re (->RedBlackSet :black rrl rre rrr))    
      (->RedBlackSet c l e r))))

(defn ->set
  [xs]
  (reduce p/insert empty-set xs))

