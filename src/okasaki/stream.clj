(ns okasaki.stream
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(c/make-type Stream [elem cons] {:make-empty? true :make-lazy? true}
  p/Seq
  (->seq [d] (conj (p/->seq (:cons d)) (:elem d))))

(extend-type EmptyStream
  p/Insert
  (insert [s v] ($ (->Stream v s)))
  p/Stream
  (++ [_ s2] s2)
  (take [s _] s)
  (drop [s _] s)
  p/Reverse
  (reverse [s] s))

(extend-type LazyStream
  p/Stream
  (++ [d s2]
    ($ (->Stream (:elem @d) (p/++ (:cons @d) s2))))
  (take [d n]
    (if (zero? n)
      empty-stream
      ($ (->Stream (:elem @d) (p/take (:cons @d) (dec n))))))
  (drop [d n]
    (if (zero? n)
      d
      (recur (:cons @d) (dec n))))
  p/Reverse
  (reverse [d]
    (letfn [(rev [a r]
              (if (p/empty? a)
                r
                (recur (:cons @a) (p/insert r (:elem @a)))))]
      (rev d empty-stream)))
  p/Insert
  (insert [s v] ($ (->Stream v s))))

(defn ->stream
  [xs]
  (reduce p/insert empty-stream (reverse xs)))

(p/->structure (->stream [1 2]))


