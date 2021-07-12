(ns okasaki.lazy-pairing-heap
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]))

(defprotocol InternalLazyPairingHeap
  (link [a b]))

(c/make-type LazyPairingHeap [elem heaps m] {:empty-name empty-heap}
  InternalLazyPairingHeap
  (link [h1 h2]
    (if (p/empty? heaps)
      (->LazyPairingHeap elem h2 m)
      (->LazyPairingHeap elem empty-heap (delay (p/merge (p/merge h2 heaps) @m)))))
  p/Heap
  (merge [h1 h2]
    (if (p/empty? h2)
      h1
      (if (p/leq? elem (:elem h2))
        (link h1 h2)
        (link h2 h1))))
  (find-min [_] elem)
  (delete-min [_] (p/merge heaps @m))
  p/Insert
  (insert [h v]
    (p/merge (->LazyPairingHeap v empty-heap (delay empty-heap)) h))
  p/Seq
  (->seq [_] (conj (concat (p/->seq heaps) (p/->seq @m)) elem)))

(extend-type EmptyLazyPairingHeap
  p/Heap
  (merge [_ h2] h2)
  (find-min [_] nil)
  (delete-min [h] h)
  p/Insert
  (insert [h v] (p/merge (->LazyPairingHeap v empty-heap (delay empty-heap)) h)))

(defn ->heap
  [xs]
  (reduce p/insert empty-heap xs))

(->heap [4 3 4 -1 2 3])

(p/->structure (->heap (shuffle (range 5))))

(p/->seq (p/delete-min (p/delete-min (p/delete-min (p/merge (->heap (shuffle (range 10)))
                                                            (->heap (shuffle (range 20 30))))))))
