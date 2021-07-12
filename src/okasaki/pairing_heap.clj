(ns okasaki.pairing-heap
  (:require [okasaki.protocols :as p]
            [okasaki.common :as c]
            [okasaki.custom-stack :as cs]))

(declare merge-pairs)

(c/make-type PairingHeap [elem heaps] {:empty-name empty-heap}
  p/Heap
  (merge [h1 h2]
    (if (p/empty? h2)
      h1
      (if (p/leq? elem (:elem h2))
        (->PairingHeap elem (p/conj heaps h2))
        (->PairingHeap (:elem h2) (p/conj (:heaps h2) h1)))))
  (find-min [_] elem)
  (delete-min [_] (merge-pairs heaps))
  p/Insert
  (insert [h v]
    (p/merge h (->PairingHeap v cs/empty-stack)))
  p/Seq
  (->seq [_] (conj (mapcat p/->seq (p/->seq heaps)) elem)))

(defn- merge-pairs
  [heaps]
  (if (p/empty? heaps)
    empty-heap
    (let [{:keys [head tail]} heaps]
      (if (p/empty? tail)
        head
        (p/merge (p/merge head (:head tail)) (merge-pairs (:tail tail)))))))

(extend-type EmptyPairingHeap
  p/Heap
  (merge [_ h2] h2)
  (find-min [_] nil)
  (delete-min [h] h)
  p/Insert
  (insert [_ v] (->PairingHeap v cs/empty-stack)))

(defn ->heap
  [xs]
  (reduce p/insert empty-heap xs))

(p/->structure (->heap (shuffle (range 5))))

(p/->seq (p/delete-min (p/delete-min (p/delete-min (p/merge (->heap (shuffle (range 10)))
                                                            (->heap (shuffle (range 20 30))))))))
