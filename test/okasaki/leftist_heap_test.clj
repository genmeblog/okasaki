(ns okasaki.leftist-heap-test
  (:require [okasaki.leftist-heap :as sut]
            [okasaki.protocols :as p]
            [clojure.test :as t]))

(t/deftest empty-heap
  (t/is (= sut/empty-heap (sut/->EmptyLeftistHeap)))
  (t/is (p/empty? sut/empty-heap))
  (t/is (not (p/find-min sut/empty-heap))))

(t/deftest inserting
  (t/is (= '(1) (p/->seq (p/insert sut/empty-heap 1))))
  (t/is (= '(1 2) (p/->seq (-> sut/empty-heap
                               (p/insert 1)
                               (p/insert 2)))))
  (t/is (not (p/empty? (p/insert sut/empty-heap 1)))))

(t/deftest minimum-value
  (t/is (= 1 (p/find-min (p/insert sut/empty-heap 1))))
  (t/is (= 0 (p/find-min (sut/->heap (shuffle (range 100))))))
  (t/is (= 1 (p/find-min (p/delete-min (sut/->heap (shuffle (range 100))))))))

(t/deftest conversion
  (t/is (= '(1 1 3 2 2 3) (p/->seq (sut/->heap [1 3 2 1 3 2])))))

(t/deftest structure
  (t/is (= {:rank 1
            :left {:rank 1
                   :left {:rank 2
                          :left {:rank 1
                                 :left nil
                                 :elem 2
                                 :right nil}
                          :elem 2
                          :right {:rank 1
                                  :left {:rank 1
                                         :left nil
                                         :elem 3
                                         :right nil}
                                  :elem 3
                                  :right nil}}
                   :elem 1
                   :right nil}
            :elem 1
            :right nil}
           (p/->structure (sut/->heap [2 2 3 3 1 1])))))
