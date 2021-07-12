(ns okasaki.stream-test
  (:require [okasaki.stream :as sut]
            [okasaki.protocols :as p]
            [clojure.test :as t]))

(t/deftest empty-stream
  (t/is (= sut/empty-stream (sut/->EmptyStream)))
  (t/is (p/empty? sut/empty-stream))
  (t/is (= sut/empty-stream (p/drop sut/empty-stream 1)))
  (t/is (= sut/empty-stream (p/take sut/empty-stream 1))))

(t/deftest inserting
  (t/is (= '(1) (p/->seq (p/take (p/insert sut/empty-stream 1) 1))))
  (t/is (not (p/empty? (p/insert sut/empty-stream 1)))))

(t/deftest access
  (t/is (= '(1 2 3) (p/->seq (p/take (sut/->stream [1 2 3 4 5 6]) 3))))
  (t/is (= '(4 5 6) (p/->seq (p/drop (sut/->stream [1 2 3 4 5 6]) 3)))))

(t/deftest conversion
  (t/is (= '(1 2 3) (p/->seq (sut/->stream [1 2 3])))))

(t/deftest concat-streams
  (t/is (= '(1 2 3 4 234 911 9234) (p/->seq (p/++ (p/++ (sut/->stream [1 2 3 4])
                                                        (p/insert sut/empty-stream 234))
                                                  (p/++ (p/insert sut/empty-stream 911)
                                                        (p/insert sut/empty-stream 9234)))))))

(t/deftest structure
  (t/is (= (list :lazy {:elem 1
                        :cons (list :lazy {:elem 2
                                           :cons (list :lazy {:elem 3
                                                              :cons nil})})})
           (p/->structure (sut/->stream [1 2 3])))))

(t/deftest rev
  (t/is (= '(3 2 1) (p/->seq (p/reverse (sut/->stream [1 2 3]))))))
