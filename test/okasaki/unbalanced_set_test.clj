(ns okasaki.unbalanced-set-test
  (:require [okasaki.unbalanced-set :as sut]
            [okasaki.protocols :as p]
            [clojure.test :as t]))

(t/deftest empty-set
  (t/is (= sut/empty-set (sut/->EmptyUnbalancedSet)))
  (t/is (p/empty? sut/empty-set))
  (t/is (not (p/member? sut/empty-set 1))))

(t/deftest inserting
  (t/is (= '(1) (p/->seq (p/insert sut/empty-set 1))))
  (t/is (= '(1 2) (p/->seq (-> sut/empty-set
                               (p/insert 1)
                               (p/insert 2)))))
  (t/is (not (p/empty? (p/insert sut/empty-set 1)))))

(t/deftest member
  (t/is (p/member? (p/insert sut/empty-set 1) 1))
  (t/is (let [s (sut/->set (shuffle (range 100)))]
          (every? (partial p/member? s) (range 100))))
  (t/is (not (p/member? (sut/->set (shuffle (range 100))) 150))))

(t/deftest conversion
  (t/is (= #{1 2 3} (set (p/->seq (sut/->set [1 3 2 1 3 2]))))))

(t/deftest structure
  (t/is (= {:left {:left nil
                   :elem 1
                   :right nil}
            :elem 2
            :right {:left nil
                    :elem 3
                    :right nil}}
           (p/->structure (sut/->set [2 2 3 3 1 1])))))
