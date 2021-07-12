(ns okasaki.custom-stack-test
  (:require [okasaki.custom-stack :as sut]
            [okasaki.protocols :as p]
            [clojure.test :as t]))

(t/deftest empty-stack
  (t/is (= sut/empty-stack (sut/->EmptyCustomStack)))
  (t/is (p/empty? sut/empty-stack))
  (t/is (not (p/tail sut/empty-stack)))
  (t/is (not (p/head sut/empty-stack))))

(t/deftest inserting
  (t/is (= '(1) (p/->seq (p/conj sut/empty-stack 1))))
  (t/is (= '(2 1) (p/->seq (-> sut/empty-stack
                               (p/conj 1)
                               (p/conj 2)))))
  (t/is (not (p/empty? (p/conj sut/empty-stack 1)))))

(t/deftest access
  (t/is (= 1 (p/head (p/conj sut/empty-stack 1))))
  (t/is (= '(:second :first) (p/->seq (p/tail (-> (p/conj sut/empty-stack :first)
                                                  (p/conj :second)
                                                  (p/conj :third)))))))

(t/deftest conversion
  (t/is (= '(1 2 3) (p/->seq (sut/->stack [1 2 3])))))

(t/deftest structure
  (t/is (= {:head 1
            :tail {:head 2
                   :tail {:head 3
                          :tail nil}}}
           (p/->structure (sut/->stack [1 2 3])))))

(t/deftest rev
  (t/is (= '(3 2 1) (p/->seq (p/reverse (sut/->stack [1 2 3]))))))
