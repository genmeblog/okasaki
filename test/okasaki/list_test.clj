(ns okasaki.list-test
  (:require [okasaki.list :as sut]
            [okasaki.protocols :as p]
            [clojure.test :as t]))

(t/deftest empty-list
  (t/is (= sut/empty-list (list)))
  (t/is (= clojure.lang.PersistentList$EmptyList (class sut/empty-list)))
  (t/is (p/empty? sut/empty-list))
  (t/is (not (p/tail sut/empty-list)))
  (t/is (not (p/head sut/empty-list))))

(t/deftest inserting
  (t/is (= '(1) (p/conj sut/empty-list 1)))
  (t/is (= '(2 1) (-> sut/empty-list
                      (p/conj 1)
                      (p/conj 2))))
  (t/is (not (p/empty? (p/conj sut/empty-list 1)))))

(t/deftest access
  (t/is (= 1 (p/head (p/conj sut/empty-list 1))))
  (t/is (= '(:second :first) (p/tail (-> (p/conj sut/empty-list :first)
                                         (p/conj :second)
                                         (p/conj :third))))))

(t/deftest structure
  (t/is (= {:head 1
            :tail {:head 2
                   :tail {:head 3
                          :tail nil}}}
           (p/->structure '(1 2 3)))))

(t/deftest rev
  (t/is (= '(3 2 1) (p/reverse '(1 2 3)))))
