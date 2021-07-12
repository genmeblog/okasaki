(ns okasaki.finite-map-test
  (:require [okasaki.finite-map :as sut]
            [okasaki.protocols :as p]
            [clojure.test :as t]))

(t/deftest empty-map
  (t/is (= sut/empty-map (sut/->EmptyFiniteMap)))
  (t/is (p/empty? sut/empty-map))
  (t/is (= :exception (try (p/lookup sut/empty-map 1)
                           (catch Exception _ :exception)))))

(t/deftest bind
  (t/is (= '([:a 1]) (p/->seq (p/bind sut/empty-map :a 1))))
  (t/is (= '([:a 1] [:b 2]) (p/->seq (-> sut/empty-map
                                         (p/bind :a 1)
                                         (p/bind :b 2)))))
  (t/is (not (p/empty? (p/bind sut/empty-map :a 1)))))

(t/deftest lookup
  (t/is (= 1 (p/lookup (p/bind sut/empty-map :a 1) :a)))
  (t/is (= :exception (try (p/lookup (p/bind sut/empty-map :a 1) :b)
                           (catch Exception _ :exception))))
  (t/is (let [m (reduce #(p/bind %1 (str %2) %2) sut/empty-map (range 100))]
          (every? (partial p/lookup m) (map str (range 100))))))

(t/deftest conversion
  (t/is (= {:a 1 :b 2 :c 43 :d 44} (into {} (p/->seq (sut/->finite-map {:a 1 :b 2 :c 43 :d 44}))))))

(t/deftest structure
  (t/is (= {:left nil,
            :k :a,
            :v 1,
            :right {:left nil,
                    :k :b,
                    :v 2,
                    :right {:left nil,
                            :k :c,
                            :v 43,
                            :right {:left nil
                                    :k :d
                                    :v 44
                                    :right nil}}}}
           (p/->structure (sut/->finite-map {:a 1 :b 2 :c 43 :d 44})))))
