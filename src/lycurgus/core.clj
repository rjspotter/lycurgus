(ns lycurgus.core
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer :all]
            [clojure.core.contracts :refer :all]
            [clojure.math.numeric-tower :as math])
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(with-test
  (def sqr
    "Generic squaring function"
    (with-constraints
      (fn [x] (* x x))
      (contract nummy "ensures numbers"
                [n] [number? => number?])
      (contract positive "result positive"
                [n] [number? => (>= % 0)])
      (contract sqrt-back "sqrt of result equals input"
                [n] [number? => (= (math/abs n) (math/sqrt %))])
      ))
  (is (= true (let [res (tc/quick-check 200 (prop/for-all
                                             [v (gen/one-of [gen/int])]
                                             (not= :assert-caught
                                                   (try
                                                     (sqr v)
                                                     (catch AssertionError e :assert-caught)))))]
                (if (:result res) true res))))

  (is (= true (let [res (tc/quick-check 200 (prop/for-all
                                             [v (gen/one-of [gen/keyword
                                                            gen/boolean
                                                            (gen/vector gen/int)
                                                            (gen/list gen/int)])]
                                             (= :assert-caught
                                                   (try
                                                     (sqr v)
                                                     (catch AssertionError e :assert-caught)))))]
                (if (:result res) true res))))
  )

(defrecord City [grain citizens land])

(defrecord Spend [feed seed buy])
