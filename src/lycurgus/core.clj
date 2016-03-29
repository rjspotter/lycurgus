(ns lycurgus.core
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer :all]
            [clojure.core.contracts :refer :all]
            [clojure.core.contracts.constraints :refer :all]
            [clojure.math.numeric-tower :as math])
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defmacro predict-to [generator application]
  `(do
     (def res#
       (clojure.test.check/quick-check
                            200
                            (clojure.test.check.properties/for-all
                             ~generator
                             (clojure.core/not=
                              :assert-caught
                              (try
                                ~application
                                (catch java.lang.AssertionError e# :assert-caught))))))
     (clojure.test/is
      (clojure.core/= true
                  (if (:result res#) true res#)))))

(defmacro predict-not [generator application]
  `(do
     (def res#
       (clojure.test.check/quick-check
                            200
                            (clojure.test.check.properties/for-all
                             ~generator
                             (clojure.core/not=
                              :assert-caught
                              (try
                                ~application
                                (catch java.lang.AssertionError e# :assert-caught))))))
     (clojure.test/is
      (clojure.core/= true
                  (if (:result res#) res# true)))))

(with-test
  (defconstrainedfn sqr
    [n] [number? => number? (>= % 0) (= (math/abs n) (math/sqrt %))]
    (* n n))
  
  (predict-to [v (gen/one-of [gen/int])] (sqr v))
  (predict-not [v (gen/one-of [gen/keyword
                           gen/boolean
                           (gen/vector gen/int)
                           (gen/list gen/int)])]
           (sqr v)))




(defrecord City [grain citizens land])

(defrecord Spend [feed seed buy])
