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

; A citizen needs 20 bushels a year.
; An acre takes one bushel to seed.
(defrecord City [grain citizens land])

(defrecord Spend [feed seed buy]) ; spends are in bushels

(defrecord RealEstate [sell buy])

(def *evil-root* (atom (list (->City 2800 100 1000))))

(defn now [] (first *evil-root*))

(with-test
  (defconstrainedfn feed
    "Feeding the citizens: each Citizen eats 20 bushels of wheat per year"
    [city spend]
    [(integer? (:feed spend))
     (integer? (:grain city))
     (integer? (:citizens city))
     =>
     (not (nil? %))
     (<= (:grain %)    (:grain city))
     (<= (:citizens %) (:citizens city))
     (=  (:land %)     (:land city))
     (integer? (:grain %))
     (integer? (:citizens %))
     ]
    (let [f (* 20 (:feed spend)) g (:grain city) c (:citizens city)]
      (->City (- g f) (min c f) (:land city))
    )))
