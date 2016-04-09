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

(def double-except-nan (gen/such-that #(not (Double/isNaN %)) gen/double))

(with-test
  (defconstrainedfn sqr
    [n] [number? (not (Double/isNaN n))=> number? (>= % 0) (= (math/abs n) (math/sqrt %))]
    (* n n))
  
  (predict-to [v (gen/one-of [gen/int
                              double-except-nan])] (sqr v))
  
  (predict-not [v (gen/one-of [gen/keyword
                           gen/boolean
                           (gen/vector gen/int)
                           (gen/list gen/int)])]
           (sqr v)))

; A citizen needs 20 bushels a year.
; An acre takes one bushel to seed.
(defrecord City [grain citizens land])

; spends are in bushels
(defrecord Spend [feed seed warfare])

; Cost to conquer in bushels
; Amount gained by pillage and burning occupied land
(defrecord Army [pillage conquer])

(def *evil-root* (atom (list (->City 2800 100 1000))))

(defn now [] (first *evil-root*))

(with-test
  (defconstrainedfn feed
    "Feeding the citizens: each Citizen eats 20 bushels of wheat per year"
    [city spend]
    [(every? integer?  [(:feed spend) (:grain city) (:citizens city)])
     (every? #(>= % 0) [(:feed spend) (:grain city) (:citizens city)])
     (<= (:feed spend) (:grain city))
     =>
     (not (nil? %))
     (<= (:grain %)    (:grain city))
     (<= (:citizens %) (:citizens city))
     (=  (:land %)     (:land city))
     (every? integer? [(:grain %) (:citizens %) (:land %)])
     (every? #(>= % 0) [(:grain %) (:citizens % (:land %))])
     ]
    (let [f (:feed spend)
          g (:grain city)
          c (:citizens city)
          s (int (/ f 20))]
      (->City (- g f) (min c s) (:land city))
      ))
  )

(with-test
  (defconstrainedfn seed
    "Sow the seeds for next years crop"
    [city spend]
    [(every? integer?  [(:seed spend) (:grain city) (:citizens city)])
     (every? #(>= % 0) [(:seed spend) (:grain city) (:citizens city)])
     (<= (:seed spend) (:grain city))
     =>
     (not (nil? %))
     (<= (:grain %)    (:grain city))
     (=  (:citizens %) (:citizens city))
     (=  (:land %)     (:land city))
     (every? integer? [(:grain %) (:citizens %) (:land %)])
     (every? #(>= % 0) [(:grain %) (:citizens % (:land %))])
     ]
    (let [s (:seed spend)
          g (:grain city)]
      (->City (- g s) (:citizens city) (:land city))
    )))
