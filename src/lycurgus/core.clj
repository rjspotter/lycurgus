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

  (predict-to [v (gen/one-of [gen/int])] (sqr v))

  (predict-not [v (gen/one-of [gen/keyword
                           gen/boolean
                           (gen/vector gen/int)
                           (gen/list gen/int)])]
           (sqr v)))

; A citizen needs 20 bushels a year.
; An acre takes one bushel to seed.
(defrecord City [grain citizens land])

; spends are in bushels
(defrecord Spend [feed seed conquor pillage])

; Cost to conquer in bushels
; Amount gained by pillage and burning occupied land
(defrecord Army [pillage conquer])

; Turn : all the data needed to go from one state to the next
(defrecord Turn [city army spend])

(def ^:dynamic *evil-root* (ref (list (->City 2800 100 1000))))

(defn now [] (first *evil-root*))

(def valid-turn (gen/let
                    [grain gen/pos-int citizens gen/pos-int land gen/pos-int exchange gen/pos-int]
                  (->Turn (->City grain citizens land)
                          (->Army exchange exchange)
                          (->Spend (int (/ grain 20)) (min grain land) (int (/ grain (+ 1 exchange))) 0))))

(def invalid-turn(gen/let
                    [grain gen/pos-int citizens gen/pos-int land gen/pos-int exchange gen/pos-int multiple gen/pos-int]
                  (->Turn (->City grain citizens land)
                          (->Army exchange exchange)
                          (->Spend (int (* grain multiple)) (max grain land) (int (* grain exchange)) 0))))

(with-test
  (defconstrainedfn feed
    "Feeding the citizens: each Citizen eats 20 bushels of wheat per year | overspending is lost"
    [{city :city spend :spend :as turn}]
    [(every? integer?  [(:feed spend) (:grain city) (:citizens city)])
     (every? #(>= % 0) [(:feed spend) (:grain city) (:citizens city)])
     (<= (:feed spend) (:grain city))
     =>
     (not (nil? %))
     (<= (:grain (:city %))    (:grain city))
     (<= (:citizens (:city %)) (:citizens city))
     (=  (:land (:city %))     (:land city))
     (every? integer? [(:grain (:city %)) (:citizens (:city %)) (:land (:city %))])
     (every? #(>= % 0) [(:grain (:city %)) (:citizens (:city %)) (:land (:city %))])
     ]
    (let [f (:feed spend)
          g (:grain city)
          c (:citizens city)
          s (int (/ f 20))]
      (assoc turn :city (assoc city :grain (- g f) :citizens (min c s)))))
  (predict-to  [t valid-turn]   (feed t))
  (predict-not [t invalid-turn] (feed t)))

(with-test
  (defconstrainedfn sow
    "Sow the seeds for next year's crop | overspending is lost"
    [{city :city spend :spend :as turn}]
    [(every? integer?  [(:seed spend) (:grain city) (:citizens city)])
     (every? #(>= % 0) [(:seed spend) (:grain city) (:citizens city)])
     (<= (:seed spend) (:grain city))
     =>
     (not (nil? %))
     (<= (:grain (:city %))    (:grain city))
     (<= (:citizens (:city %)) (:citizens city))
     (=  (:land (:city %))     (:land city))
     (every? integer? [(:grain (:city %)) (:citizens (:city %)) (:land (:city %))])
     (every? #(>= % 0) [(:grain (:city %)) (:citizens (:city %)) (:land (:city %))])
     ]
    (let [s (:seed spend)
          g (:grain city)]
      (assoc turn :city (->City (- g s) (:citizens city) (:land city)))
      ))
  (predict-to  [t valid-turn]   (feed t))
  (predict-not [t invalid-turn] (feed t)))

(with-test
  (defconstrainedfn conquor
    "Conquer : feeding the army extra so they can effectively kill their neighbors and take their land"
    [{city :city spend :spend army :army :as turn}]
    [(every? integer?  [(:conquor spend) (:grain city) (:citizens city)])
     (every? #(>= % 0) [(:conquor spend) (:grain city) (:citizens city)])
     (<= (:conquor spend) (:grain city))
     =>
     (not (nil? %))
     (<= (:grain %)    (:grain city))
     (=  (:citizens %) (:citizens city))
     (>= (:land %)     (:land city))
     (every? integer? [(:grain %) (:citizens %) (:land %)])
     (every? #(>= % 0) [(:grain %) (:citizens % (:land %))])
     ]
    (let [spent (:conquor spend)
          grain (:grain city)
          cost  (:conquor army)
          ]
      (assoc turn :city (->City (- grain spent) (:citizens city) (+ (:land city) (int (Math/floor (/ spent cost))))))
      ))
  (predict-to  [t valid-turn]   (feed t))
  (predict-not [t invalid-turn] (feed t)))


(with-test
  (defconstrainedfn pillage
    "Pillage : Strip a piece of land of it's resources leaving an unusable husk"
    [{city :city spend :spend army :army :as turn}]
    [(every? integer?  [(:pillage spend) (:grain city) (:citizens city)])
     (every? #(>= % 0) [(:pillage spend) (:grain city) (:citizens city)])
     (<= (:pillage spend) (:grain city))
     =>
     (not (nil? %))
     (>= (:grain (:city %))    (:grain city))
     (=  (:citizens (:city %)) (:citizens city))
     (<= (:land (:city %))     (:land city))
     (every? integer? [(:grain (:city %)) (:citizens (:city %)) (:land (:city %))])
     (every? #(>= % 0) [(:grain (:city %)) (:citizens (:city %)) (:land (:city %))])
     ]
    (let [spent (:pillage spend)
          yield  (:pillage army)
          ]
      (assoc turn :city (->City (+ (:grain city) (int (Math/floor (* spent yield)))) (:citizens city) (- (:land city) spent)))
      ))
  (predict-to  [t valid-turn]   (feed t))
  (predict-not [t invalid-turn] (feed t)))

(with-test
  (defconstrainedfn reap
    "Reap : Gather all the grown wheat you sowed earlier"
    [{city :city spend :spend army :army :as turn}]
    [(every? integer?  [(:pillage spend) (:grain city) (:citizens city)])
     (every? #(>= % 0) [(:pillage spend) (:grain city) (:citizens city)])
     (<= (:pillage spend) (:grain city))
     =>
     (not (nil? %))
     (>= (:grain (:city %))    (:grain city))
     (=  (:citizens (:city %)) (:citizens city))
     (<= (:land (:city %))     (:land city))
     (every? integer? [(:grain (:city %)) (:citizens (:city %)) (:land (:city %))])
     (every? #(>= % 0) [(:grain (:city %)) (:citizens (:city %)) (:land (:city %))])
     ]
    (let [spent (:pillage spend)
          yield  (:pillage army)
          ]
      (assoc turn :city (->City (+ (:grain city) (int (Math/floor (* spent yield)))) (:citizens city) (- (:land city) spent)))
      ))
  (predict-to  [t valid-turn]   (feed t))
  (predict-not [t invalid-turn] (feed t)))

