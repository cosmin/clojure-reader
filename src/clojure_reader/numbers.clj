(ns clojure-reader.numbers
  (:use clojure-reader.util)
  (:import [java.io PushbackReader]
           [java.math BigDecimal BigInteger]
           [java.util.regex Pattern Matcher]
           [clojure.lang Numbers BigInt]))

(defn- number-and-radix [^Matcher m]
  (cond-let [n]
            (.group m 3) [n 10]
            (.group m 4) [n 16]
            (.group m 5) [n 8]
            (.group m 7) [n (Integer/parseInt (.group m 6))]))

(defn- read-big-integer [negate? n radix]
  (let [bn (BigInteger. n radix)]
    (if negate?
      (.negate bn)
      bn)))

(def int-pattern (Pattern/compile "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?"))
(defn- parse-integer [s]
  (let [m (.matcher int-pattern s)]
    (if (.matches m)
      (if (not-nil? (.group m 2))
        (if (not-nil? (.group m 8))
          BigInt/ZERO
          (Numbers/num 0))
        (let [negate? (= "-" (.group m 1))
              [n radix] (number-and-radix m)]
          (if (not-nil? n)
            (let [bn (read-big-integer negate? n radix)]
              (if (not-nil? (.group m 8))
                (BigInt/fromBigInteger bn)
                (if (< (.bitLength bn) 64)
                  (Numbers/num (.longValue bn))
                  (BigInt/fromBigInteger bn))))))))))

(def ratio-pattern (Pattern/compile "([-+]?[0-9]+)/([0-9]+)"))
(defn- parse-ratio [s]
  (let [m (.matcher ratio-pattern s)
        reduce-big-int (fn [x] (Numbers/reduceBigInt (BigInt/fromBigInteger (BigInteger. x))))]
    (if (.matches m)
      (Numbers/divide (reduce-big-int (.group m 1))
                      (reduce-big-int (.group m 2))))))

(def float-pattern (Pattern/compile "([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?"))
(defn- parse-float [s]
  (let [m (.matcher float-pattern s)]
    (if (.matches m)
      (if (nil? (.group m 4))
        (Double/parseDouble s)
        (BigDecimal. (.group m 1))))))

(defn match-number [s]
  (cond
     (.contains s "/") (parse-ratio s)
     (.contains s ".") (parse-float s)
     :else (parse-integer s)))
