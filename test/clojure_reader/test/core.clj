(ns clojure-reader.test.core
  (:require [clojure-reader.core :as cr])
  (:use [clojure.test]))

(deftest reading-integers
  (is 23 (cr/read-string "23"))
  (is -23 (cr/read-string "-23"))
  (is +23 (cr/read-string "+23"))
  
  (is 23N (cr/read-string "23N"))
  (is -23N (cr/read-string "-23N"))
  (is +23N (cr/read-string "+23N"))

  (is 0123 (cr/read-string "0123"))
  (is 0xff (cr/read-string "0xff")))

(deftest reading-floats
  (is 2.3 (cr/read-string "2.3"))
  (is -2.3 (cr/read-string "-2.3"))
  (is +2.3 (cr/read-string "+2.3"))

  (is 2.3M (cr/read-string "2.3M"))
  (is -2.3M (cr/read-string "-2.3M"))
  (is +2.3M (cr/read-string "+2.3M")))

(deftest reading-rations
  (is 2/5 (cr/read-string "2/5"))
  (is -2/5 (cr/read-string "-2/5")))

(deftest reading-symbols
  (is :foo (cr/read-string ":foo"))
  (is :clojure-reader.test.core/foo (cr/read-string "::foo"))
  (is 'clojure.core/map (cr/read-string "clojure.core/map")))

(deftest reading-regexs
  (is #"foo" (cr/read-string "#\"foo\"")))

(deftest reading-lists
  (is '() (cr/read-string "()"))
  (is '(1) (cr/read-string "(1)"))
  (is '(1 2 3) (cr/read-string "(1 2 3)"))
  (is '(1 (2 3)) (cr/read-string "(1 (2 3))")))

(deftest reading-vectors
  (is [] (cr/read-string "[]"))
  (is [1] (cr/read-string "[1]"))
  (is [1 2 3] (cr/read-string "[1 2 3]"))
  (is [1 [2 3]] (cr/read-string "[1 [2 3]]")))

(deftest reading-maps
  (is {} (cr/read-string "{}"))
  (is {1 2} (cr/read-string "{1 2}"))
  (is {1 {2 3}} (cr/read-string "{1 {2 3}}"))
  (is {1 {2 3} 4 5} (cr/read-string "{1 {2 3} 4 5}")))

(deftest reading-sets
  (is #{} (cr/read-string "#{}"))
  (is #{1} (cr/read-string "#{1}"))
  (is #{1 2} (cr/read-string "#{1 2}"))
  (is #{1 2 #{3 4}} (cr/read-string "#{1 2 #{3 4}}")))

(deftest comma-is-whitespace
  (is [1 2] (cr/read-string "[1, 2]"))
  (is {1 2} (cr/read-string "{1, 2}"))
  (is 1 (cr/read-string ",1")))

(deftest reading-metadata
  (is {:foo true} (meta (cr/read-string "^:foo (1 2 3)")))
  (is {:a 1} (meta (cr/read-string "^{:a 1} (1 2 3)"))))

(deftest reading-strings
  (let [simple-string "\"abcd\""
        octal-string "\"abc\\177\""
        unicode-string "\"abc\\0104\""
        escaped-string "\"\\n\\b\\f\\r\""]
    (is (clojure.core/read-string simple-string)
        (cr/read-string simple-string))
    (is (clojure.core/read-string octal-string)
        (cr/read-string octal-string))
    (is (clojure.core/read-string unicode-string)
        (cr/read-string unicode-string))
    (is (clojure.core/read-string escaped-string)
        (cr/read-string escaped-string))))

(deftest read-characters
  (is \space (cr/read-string "\\space"))
  (is \c     (cr/read-string "\\c"))
  (is \o377  (cr/read-string "\\o377"))
  (is \u0104 (cr/read-string "\\u0104")))

(deftest test-syntax-quote
  (doseq [form ["`1" "`map" "`asdfasdv" "`(1 2 (3 4))" "`(a b c ~z)" "`(a b c ~@z)"]]
    (is (= (clojure.core/read-string form)) (cr/read-string form))))