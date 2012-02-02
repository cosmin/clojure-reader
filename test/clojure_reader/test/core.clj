(ns clojure-reader.test.core
  (:require [clojure-reader.core :as cr])
  (:use [clojure.test]))

(deftest read-integers
  (is 23 (cr/read-string "23"))
  (is -23 (cr/read-string "-23"))
  (is +23 (cr/read-string "+23"))
  
  (is 23N (cr/read-string "23N"))
  (is -23N (cr/read-string "-23N"))
  (is +23N (cr/read-string "+23N"))

  (is 0123 (cr/read-string "0123"))
  (is 0xff (cr/read-string "0xff")))

(deftest read-floats
  (is 2.3 (cr/read-string "2.3"))
  (is -2.3 (cr/read-string "-2.3"))
  (is +2.3 (cr/read-string "+2.3"))

  (is 2.3M (cr/read-string "2.3M"))
  (is -2.3M (cr/read-string "-2.3M"))
  (is +2.3M (cr/read-string "+2.3M")))

(deftest read-rations
  (is 2/5 (cr/read-string "2/5"))
  (is -2/5 (cr/read-string "-2/5")))

(deftest read-symbols
  (is :foo (cr/read-string ":foo"))
  (is :clojure-reader.test.core/foo (cr/read-string "::foo"))
  (is 'clojure.core/map (cr/read-string "clojure.core/map")))
