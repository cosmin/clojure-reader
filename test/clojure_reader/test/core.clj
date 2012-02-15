(ns clojure-reader.test.core
  (:require [clojure-reader.core :as cr])
  (:use [clojure.test]))

(deftest test-reading-integers
  (is (= 23 (cr/read-string "23")))
  (is (= -23 (cr/read-string "-23")))
  (is (= +23 (cr/read-string "+23")))
  
  (is (= 23N (cr/read-string "23N")))
  (is (= -23N (cr/read-string "-23N")))
  (is (= +23N (cr/read-string "+23N")))

  (is (= 0123 (cr/read-string "0123")))
  (is (= 0xff (cr/read-string "0xff"))))

(deftest test-reading-floats
  (is (= 2.3 (cr/read-string "2.3")))
  (is (= -2.3 (cr/read-string "-2.3")))
  (is (= +2.3 (cr/read-string "+2.3")))

  (is (= 2.3M (cr/read-string "2.3M")))
  (is (= -2.3M (cr/read-string "-2.3M")))
  (is (= +2.3M (cr/read-string "+2.3M"))))

(deftest test-reading-rations
  (is (= 2/5 (cr/read-string "2/5")))
  (is (= -2/5 (cr/read-string "-2/5"))))

(deftest test-reading-symbols
  (is (= :foo (cr/read-string ":foo")))
  (is (= :user/foo (cr/read-string "::foo")))
  (is (= 'clojure.core/map (cr/read-string "clojure.core/map"))))

(defn regex-equal [r1 r2]
  (and (= (class r1) (class r2))
       (= (str r1) (str r2))))

(deftest test-reading-regexs
  (is (regex-equal #"foo" (cr/read-string "#\"foo\""))))

(deftest test=reading-lists
  (is (= '() (cr/read-string "()")))
  (is (= '(1) (cr/read-string "(1)")))
  (is (= '(1 2 3) (cr/read-string "(1 2 3)")))
  (is (= '(1 (2 3)) (cr/read-string "(1 (2 3))"))))

(deftest test-reading-vectors
  (is (= [] (cr/read-string "[]")))
  (is (= [1] (cr/read-string "[1]")))
  (is (= [1 2 3] (cr/read-string "[1 2 3]")))
  (is (= [1 [2 3]] (cr/read-string "[1 [2 3]]"))))

(deftest test-reading-maps
  (is {} (cr/read-string "{}"))
  (is {1 2} (cr/read-string "{1 2}"))
  (is {1 {2 3}} (cr/read-string "{1 {2 3}}"))
  (is {1 {2 3} 4 5} (cr/read-string "{1 {2 3} 4 5}")))

(deftest test-reading-sets
  (is (= #{} (cr/read-string "#{}")))
  (is (= #{1} (cr/read-string "#{1}")))
  (is (= #{1 2} (cr/read-string "#{1 2}")))
  (is (= #{1 2 #{3 4}} (cr/read-string "#{1 2 #{3 4}}"))))

(deftest test-comma-is-whitespace
  (is (= [1 2] (cr/read-string "[1, 2]")))
  (is (= {1 2} (cr/read-string "{1, 2}")))
  (is (= 1 (cr/read-string ",1"))))

(deftest test-reading-metadata
  (is (= {:foo true} (meta (cr/read-string "^:foo (1 2 3)"))))
  (is (= {:a 1} (meta (cr/read-string "^{:a 1} (1 2 3)")))))

(deftest test-reading-strings
  (let [simple-string "\"abcd\""
        octal-string "\"abc\\177\""
        unicode-string "\"abc\\0104\""
        escaped-string "\"\\n\\b\\f\\r\""]
    (is (= (clojure.core/read-string simple-string)
           (cr/read-string simple-string)))
    (is (= (clojure.core/read-string octal-string)
           (cr/read-string octal-string)))
    (is (= (clojure.core/read-string unicode-string)
           (cr/read-string unicode-string)))
    (is (= (clojure.core/read-string escaped-string)
           (cr/read-string escaped-string)))))

(deftest test-read-characters
  (is (= \space (cr/read-string "\\space")))
  (is (= \c     (cr/read-string "\\c")))
  (is (= \o377  (cr/read-string "\\o377")))
  (is (= \u0104 (cr/read-string "\\u0104"))))

(deftest test-syntax-quote
  (doseq [form ["`1" "`map" "`asdfasdv" "`(1 2 (3 4))" "`(a b c ~z)" "`(a b c ~@z)"]]
    (is (= (clojure.core/read-string form)) (cr/read-string form))))

(deftest test-tagged-literals
  (let [s "#inst \"2010-11-12T13:14:15.666-06:00\""]
    (is (= (clojure.core/read-string s) (cr/read-string s)))
    (binding [*data-readers* {}]
      (= java.util.Date (class (cr/read-string s))))
    (binding [*data-readers* {'inst clojure.instant/read-instant-calendar}]
      (= java.util.GregorianCalendar (class (cr/read-string s))))))

(defrecord Person [name age])

(deftest test-records
  (is (= #clojure_reader.test.core.Person["test" 18]
         (cr/read-string "#clojure_reader.test.core.Person[\"test\" 18]")))
  (is (= #clojure_reader.test.core.Person{:age 18}
         (cr/read-string "#clojure_reader.test.core.Person{:age 18}"))))

(deftest test-eval-reader
  (is (= 3 (cr/read-string "#=(+ 2 1)")))
  (is (= "" (cr/read-string "#=(java.lang.String.)")))
  (is (= 12 (cr/read-string "#=(java.lang.Long/valueOf 12)"))))

(deftest test-fn-reader
  (is (= (cr/read-string "#(+ 1 2)") (clojure.core/read-string "#(+ 1 2)"))))

(deftest test-arg-reader
  (is (= 3 (apply (eval (cr/read-string "#(+ %1 %2)")) [1 2]))))

(deftest test-wrapping-readers
  (let [quote-form "'foo"
        var-form "#'foo"
        deref-form "@foo"]
    (is (= (clojure.core/read-string quote-form) (cr/read-string quote-form)))
    (is (= (clojure.core/read-string var-form) (cr/read-string var-form)))
    (is (= (clojure.core/read-string deref-form) (cr/read-string deref-form)))))
