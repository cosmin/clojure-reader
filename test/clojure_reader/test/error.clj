(ns clojure-reader.test.error
  (:require [clojure-reader.core :as cr])
  (:use [clojure.test]))

(deftest test-read-eof
  (is (thrown-with-msg? RuntimeException #"EOF while reading character"
        (cr/read-string "\\")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading character"
        (cr/read-string "~")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading character"
        (cr/read-string "#")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading string"
        (cr/read-string "\"abc")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading string"
        (cr/read-string "\"abc\\")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading regex"
        (cr/read-string "#\"abc")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading"
        (cr/read-string "(1 2 3")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading"
        (cr/read-string "@")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading"
        (cr/read-string "[1 2 3")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading"
        (cr/read-string "{1 2 3")))
  (is (thrown-with-msg? RuntimeException #"EOF while reading"
        (cr/read-string "#{1 2 3"))))

(deftest test-read-record-errors
  ;; in this particular case Clojure reports a different error because it doesn't catch EOF
  (is (thrown-with-msg? RuntimeException #"EOF while reading constructor form"
        (cr/read-string "#java.util.ArrayList")))
    (is (thrown-with-msg? RuntimeException #"Unreadable constructor form starting with \"#java.util.ArrayList\("
          (cr/read-string "#java.util.ArrayList(")))
    ;; the Clojure source indicates the record reader might skip whitespace in the future
    (is (thrown-with-msg? RuntimeException #"Unreadable constructor form starting with \"#java.util.ArrayList \""
        (cr/read-string "#java.util.ArrayList "))))

(deftest test-unmatched-delimiter-read
  (is (thrown-with-msg? RuntimeException #"Unmatched delimiter: \)"
        (cr/read-string ")")))
  (is (thrown-with-msg? RuntimeException #"Unmatched delimiter: \]"
        (cr/read-string "]")))
  (is (thrown-with-msg? RuntimeException #"Unmatched delimiter: \}"
        (cr/read-string "}"))))
