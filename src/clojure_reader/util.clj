(ns clojure-reader.util
  (:import [java.io PushbackReader]
           [clojure.lang LineNumberingPushbackReader]))

(defn not-nil? [val]
  (not (nil? val)))

(defmacro cond-let [bindings & clauses]
  (let [binding (first bindings)]
    (when-let [[test expr & more] clauses]
      (if (= test :else)
        expr
        `(if-let [~binding ~test]
           ~expr
           (cond-let ~bindings ~@more))))))

(defn throw-runtime
  "Throws the supplied exception wrapped in a runtime exception,
  unless it is already a runtime exception"
  ([e]
     (if (instance? RuntimeException e)
       (throw e)
        (throw (RuntimeException. e)))))

(defn whitespace?
  "Determine if the character is considered whitespace in Clojure"
  [ch]
  (if (= -1 ch)
    false
    (or (Character/isWhitespace ch) (= \, (char ch)))))

(defn plus-or-minus? [^Character ch]
  (let [chr (char ch)]
    (or (= chr \+) (= chr \-))))

(defn get-line-number [^PushbackReader reader]
  (if (instance? LineNumberingPushbackReader reader)
    (.getLineNumber reader)
    -1))

(defn slice
  ([string start-idx]
     (slice string start-idx (.length string)))
  ([string start-idx end-idx]
     (let [len (.length string)
           positivize #(if (< %1 0) (+ len %1) %1)
           s (positivize start-idx)
           e (positivize end-idx)]
       (println string "[" s ":" e "]")
       (if (< s e)
         (.substring string s e)
         ""))))