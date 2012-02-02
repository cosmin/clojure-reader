(ns clojure-reader.core
  (:refer-clojure :exclude [read read-string])
  (:use clojure-reader.util
        [clojure-reader.numbers :only (match-number)]
        [clojure-reader.symbols :only (interpret-token)]) 
  (:import [java.io PushbackReader StringReader]
           [java.util.regex Pattern Matcher]
           [clojure.lang Namespace Compiler Symbol Keyword]
           [clojure.lang LineNumberingPushbackReader LispReader$ReaderException]))


(defn- get-macro [ch]
  (fn [^PushbackReader stream, Character ch] ch))

(defn- macro? [ch]
  false)

(defn- terminating-macro? [ch]
  false)

(defn- read-a-token [^PushbackReader stream, ch]
  (let [sb (StringBuilder.)]
    (.append sb (char ch))
    (loop [ch (.read stream)]
      (if (not (or (= -1 ch) (whitespace? (char ch)) (terminating-macro? ch)))
        (do
          (.append sb (char ch))
          (recur (.read stream)))
        (do
          (.unread stream ch)
          (.toString sb))))))

(defn- read-a-number [^PushbackReader stream, ^Character ch]
  (let [sb (StringBuilder.)]
    (.append sb (char ch))
    (loop [ch (.read stream)]
      (if (not (or (= -1 ch) (whitespace? (char ch)) (macro? ch)))
        (do
          (.append sb (char ch))
          (recur (.read stream)))
        (.unread stream ch)))
    (let [s (.toString sb)
          n (match-number s)]
      (if (nil? n)
        (throw (NumberFormatException. (str "Invalid number: " s)))
        n))))


(defn read
  "Reads the next object from stream, which must be an instance of
  java.io.PushbackReader or some derivee.  stream defaults to the
  current value of *in* ."
  ([]
     (read *in*))
  ([stream]
     (read stream true nil))
  ([stream eof-error? eof-value]
     (read stream eof-error? eof-value false))
  ([stream eof-error? eof-value recursive?]
     (try
       (loop [ch (.read stream)]
         (if (whitespace? (char ch))
           (recur (.read stream))
           (cond
            (= -1 ch) (if eof-error?
                        (throw (RuntimeException. "EOF while reading"))
                        eof-value)
            (Character/isDigit ch) (read-a-number stream ch)
            (macro? ch) ((get-macro ch) stream (char ch))
            (plus-or-minus? ch) (let [ch2 (.read stream)]
                                  (if (Character/isDigit ch2)
                                    (do
                                      (.unread stream ch2)
                                      (read-a-number stream ch))
                                    (do
                                      (.unread stream ch2)
                                      (interpret-token (read-a-token stream ch)))))
            :else (interpret-token (read-a-token stream ch)))))
       (catch Exception e
         (if (or recursive? (not (instance? LineNumberingPushbackReader stream)))
           (throw-runtime e)
           (throw (LispReader$ReaderException. (.getLineNumber stream) e))
           )))))

(defn read-string
  "Reads one object from the supplied string"
  ([s]
     (let [stream (PushbackReader. (StringReader. s))]
       (read stream true nil false))))
