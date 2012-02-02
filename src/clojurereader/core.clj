(ns clojurereader.core
  (:refer-clojure :exclude [read read-string])
  (:use clojurereader.util
        [clojurereader.numbers :only (match-number)])
  (:import [java.io PushbackReader StringReader]
           [java.util.regex Pattern Matcher]
           [clojure.lang Namespace Compiler Symbol Keyword]
           [clojure.lang LineNumberingPushbackReader LispReader$ReaderException]))


(defn get-macro [ch]
  (fn [^PushbackReader stream, Character ch] ch))

(defn macro? [ch]
  false)

(defn terminating-macro? [ch]
  false)

(defn whitespace?
  "Determine if the character is considered whitespace in Clojure"
  [ch] (or (Character/isWhitespace ch) (= \, ch)))

(defn read-token [^PushbackReader stream, ch]
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

(defn namespace-for
  ([^Symbol sym] (namespace-for *ns* sym))
  ([^Namespace inns, ^Symbol sym]
     (let [ns-sym (Symbol/intern (.ns sym))
           ns (.lookupAlias inns ns-sym )]
       (if (nil? ns)
         (Namespace/find ns-sym)
         ns))))

(def symbol-pattern (Pattern/compile "[:]?([\\D&&[^/]].*/)?([\\D&&[^/]][^/]*)"))
(defn match-symbol [s]
  (let [m (.matcher symbol-pattern s)]
    (if (.matches m)
      (let [gc (.groupCount m)
            ns (.group m 1)
            name (.group m 2)]
        (cond
         (or (and (not-nil? ns) (.endsWith ns ":/"))
             (.endsWith name ":")
             (not= -1 (.indexOf s "::" 1)))
         nil

         (.startsWith s "::")
         (let [ks (Symbol/intern (.substring s 2))
               kns (if (not-nil? (.getNamespace ks))
                     (namespace-for ks)
                     *ns*)]
           (if (not-nil? kns)
             (Keyword/intern (.. kns getName getName) (. ks getName))))

         :else
         (let [keyword? (= \: (.charAt s 0))
               sym (Symbol/intern (.substring s (if keyword? 1 0)))]
           (if keyword?
             (Keyword/intern sym)
             sym)))))))

(defn interpret-token [token]
  (condp = token
    "nil" nil
    "true" true
    "false" false
    "/" /
    "clojure.core//" clojure.core//
    (if-let [matched (match-symbol token)]
      matched
      (throw (RuntimeException. (str "Invalid token: " token))))))

(defn read-a-number [^PushbackReader stream, ^Character ch]
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

(defn- plus-or-minus? [^Character ch]
  (let [chr (char ch)]
    (or (= chr \+) (= chr \-))))

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
                                      (interpret-token (read-token stream ch)))))
            :else (interpret-token (read-token stream ch)))))
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
