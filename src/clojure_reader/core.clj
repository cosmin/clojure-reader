(ns clojure-reader.core
  (:refer-clojure :exclude [read read-string])
  (:use clojure-reader.util
        [clojure-reader.numbers :only (match-number)]
        [clojure-reader.symbols :only (interpret-token)]) 
  (:import [java.io PushbackReader StringReader]
           [java.util ArrayList]
           [java.util.regex Pattern Matcher]
           [clojure.lang IFn RT Symbol]
           [clojure.lang PersistentList PersistentHashSet LazilyPersistentVector]
           [clojure.lang LineNumberingPushbackReader LispReader$ReaderException]))

(declare read)
(declare read-string)

(def macros (make-array IFn 256))
(def dispatch-macros (make-array IFn 256))

(defn- get-macro [ch]
  (if (< ch (alength macros))
    (aget macros ch)))

(defn- macro? [ch]
  (and (< ch (alength macros))
       (not-nil? (aget macros ch))
       ))

(defn- terminating-macro? [ch]
  (and (not (or (= \# (char ch))
                (= \\ (char ch))
                ))
       (macro? ch)))

(defn get-line-number [^PushbackReader reader]
  (if (instance? LineNumberingPushbackReader reader)
    (.getLineNumber reader)
    -1))

(defn read-delimited-list [^Character delim, ^PushbackReader reader, recursive?]
  (let [first-line (get-line-number reader)
        a (ArrayList.)]
    (loop [ch (.read reader)]
      (if (= -1 ch)
        (if (< first-line 0)
          (throw (RuntimeException. "EOF while reading"))
          (throw (RuntimeException. (str "EOF while reading, starting at line " first-line))))
        (if (whitespace? ch)
          (recur (.read reader))
          (if (= (char ch) delim)
            a
            (do
              (if (macro? ch)
                (let [mret ((get-macro ch) reader (char ch))]
                  (if (not= reader mret)
                    (do (.add a mret)
                        (recur (.read reader))
                        )
                    (recur (.read reader))))
                (do
                  (.unread reader ch)
                  (let [o (read reader true nil recursive?)]
                    (if (not= reader o)
                      (.add a o)))
                  (recur (.read reader)))))))))))

(defmacro defreader [name & body]
  `(defn ~name ~[^PushbackReader 'reader, ^Character 'ch]
     ~@body
     )
  )

(defreader string-reader)

(defreader comment-reader
  (loop [ch (.read reader)]
    (if (not (or (= -1 ch)
                 (= \newline (char ch))
                 (= \return (char ch))
                 ))
      (recur (.read reader))
      reader)))

(defreader ctor-reader)
(defreader meta-reader)
(defreader syntax-quote-reader)
(defreader unquote-reader)

(defreader list-reader
  (let [line (get-line-number reader)
        lst (read-delimited-list \) reader true)]
    (if (.isEmpty lst)
      (PersistentList/EMPTY)
      (let [s (PersistentList/create lst)]
        (if (not= -1 line)
          (.withMeta s {:line line})
          s
          )))))

(defreader character-reader)
(defreader arg-reader)
(defreader dispatch-reader
  (loop [ch (.read reader)]
    (if (= -1 ch)
      (throw (RuntimeException. "EOF while reading character"))
      (let [dfn (aget dispatch-macros ch)
            chr (char ch)]
        (if (nil? dfn)
          (do
            (.unread reader chr)
            (let [result (ctor-reader reader chr)]
              (if (nil? result)
                (throw (RuntimeException. (str "No dispatch macro for:" chr)))
                result)))
          (dfn reader (char ch)))))))

(defreader regex-reader
  (let [sb (StringBuilder.)]
    (loop [ch (.read reader)]
      (if (= -1 ch)
        (throw (RuntimeException. "EOF while reading regex"))
        (let [chr (char ch)]
          (if (not= \" chr)
            (do
              (.append sb chr)
              (if (= \\ chr)
                (let [ch2 (.read reader)]
                  (if (= -1 ch2)
                    (throw (RuntimeException. "EOF while reading regex"))
                    (.append sb (char ch2)))))
              (recur (.read reader)))
            (Pattern/compile (.toString sb))))))))

(defreader fn-reader)
(defreader eval-reader)

(defreader discard-reader
  (read reader true nil true)
  reader)

(defreader unmatched-delimiter-reader
  (throw (RuntimeException. (str "Unmatched delimiter: " ch))))

(defreader unreadable-reader
  (throw (RuntimeException. "Unreadable form")))

(defreader set-reader
  (PersistentHashSet/createWithCheck (read-delimited-list \} reader true)))

(defreader vector-reader
  (LazilyPersistentVector/create (read-delimited-list \] reader true)))

(defreader map-reader
  (let [read (read-delimited-list \} reader true)]
    (if (not= 0 (bit-and (.size read) 1))
      (throw (RuntimeException. "Map literal must contain an even number of forms"))
      (RT/map (.toArray read)))))

(defn make-wrapping-reader [^Symbol sym]
  (fn [^PushbackReader reader, ^Character ch]
    (RT/list sym (read reader true nil true))))

(do
  (aset macros \" string-reader)
  (aset macros \; comment-reader)
  (aset macros \' (make-wrapping-reader (Symbol/intern "quote")))
  (aset macros \@ (make-wrapping-reader (Symbol/intern "clojure.core" "deref")))
  (aset macros \^ meta-reader)
  (aset macros \` syntax-quote-reader)
  (aset macros \~ unquote-reader)
  (aset macros \( list-reader)
  (aset macros \) unmatched-delimiter-reader)
  (aset macros \[ vector-reader)
  (aset macros \] unmatched-delimiter-reader)
  (aset macros \{ map-reader)
  (aset macros \} unmatched-delimiter-reader)
  (aset macros \\ character-reader)
  (aset macros \% arg-reader)
  (aset macros \# dispatch-reader))

(do
  (aset dispatch-macros \^ meta-reader)
  (aset dispatch-macros \' (make-wrapping-reader (Symbol/intern "var")))
  (aset dispatch-macros \" regex-reader)
  (aset dispatch-macros \( fn-reader)
  (aset dispatch-macros \{ set-reader)
  (aset dispatch-macros \= eval-reader)
  (aset dispatch-macros \! comment-reader)
  (aset dispatch-macros \< unreadable-reader)
  (aset dispatch-macros \_ discard-reader))


(defn- read-a-token [^PushbackReader stream, ch]
  (let [sb (StringBuilder.)]
    (.append sb (char ch))
    (loop [ch (.read stream)]
      (if (not (or (= -1 ch) (whitespace? ch) (terminating-macro? ch)))
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
      (if (not (or (= -1 ch) (whitespace? ch) (macro? ch)))
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
         (if (whitespace? ch)
           (recur (.read stream))
           (cond
            (= -1 ch) (if eof-error?
                        (throw (RuntimeException. "EOF while reading"))
                        eof-value)
            (Character/isDigit ch) (read-a-number stream ch)
            (macro? ch) (let [the-macro (get-macro ch)
                              val (the-macro stream (char ch))]
                          (if (= stream val) ; no-op macros return the reader
                            (recur (.read stream))
                            val))
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
