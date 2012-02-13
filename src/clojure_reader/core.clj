(ns clojure-reader.core
  (:refer-clojure :exclude [read read-string])
  (:use clojure-reader.util
        [clojure-reader.numbers :only (match-number)]
        [clojure-reader.symbols :only (interpret-token resolve-symbol is-special)])
  (:import [java.io PushbackReader StringReader]
           [java.util ArrayList]
           [java.util.regex Pattern Matcher]
           [clojure.lang IFn RT Symbol Keyword IMeta IReference IObj]
           [clojure.lang PersistentList PersistentHashSet LazilyPersistentVector]
           [clojure.lang IPersistentMap IPersistentCollection]
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

(defn read-unicode-char
  ([^String token, ^long offset, ^long length, ^long base]
     (if (not= (.length token) (+ offset length))
       (throw (IllegalArgumentException. (str "Invalid unicode character: \\" token)))
       (loop [uc 0 i offset]
         (if (= i (+ offset length))
           uc
           (do
             (let [ch (.charAt token i)
                   d (Character/digit ch base)]
               (if (= -1 d)
                 (throw (IllegalArgumentException. (str "Invalid digit: " ch)))
                 (recur (+ d (* uc base)) (+ i 1)))))))))
  ([^PushbackReader reader,
     ^Character initch,
     base, length,
     exact?]
      (loop [uc (Character/digit initch (int base)) i 1]
        (if (= -1 uc)
          (throw (IllegalArgumentException. (str "Invalid digit: " initch)))
          (let [ch (.read reader)]
            (if (or (= -1 ch) (whitespace? (char ch)) (macro? ch))
              (do
                (.unread reader ch)
                (if (and exact? (not= length i))
                  (throw (IllegalArgumentException. (str "Invalid character length: " i ", should be: " length)))
                  uc
                  )
                )
              (let [d (Character/digit (char ch) (int base))]
                (if (= -1 d)
                  (throw (IllegalArgumentException. (str "Invalid digit: " ch)))
                  (recur (+ d (* uc base)) (+ i 1))))))))))

(defn read-escaped-character [^PushbackReader reader]
  (let [ch (.read reader)]
    (if (= -1 ch)
      (throw (RuntimeException. "EOF while reading string"))
      (let [chr (char ch)]
        (condp = chr
          \t \tab
          \r \return
          \n \newline
          \\ \\
          \" \"
          \b \backspace
          \f \formfeed
          \u
          (let [ch (.read reader)]
            (if (= -1 (Character/digit ch 16))
              (throw (RuntimeException. (str "Invalid unicode escape: \\u" (char ch))))
              (read-unicode-char reader (char ch) 16 4 true)))
          (if (Character/isDigit chr)
            (let [uchr (read-unicode-char reader chr 8 3 false)]
              (if (> uchr 0377)
                (throw (RuntimeException. "Octal escape sequence must be in range [0, 377]."))
                (char uchr)))
            (throw (RuntimeException. (str "Unsupported escape character: \\" chr)))))))))

(defreader string-reader
  (let [sb (StringBuilder.)]
    (loop [ch (.read reader)]
      (if (= -1 ch)
        (throw (RuntimeException. "EOF while reading string"))
        (let [chr (char ch)]
          (if (= \" chr)
            (.toString sb)
            (if (not= \\ chr)
              (do
                (.append sb chr)
                (recur (.read reader)))
              (let [echr (read-escaped-character reader)]
                (.append sb echr)
                (recur (.read reader))))))))))

(defreader comment-reader
  (loop [ch (.read reader)]
    (if (not (or (= -1 ch)
                 (= \newline (char ch))
                 (= \return (char ch))
                 ))
      (recur (.read reader))
      reader)))

(defn to-meta-map [meta]
  (cond
   (or (instance? Symbol meta) (instance? String meta)) {:tag meta}
   (instance? Keyword meta) {meta true}
   (instance? IPersistentMap meta) meta
   :else (throw (IllegalArgumentException. "Metadata must be Symbol,Keyword,String or Map"))))

(defn to-meta-map-with-line-number
  ([meta] (to-meta-map-with-line-number -1))
  ([meta line]
     (let [meta-without-line (to-meta-map meta)]
       (if (not= line -1)
         (assoc meta-without-line :line line)
         meta-without-line))))

(defreader meta-reader
  (let [line (get-line-number reader)
        meta (to-meta-map-with-line-number (read reader true nil true) line)
        o (read reader true nil true)]
    (if (instance? IMeta o)
      (if (instance? IReference o)
        (do (.resetMeta o meta) o)
        (.withMeta o (merge (RT/meta o) meta)))
      (throw (IllegalArgumentException. "Metadata can only be applied to IMetas")))))


(def ^:dynamic *gensym-environment* nil)


(defn- unquote? [form]
  (and (seq? form) (= (first form) clojure.core/unquote)))

(defn- unquote-splicing? [form]
  (and (seq? form) (= (first form) clojure.core/unquote-splicing)))

(declare syntax-quote)

(defn- syntax-quote-symbol [^Symbol sym]
  (list (Symbol/intern "quote")
        (cond
         (and (nil? (namespace sym)) (.endsWith (name sym) "#"))
         (let [gmap *gensym-environment*]
           (if (nil? gmap)
             (throw (IllegalStateException. "Gensym literal not in syntax-quote"))
             (let [gs (gmap sym)]
               (if (nil? gs)
                 (let [sym-name (str (slice (name sym) 0 -1) "__" (RT/nextID) "__auto__")
                       gs (Symbol/intern nil sym-name)]
                   (var-set (var *gensym-environment*) (assoc gmap sym gs))
                   gs)
                 gs))))

         (and (nil? (namespace sym)) (.endsWith (name sym) "."))
         (let [csym (Symbol/intern nil (slice (name sym) 0 -1))
               rsym (resolve-symbol csym)]
           (Symbol/intern nil (.concat (name rsym) ".")))

         (and (nil? (namespace sym)) (.startsWith (name sym) "."))
         sym

         :else
         (if (not-nil? (namespace sym))
           (let [maybe-class (.getMapping *ns* (Symbol/intern nil (.getNamespace sym)))]
             (if (instance? Class maybe-class)
               (Symbol/intern (.getName maybe-class) (name sym))
               (resolve-symbol sym)))
           (resolve-symbol sym)))))

(defn- flatten-map [form]
  (reduce (fn [result keyval] (apply conj result keyval)) [] form))

(defn- seq-expand-list [lst]
  (doall (map (fn [item]
                (cond
                 (unquote? item) (RT/list 'clojure.core/list (second item))
                 (unquote-splicing? item) (second item)
                 :else (list 'clojure.core/list (syntax-quote item))
                 )
                ) lst)))

(defn- syntax-quote-seq [constructor form]
  (let [inner (list 'clojure.core/seq
                    (apply list 'clojure.core/concat
                          (seq-expand-list form)))]
    (if (nil? constructor)
      inner
      (RT/list 'clojure.core/apply constructor inner))))

(defn- syntax-quote-col [form]
  (cond
   (map? form) (syntax-quote-seq 'clojure.core/hash-map (flatten-map form))
   (vector? form) (syntax-quote-seq 'clojure.core/vector form)
   (set? form) (syntax-quote-seq 'clojure.core/hash-set form)
   (or (seq? form) (list? form))
   (if (nil? (seq form)) (list 'clojure.core/list) (syntax-quote-seq nil form))
   :else (throw (UnsupportedOperationException. "Unknown Collection type"))))


(defn syntax-quote [form]
  (let [ret (cond
             (is-special form) (list 'quote form)
             (instance? Symbol form) (syntax-quote-symbol form)
             (unquote? form) (RT/second form)
             (unquote-splicing? form) (throw (IllegalStateException. "splice not in list"))
             (instance? IPersistentCollection form) (syntax-quote-col form)
             (or  (instance? Keyword form) (instance? Number form)
                  (instance? Character form) (instance? String form)) form
                  :else (list 'quote form))]
    (if (and (instance? IObj form) (not-nil? (meta form)))
      (let [new-meta (dissoc  (meta form) :line)] ; filter line numbers
        (if (> 0 (.count new-meta))
          (list clojure.core/with-meta ret (syntax-quote (meta form)))
          ret))
      ret
      )))

(defreader syntax-quote-reader
  (binding [*gensym-environment* {}]
    (let [form (read reader true nil true)]
      (syntax-quote form))))

(defreader unquote-reader
  (let [ch (.read reader)]
    (if (= -1 ch)
      (throw (RuntimeException. "EOF while reading character"))
      (if (= \@ (char ch))
        (let [o (read reader true nil true)]
          (RT/list clojure.core/unquote-splicing o))
        (do
          (.unread reader ch)
          (let [o (read reader true nil true)]
            (RT/list clojure.core/unquote o)))))))

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

(defreader character-reader
  (let [ch (.read reader)]
    (if (= -1 ch)
      (throw (RuntimeException. "EOF while reading character"))
      (let [token (read-a-token reader (char ch))]
        (if (= 1 (.length token))
          (Character/valueOf (.charAt token 0))
          (condp = token
            "newline" \newline
            "space" \space
            "tab" \tab
            "backspace" \backspace
            "formfeed" \formfeed
            "return" \return
            (cond
             (.startsWith token "u")
             (let [c (read-unicode-char token 1 4 16)]
               (if (and  (>= c 0xD800 (<= c 0xDFFF)))
                 (throw (RuntimeException. (str "Invalid character constant: \\u"
                                                (Integer/toString c 16))))
                 (char c)))

             (.startsWith token "o")
             (let [len (- (.length token) 1)]
               (if (> len 3)
                 (throw (RuntimeException. (str "Invalid octal escape sequence length: " len)))
                 (let [uc (read-unicode-char token 1 len 8)]
                   (if (> uc 0377)
                     (throw (RuntimeException. "Octal escape sequence must be in range [0, 377]."))
                     (char uc)))))

             :else
             (throw (RuntimeException. (str "Unsupported character: \\" token))))))))))

(declare ctor-reader)

(defreader dispatch-reader
  (loop [ch (.read reader)]
    (if (= -1 ch)
      (throw (RuntimeException. "EOF while reading character"))
      (let [dfn (aget dispatch-macros ch)
            chr (char ch)]
        (if (nil? dfn)
          (do
            (.unread reader ch)
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

(defn- read-record [^PushbackReader reader, ^Symbol sym] nil)

(defn- get-data-reader [tag]
  (let [data-readers clojure.core/*data-readers*
        data-reader (data-readers tag)]
    (if (nil? data-reader)
      (let [default-reader (clojure.core/default-data-readers tag)]
        (if (nil? default-reader)
          (throw (RuntimeException. (str "No reader function for tag" tag)))
          default-reader
          ))
      data-reader)))

(defn- read-tagged [^PushbackReader reader, ^Symbol tag]
  (let [o (read reader true nil true)
        data-reader (get-data-reader tag)]
    (data-reader o)))

(defreader ctor-reader
  (let [sym (read reader true nil false)]
    (if (not (instance? Symbol sym))
      (throw (RuntimeException. "Reader tag must be a symbol"))
      (if (.contains (name sym) ".")
        (read-record reader sym)
        (read-tagged reader sym)))))

(defreader fn-reader)
(defreader arg-reader)

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
