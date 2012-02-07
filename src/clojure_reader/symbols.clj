(ns clojure-reader.symbols
  (:use clojure-reader.util)
  (:import [java.util.regex Pattern Matcher]
           [clojure.lang Namespace Compiler Symbol Keyword Var]))

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
               kns (if (not-nil? (namespace ks))
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

(defn resolve-symbol [sym]
  (cond
   (> (.indexOf (name sym) ".") 0) sym ;; already namespace qualified or class name
   (not-nil? (namespace sym)) (let [ns (namespace-for sym)]
                          (if (or (nil? ns) (= (namespace sym) (.. ns getName getName)))
                            sym ;; cannot be found or same namespace
                            (Symbol/intern (.. ns getName getName) (. sym getName))))
   :else
   (let [o (.getMapping *ns* sym)]
     (cond
      (nil? o) (Symbol/intern (.. *ns* getName getName) (. sym getName))
      (instance? Class o) (Symbol/intern nil (.getName ^Class o))
      (instance? Var o) (Symbol/intern (.. ^Var o ns getName getName) (.. ^Var o sym getName))
      :else nil))))

(defn is-special [sym] (.containsKey Compiler/specials sym))
