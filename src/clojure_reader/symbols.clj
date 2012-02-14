(ns clojure-reader.symbols
  (:use clojure-reader.util)
  (:import [java.util.regex Pattern Matcher]
           [clojure.lang Namespace Compiler Symbol Keyword Var]))

(defn namespace-for
  ([^Symbol sym] (namespace-for *ns* sym))
  ([^Namespace inns, ^Symbol sym]
     (let [ns-sym (symbol (.ns sym))
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
         (let [ks (symbol (.substring s 2))
               kns (if (not-nil? (namespace ks))
                     (namespace-for ks)
                     *ns*)]
           (if (not-nil? kns)
             (Keyword/intern (.. kns getName getName) (. ks getName))))

         :else
         (let [keyword? (= \: (.charAt s 0))
               sym (symbol (.substring s (if keyword? 1 0)))]
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
                                (if (or (nil? ns) (= (namespace sym) (name (ns-name ns))))
                            sym ;; cannot be found or same namespace
                            (symbol (name (ns-name ns)) (name sym))))
   :else
   (let [o (.getMapping *ns* sym)]
     (cond
      (nil? o) (symbol (name (ns-name *ns*)) (name sym))
      (class? o) (symbol nil (.getName ^Class o))
      (var? o) (symbol (name (ns-name (.ns ^Var o))) (name (.sym ^Var o)))
      :else nil))))

(defn is-special [sym] (.containsKey Compiler/specials sym))
