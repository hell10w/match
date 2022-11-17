(ns one.yuyu.match.primitives.spec
  (:refer-clojure :exclude [var?])
  (:require [one.yuyu.match.dynamic :as d]))

(defn- var-pattern []
  (case d/*var-pattern*
    :question-mark #"^\?.*"
    :upper-case #"^[A-Z].*"
    (if (instance? java.util.regex.Pattern d/*var-pattern*)
      d/*var-pattern*
      (throw (ex-info "unknown var pattern" {:var-pattern d/*var-pattern*})))))

(defn wildcard? [v] (= v '_))

(defn var? [s] (and (symbol? s) (re-matches (var-pattern) (name s))))

(defn value? [v]
  (or (nil? v)
      (boolean? v)
      (number? v)
      (string? v)
      (keyword? v)
      (symbol? v)))

(defn escape [v] (if-not (symbol? v) v `(~'quote ~v)))

(defn ground? [v]
  (or (and (value? v) (not (var? v)))
      (and (seqable? v) (every? ground? (seq v)))))

(defn arrow? [sym]
  (case sym
    (<- ->) true
    false))

(defn where? [v] (= 'where v))

(defn head? [v] (and (not (arrow? v)) (not (where? v))))

(defn backtrack? [mode] (= mode '<-))

(defn seq+tail? [v] (and (vector? v) (seq v) (= '| (peek (pop v)))))

(defn map+tail? [v] (and (map? v) (contains? v '|)))

;;; grouping of alias with multiarity

(defn- lists? [spec] (every? list? spec))

(defn parse-spec [[head & tail :as spec]]
  (cond
    (symbol? head) (if (lists? tail)
                     [head tail]
                     [nil (list spec)])
    (lists? spec)  [nil spec]
    :else          [nil (list spec)]))
