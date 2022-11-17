(ns one.yuyu.match.compile
  (:refer-clojure :exclude [compile])
  (:require [one.yuyu.match.primitives.spec :as s]
            [one.yuyu.match.compile.prepare :as p]
            [one.yuyu.match.compile.unify :as u]
            [one.yuyu.match.compile.tree :as t]
            [one.yuyu.match.compile.cps :as cps]
            [one.yuyu.match.compile.codegen :as cg]
            [one.yuyu.match.context :as ctx]))

(defn compile
  ([spec] (compile spec (ctx/->context)))
  ([spec ctx]
   (cond-> (p/prepare spec ctx)
     (ctx/continue? ctx) (u/unify-clauses ctx)
     (ctx/continue? ctx) (t/build-tree ctx)
     (ctx/continue? ctx) (cps/process ctx)
     (ctx/continue? ctx) (cg/codegen ctx))))

;;; fn generation

(defn spec->fn
  ([spec] (spec->fn spec (ctx/->context)))
  ([spec ctx]
   (let [[alias spec] (s/parse-spec spec)]
     (spec->fn spec alias ctx)))
  ([specs-by-arity alias ctx]
   (let [by-arity (-> (fn [spec]
                        (let [{:keys [env sexpr]} (compile spec ctx)]
                          `([~@(:inputs env)] ~sexpr)))
                      (map specs-by-arity)
                      doall)]
     (ctx/check-errors ctx specs-by-arity)
     (concat (if alias `(fn ~alias) `(fn))
             by-arity))))

;;; match generation

(defn spec->match
  ([inputs spec] (spec->match inputs spec (ctx/->context)))
  ([inputs spec ctx]
   (assert (vector? inputs) "match input ought to be vector")
   (let [{:keys [arity env sexpr]} (compile spec ctx)
         _ (assert (= (count inputs) arity) "arity mismatch")]
     (ctx/check-errors ctx spec)
     `(let [~@(interleave (:inputs env) inputs)]
         ~sexpr))))
