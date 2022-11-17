(ns one.yuyu.match.predicates.zip
  (:refer-clojure :exclude [next])
  (:require [clojure.zip :as zip]
            [one.yuyu.match.primitives.env :as e]))

(defn bad-arity [env l r]
  (e/push-error env :zip/bad-arity :lr [l r]))

(defn- assign
  ([env l r f] (assign env l r f true))
  ([env l r f zero-arity?]
   (let [arity (dec (count r))
         ref? (= arity 1)
         ref (if ref? (second r) ::no-ref)]
     (if (and (not ref?) (not zero-arity?))
       (bad-arity env l r)
       (let [[*ref env] (e/push-assign env `(~f ~l))]
         (cond-> env
           (not ref?) (e/push-constraint *ref *ref)
           ref? (e/push-queue *ref ref)))))))

(defn end [env l r]
  (let [[_ root-ref] r
        [root env] (e/push-assign env `(zip/root ~l))]
    (cond-> (e/push-constraint env `(zip/end? ~l))
      root-ref (e/push-queue root root-ref))))

(defn- unpack [env l node loc]
  (let [[*node env] (e/push-assign env `(zip/node ~l))]
    (cond-> env
      node (e/push-queue *node node)
      loc (e/push-queue l loc))))

(defn node [env l r] (let [[_ node loc] r] (unpack env l node loc)))
(defn loc  [env l r] (let [[_ loc node] r] (unpack env l node loc)))

(defn root [env l r] (assign env l r `zip/root) false)
(defn next [env l r] (assign env l r `zip/next))
(defn prev [env l r] (assign env l r `zip/prev))
(defn up [env l r] (assign env l r `zip/up))
(defn down [env l r] (assign env l r `zip/down))
(defn left [env l r] (assign env l r `zip/left))
(defn right [env l r] (assign env l r `zip/right))
(defn rightmost [env l r] (assign env l r `zip/rightmost))
(defn leftmost [env l r] (assign env l r `zip/leftmost))
