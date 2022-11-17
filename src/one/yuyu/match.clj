(ns one.yuyu.match
  (:refer-clojure :exclude [defn] :rename {fn *fn})
  (:require [one.yuyu.match.primitives.spec :as s]
            [one.yuyu.match.dynamic :as d]
            [one.yuyu.match.compile :as c]
            [one.yuyu.match.runtime :as rt]
            [one.yuyu.match.context :as ctx]))

(defn- form->ctx [form]
  (let [{:keys [cps m-e mutually-exclusive short-vars?] :as m} (meta form)
        asserts (-> (*fn [m [k v]] (cond-> m (true? v) (conj k)))
                    (reduce #{} m))]
    (ctx/->context :asserts asserts
                   :mutually-exclusive? (or m-e mutually-exclusive)
                   :cps? cps
                   :short-vars? short-vars?)))

(defn- -match
  ([input spec form] (-match input spec form nil))
  ([input spec form var-pattern]
   (binding [d/*var-pattern* (or var-pattern d/*var-pattern*)]
     (let [ctx (form->ctx form)]
       (c/spec->match input spec ctx)))))

(defn- -fn
  ([spec form] (-fn spec form nil))
  ([spec form var-pattern]
   (binding [d/*var-pattern* (or var-pattern d/*var-pattern*)]
     (let [ctx (form->ctx form)]
       (c/spec->fn spec ctx)))))

(defn- -defn
  ([name spec form] (-defn name spec form nil))
  ([name spec form var-pattern]
   (binding [d/*var-pattern* (or var-pattern d/*var-pattern*)]
     (let [[alias spec] (s/parse-spec spec)
           ctx (form->ctx form)]
       (assert (not alias))
       `(~'def ~name ~(c/spec->fn spec name ctx))))))

;;;

(defmacro match [input & spec] (-match input spec &form))
(defmacro fn [& spec] (-fn spec &form))
(defmacro defn [name & spec] (-defn name spec &form))

(defmacro ?match [input & spec] (-match input spec &form :question-mark))
(defmacro ?fn [& spec] (-fn spec &form :question-mark))
(defmacro ?defn [name & spec] (-defn name spec &form :question-mark))

(def backtrack? rt/backtrack?)
(def fail rt/fail)
