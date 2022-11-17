(ns one.yuyu.match.primitives.analyze
  (:require [one.yuyu.match.primitives.spec :as spec]
            [one.yuyu.match.primitives.var :as var]))

(defn- unsolvable-var? [v] (and (spec/var? v) (not (some-> v resolve))))
(defn- inc-in [m k] (update m k (fnil inc 0)))

(defn search
  ([pred] (search pred #{} conj))
  ([pred acc add]
   (fn [form]
     (let [res (volatile! acc)]
       (-> (fn [form]
             (when (and form (symbol? form) (pred form))
               (vswap! res add form))
             form)
           (clojure.walk/postwalk form))
       @res))))

(def search-deps (search var/var?))
(def search-head-vars (search unsolvable-var? {} inc-in))
(def search-body-vars (search unsolvable-var?))

(defn erase-vars [form unused]
  (-> (fn [form]
        (if (and (symbol? form) (contains? unused form))
          '_
          form))
      (clojure.walk/postwalk form)))

(defn rewrite-aliases [form aliases]
  (if-not (seq aliases)
    form
    (-> (fn [form]
          (cond->> form
            (and (symbol? form) (contains? aliases form)) (get aliases)))
        (clojure.walk/postwalk form))))
