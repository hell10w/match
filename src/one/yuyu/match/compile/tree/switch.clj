(ns one.yuyu.match.compile.tree.switch
  (:require [one.yuyu.match.primitives.exprs :as exprs]))

(defn ->ob []
  {:keys [] :vals nil})

(defn add [{:keys [vals] :as ob} k v]
  (let [new? (not (contains? vals k))]
    (cond-> (assoc ob :vals (update vals k conj v))
      new? (update :keys conj k))))

(defn ob->seq [{:keys [keys vals]}]
  (reduce (fn [m k] (conj m (reverse (get vals k))))
          []
          keys))

;;;

(defn- fallback? [v] (-> v meta :fallback))
(defn- mark-fallback [v] (vary-meta v assoc :fallback true))
(defn- reset-fallback [v] (vary-meta v dissoc :fallback))

(defn- classify [a clause]
  (let [b (first clause)]
    (cond
      (= a b)              0
      (exprs/similar? a b) (if (fallback? clause) 1 0)
      :else                1)))

(defn- split-by-similarity [clauses expr]
  (reduce (fn [m clause]
            (let [index (classify expr clause)]
              (update m index conj (reset-fallback clause))))
          [[] []]
          clauses))

(defn- clauses->buckets [clauses]
  (->> clauses
       (reduce (fn [m clause]
                 (add m (exprs/key (first clause)) clause))
               (->ob))
       ob->seq))

(defn- fallback-provision [clauses fallback]
  (let [fallback? (seq fallback)]
    (cond-> (cond->> clauses
              fallback? (map #(concat % fallback)))
      fallback? (concat (list fallback)))))

(defn derive-branches [clauses top]
  (let [[similar fallback] (split-by-similarity clauses top)
        fallback (map mark-fallback fallback)
        buckets (clauses->buckets similar)
        clauses (map #(map next %) buckets)]
    [(map ffirst buckets)
     (fallback-provision clauses fallback)]))
