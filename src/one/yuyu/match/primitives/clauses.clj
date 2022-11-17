(ns one.yuyu.match.primitives.clauses
  (:require [clojure.set :as set]
            [one.yuyu.match.primitives.analyze :as analyze]
            [one.yuyu.match.primitives.spec :as spec]))

(defn- computed-provision [{:keys [head guard? guard body] :as clause}]
  (let [mhead (analyze/search-head-vars head)
        stail (cond-> (analyze/search-body-vars body)
                guard? (into (analyze/search-body-vars guard)))
        shead (into #{} (keys mhead))
        shead1 (->> mhead
                    (filter #(-> % second (<= 1)))
                    keys
                    (into #{}))
        undefined (set/difference stail shead)
        unused (set/difference shead1 stail)]
    (cond-> clause
      (seq undefined) (assoc :vars/undefined undefined)
      (seq unused)    (assoc :vars/unused unused))))

(defn ->clause
  ([head mode body]
   (->clause head ::spec/noop mode body))
  ([head guard mode body]
   (let [has-guard? (not= guard ::spec/noop)]
     (->> (cond-> {:head head
                   :mode mode
                   :body body}
            has-guard? (assoc :guard? true :guard guard))
          computed-provision))))

;;;

(defn arity [clauses] (or (some-> clauses first :head count) 0))

(defn add-clause [clauses clause]
  (conj clauses (assoc clause :index (count clauses))))

(defn add-fallback [clauses arity]
  (let [head (repeat arity '_)
        mode '->
        body (list 'one.yuyu.match/fail)
        clause (-> (->clause head mode body)
                   (assoc :auto? true))]
    (add-clause clauses clause)))

(defn ->clauses [] [])
