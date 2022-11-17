(ns one.yuyu.match.compile.prepare
  (:require [one.yuyu.match.primitives.analyze :as analyze]
            [one.yuyu.match.primitives.spec :as spec]
            [one.yuyu.match.primitives.clauses :as clauses]
            [one.yuyu.match.context :as ctx]))

(defn push-warning [ctx arity clause id & opts]
  (apply ctx/push-warning ctx :preparation id :arity arity :clause clause opts))

(defn push-error [ctx arity clause id & opts]
  (apply ctx/push-error ctx :preparation id :arity arity :clause clause opts))

;;; parsing

(defn- pop-head [spec]
  (let [head (take-while spec/head? spec)]
    [head (drop (count head) spec)]))

(defn- pop-guard [spec]
  (if (spec/where? (first spec))
    [(second spec) (drop 2 spec)]
    [::spec/noop spec]))

(defn- pop-mode [spec]
  (let [head (first spec)]
    (if (spec/arrow? head)
      [head (next spec)]
      [::spec/noop spec])))

(defn- pop-body [spec] [(first spec) (next spec)])

(defn- split-clauses [spec]
  (loop [clauses (clauses/->clauses)
         spec spec]
    (if (empty? spec)
      (let [arity (clauses/arity clauses)]
        {:arity arity
         :clauses (clauses/add-fallback clauses arity)})
      (let [[head spec] (pop-head spec)
            [guard spec] (pop-guard spec)
            [mode spec] (pop-mode spec)
            [body spec] (pop-body spec)]
        (recur (->> (clauses/->clause head guard mode body)
                    (clauses/add-clause clauses))
               spec)))))

;;; verification

(defn- cleanup-clause [{:keys [vars/unused] :as clause}]
  (update clause :head analyze/erase-vars unused))

(defn- verify-clauses [{:keys [arity clauses] :as spec} ctx]
  (doseq [{:keys [index head mode body vars/undefined vars/unused]} clauses]
    (when (not= arity (count head))
      (push-error ctx arity index :clause/inconsistent-arity))
    (when (= ::spec/noop mode)
      (push-error ctx arity index :clause/no-mode))
    (when (= ::spec/noop body)
      (push-error ctx arity index :clause/no-body))
    (when (seq undefined)
      (push-error ctx arity index :clause/undefined-vars :undefined undefined))
    (when (seq unused)
      (push-warning ctx arity index :clause/unused-vars :unused unused)))
  (assoc spec :clauses (map cleanup-clause clauses)))

;;;

;; todo check clause uniqueness

(defn prepare [spec ctx]
  (-> (split-clauses spec)
      (verify-clauses ctx)))
