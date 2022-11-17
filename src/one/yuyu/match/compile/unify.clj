(ns one.yuyu.match.compile.unify
  (:require [one.yuyu.match.primitives.env :as e]
            [one.yuyu.match.primitives.var :as var]
            [one.yuyu.match.primitives.spec :as spec]
            [one.yuyu.match.predicates.core :as p]
            [one.yuyu.match.predicates.seq :as sp]
            [one.yuyu.match.predicates.maps :as mp]))

(defn- unknown-unification [env l r]
  (e/push-error env :unify/unknown-equivalency :lr [l r]))

(defn- equality [env l r]
  (cond
    (var/var? r)  (e/push-constraint env (list '= l r))  ;; todo sort args?
    :else         (e/push-equality env l r)))

(defn- rename [env l r]
  (if-let [previous (e/alias env r)]
    (equality env l previous)
    (e/push-alias env r l)))

(defn- unify-queue [env]
  (if (e/done? env)
    env
    (let [[[l r payload] env] (e/pop-queue env)]
      (cond
        (spec/wildcard? l) (recur env)
        (spec/wildcard? r) (recur env)
        (= r ())           (recur (e/push-queue env l (list 'empty '_)))
        (= r {})           (recur (e/push-queue env l (list 'empty '_)))
        (seq? r)           (recur (p/dispatch env l r payload))
        (spec/seq+tail? r) (recur (sp/*seq+tail env l r))
        (vector? r)        (recur (sp/*seq env l r))
        (spec/map+tail? r) (recur (mp/*map+tail env l r payload))
        (map? r)           (recur (mp/*map env l r payload))
        (spec/var? l)      (recur (rename env r l))
        (spec/var? r)      (recur (rename env l r))
        (spec/value? r)    (recur (equality env l r))
        :else              (recur (unknown-unification env l r))))))

(defn- unify-clause [env {:keys [index head mode guard? guard body]}]
  (-> (reduce (fn [env [column form]]
                (let [[root env] (e/push-root env column)]
                  (e/push-queue env root form nil [index column])))
              env
              (map-indexed vector head))
      (unify-queue)
      (cond-> guard? (e/push-guard guard))
      (e/push-return body (spec/backtrack? mode) index)
      (e/commit)))

(defn unify-clauses [{:keys [arity clauses] :as spec} ctx]
  (let [env (reduce unify-clause
                    (e/->env ctx arity)
                    clauses)]
    (assoc spec :env (e/flush env))))
