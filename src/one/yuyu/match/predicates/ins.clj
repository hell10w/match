(ns one.yuyu.match.predicates.ins
  (:require [one.yuyu.match.primitives.spec :as spec]
            [one.yuyu.match.primitives.env :as e]))

(defn wrong-instantiation [env l r vars]
  (e/push-error env
                :predicate/instantiation
                :vars vars
                :lr [l r]))

(defn instantiate-all [env args input?]
  (loop [all? true
         args (map-indexed vector args)
         uninstantiated #{}
         res []]
    (if (empty? args)
      [all? res uninstantiated]
      (let [[[index arg] & args] args]
        (if (or (not (input? arg index))
                (and (spec/ground? arg) (not (spec/wildcard? arg)))
                (when (symbol? arg) (resolve arg)))
          (recur all?
                 args
                 uninstantiated
                 (conj res arg))
          (if-let [instance (e/alias env arg)]
            (recur all?
                   args
                   uninstantiated
                   (conj res instance))
            (recur false
                   args
                   (conj uninstantiated arg)
                   (conj res arg))))))))


(defn invoke [env f l r payload input?]
  (let [[inst? args unins] (instantiate-all env (next r) input?)]
    (if inst?
      (f env l (list* (first r) args))
      (if-not payload
        (e/push-queue env l r unins)
        (wrong-instantiation env l r unins)))))
