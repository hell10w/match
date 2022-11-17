(ns one.yuyu.match.predicates.maps
  (:require [one.yuyu.match.primitives.env :as e]
            [one.yuyu.match.predicates.ins :as ins]))

(defn -ensure-map [env l]
  (-> env
      (e/push-constraint (list 'map? l) :assert :map?)
      (e/push-constraint (list 'seq l) :assert :seq)))

(defn- push-kv [env l k v]
  (let [[*k env] (e/push-assign env `(get ~l ~k))]
    (e/push-queue env *k v)))

(defn- push-kvs [env l ks vs]
  (loop [env env
         ks ks
         vs vs]
    (if (empty? ks)
      env
      (recur (push-kv env l (first ks) (first vs))
             (next ks)
             (next vs)))))

(defn *map [env l r payload]
  (let [env (cond-> env (not payload) (-ensure-map l))
        [inst? args unins] (ins/instantiate-all env (keys r) (constantly true))]
    (if inst?
      (push-kvs env l args (vals r))
      (if-not payload
        (e/push-queue env l r unins)
        (ins/wrong-instantiation env l r unins)))))

(defn *map+tail [env l r payload]
  (-> (*map env l (dissoc r '|) payload)
      (e/push-queue l (get r '|))))
