(ns one.yuyu.match.compile.tree
  (:refer-clojure :exclude [merge])
  (:require [one.yuyu.match.primitives.exprs :as exprs]
            [one.yuyu.match.primitives.vars :as vars]
            [one.yuyu.match.compile.tree.switch :as switch]
            [one.yuyu.match.compile.tree.merge :as merge]
            [one.yuyu.match.context :as c]))

;;; checker for unused clauses

(defn- spec->checker [{:keys [arity clauses] :as _spec} ctx]
  {:ctx ctx
   :arity arity
   :clauses (->> (remove :auto? clauses)
                 (map :index)
                 (into #{})
                 (volatile!))})

(defn- clause-visited! [{:keys [clauses]} index]
  (vswap! clauses disj index))

(defn- check [{:keys [arity clauses ctx]}]
  (when-let [clauses (seq @clauses)]
    (doseq [clause clauses]
      (c/push-warning ctx :tree :clause/unreachable
                      :arity arity
                      :clause clause))))

;;;

(defn- continue [{:keys [walk] :as scope} clauses]
  (walk clauses scope))

(defn- ->scope [walk env checker ctx]
  {:walk walk
   :seen (into #{} (-> env :vars :roots vals))
   :env env
   :checker checker
   :ctx ctx})

;;;

(defn- with-deps [{seen :seen {vars :vars} :env :as scope} deps f]
  (let [[seen ks vs] (vars/->bindings deps seen vars)]
    (if (empty? ks)
      (f scope)
      {:op :let
       :b (interleave ks vs)
       :ks (into #{} ks)
       :deps (vars/let-deps ks vars)
       :c (list (f (assoc scope :seen seen)))})))

;;;

(defn- terminal [clauses {:keys [checker] :as scope}]
  (let [{:keys [clause body deps]} (ffirst clauses)]
    (clause-visited! checker clause)
    (->> (fn [_scope] {:op :terminal :b body :deps deps})
         (with-deps scope deps))))

(defn- backtrack [clauses scope]
  {:op :backtrack
   :c (list (terminal clauses scope)
            (continue scope (next clauses)))})

(defn- finalize [clauses scope]
  (let [backtrack? (and (-> clauses ffirst :backtrack?)
                        (seq (next clauses)))]
    (if backtrack?
      (backtrack clauses scope)
      (terminal clauses scope))))

(defn- switch [clauses scope]
  (case (count (first clauses))
    1 (finalize clauses scope)
    (let [top (ffirst clauses)
          [tops branches] (switch/derive-branches clauses top)
          deps (reduce into #{} (map :deps tops))
          root? (exprs/root? top)]
      (->> (fn [scope]
             (cond-> {:op :switch
                      :t (if root? :case :cond)
                      :ts (map exprs/test tops)
                      :deps deps
                      :c (doall (map #(continue scope %) branches))}
               root? (assoc :r (exprs/root top)
                            :ks (map exprs/key tops))
               true merge/unwind))
           (with-deps scope deps)))))

;;;

(defn- walk [clauses scope]
  (case (count clauses)
    0 nil
    1 (switch clauses scope)
    (switch clauses scope)))

(defn- env->clauses [{:keys [clauses exprs] :as _spec}]
  (map (fn [clause]
         (->> (map #(get exprs %) clause)
              (sort-by exprs/key-fn)))
       clauses))

(defn build-tree [{:keys [env] :as spec} ctx]
  (let [checker (spec->checker spec ctx)
        tree (-> (->scope walk env checker ctx)
                 (continue (env->clauses env)))]
    (check checker)
    (assoc spec :tree tree)))
