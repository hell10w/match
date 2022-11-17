(ns one.yuyu.match.primitives.env
  (:refer-clojure :exclude [flush alias])
  (:require [one.yuyu.match.primitives.vars :as vars]
            [one.yuyu.match.primitives.exprs :as exprs]
            [one.yuyu.match.primitives.analyze :as analyze]
            [one.yuyu.match.primitives.queue :as q]
            [one.yuyu.match.context :as ctx]))

(defn push-warning [{:keys [arity pos ctx] :as env} id & opts]
  (let [[clause arg] pos]
    (apply ctx/push-warning ctx :unification id
           :arity arity :clause clause :arg arg
           opts))
  env)

(defn push-error [{:keys [arity pos ctx] :as env} id & opts]
  (let [[clause arg] pos]
    (apply ctx/push-error ctx :unification id
           :arity arity :clause clause :arg arg
           opts))
  env)

;;; building

(defn- -retrieve [m k f & args]
  (let [store (k m)
        [id *store] (apply f store args)
        m (cond-> m (not= store *store) (assoc k *store))]
    [id m]))

(defn alias
  "spec var -> env var"
  [{:keys [aliases]} var]
  (get aliases var))

(defn push-alias [env spec-var env-var]
  (update env :aliases assoc spec-var env-var))

(defn push-root [env column]
  (-retrieve env :vars vars/push-root column))

(defn push-assign [env form]
  (-retrieve env :vars vars/push-form form))

(defn push-constraint [{:keys [ctx] :as env} test
                       & {:keys [assert mutually-exclusive]}]
  (if (ctx/ignored-assert? ctx assert)
    env
    (let [mutually-exclusive (ctx/normalize-mutual-exclusive ctx mutually-exclusive)
          expr (exprs/->constraint test mutually-exclusive)
          [id env] (-retrieve env :exprs exprs/push expr)]
      (update env :current conj id))))

(defn push-guard [{aliases :aliases :as env} test]
  (->> (cond-> test
         (seq aliases) (analyze/rewrite-aliases aliases))
       (push-constraint env)))

(defn push-equality [env var val]
  (let [expr (exprs/->equality var val)
        [id env] (-retrieve env :exprs exprs/push expr)]
    (update env :current conj id)))

(defn push-return [{:keys [aliases] :as env} body backtrack? index]
  (let [body (analyze/rewrite-aliases body aliases)
        expr (exprs/->body body backtrack? index)
        [id env] (-retrieve env :exprs exprs/push expr)]
    (update env :current conj id)))

(defn commit [{:keys [current] :as env}]
  (-> (update env :clauses conj current)
      (assoc :current []
             :aliases {})))

(defn flush [{:keys [vars clauses exprs]}]
  (let [vars (-> (vars/flush vars)
                 vars/provision-deps
                 vars/provision-rdeps
                 vars/provision-priorities)
        exprs (-> (exprs/flush exprs)
                  (exprs/provision-deps vars))]
    {:vars vars
     :inputs (->> (-> vars :roots)
                  (sort-by first)
                  (map second))
     :exprs exprs
     :clauses clauses}))

;;; queue

(defn done? [{:keys [queue]}] (q/done? queue))

(defn push-queue
  ([env l r]
   (push-queue env l r nil nil))
  ([env l r payload]
   (push-queue env l r payload nil))
  ([env l r payload pos]
   (let [split? (boolean payload)
         pos (if pos pos (:pos env))
         _ (assert pos)
         value [pos [l r payload]]]
     (update env :queue q/push value split?))))

(defn pop-queue [{:keys [queue] :as env}]
  (let [[[pos v] q] (q/pop queue)]
    [v (assoc env :queue q :pos pos)]))

;;;

(defn ->env
  ([] (->env nil nil))
  ([ctx arity]
   {:ctx ctx
    :arity arity
    :queue (q/->queue)
    :pos []
    :vars (vars/->vars (:short-vars? ctx))
    :exprs (exprs/->exprs)
    :aliases {}
    :current []
    :clauses []}))
