(ns one.yuyu.match.primitives.vars
  (:refer-clojure :exclude [var? flush])
  (:require [one.yuyu.match.primitives.var :as var]
            [one.yuyu.match.primitives.analyze :as analyze]
            [clojure.set :as set]))

;;; building

(defn- -add-root [{:keys [base] :as vars} column]
  (update vars :roots assoc column (var/->var column base true)))

(defn push-root [{:keys [roots] :as vars} column]
  (if-let [sym (get roots column)]
    [sym vars]
    (recur (-add-root vars column) column)))

(defn- -add-form [{:keys [base counter] :as vars} form]
  (let [sym (var/->var counter base false)]
    (-> vars
        (update :form->sym assoc form sym)
        (update :register assoc sym form)
        (update :counter dec))))

(defn push-form [{:keys [form->sym] :as vars} form]
  (if-let [sym (get form->sym form)]
    [sym vars]
    (recur (-add-form vars form) form)))

(defn flush [vars]
  (select-keys vars [:roots :register]))

;;;

(defn ->vars [short-vars?]
  {:roots {}
   :form->sym {}
   :deps {}
   :register {}
   :counter -1
   :base (if short-vars? "$$" (str (name (gensym "$$")) "-"))})

;;; aggregates

(defn provision-deps [{:keys [register] :as vars}]
  (->> (-> (fn [m [sym form]] (assoc m sym (analyze/search-deps form)))
           (reduce {} register))
       (assoc vars :deps)))

(defn provision-rdeps [{:keys [deps] :as vars}]
  (letfn [(add-rdep [m [sym dep]]
            (update m dep (fnil conj #{}) sym))
          (add-rdeps [m [sym deps]]
            (->> (map vector (repeat sym) deps)
                 (reduce add-rdep m)))]
    (->> (reduce add-rdeps {} deps)
         (assoc vars :rdeps))))

(defn provision-priorities [{:keys [roots rdeps] :as vars}]
  (letfn [(adjust-priority [old new] (cond-> new old (max old)))
          (visit-symbol [m root priority rdeps]
            (-> (update m root adjust-priority priority)
                (walk (get rdeps root) (inc priority) rdeps)))
          (walk [m roots priority rdeps]
            (-> (fn [m root] (visit-symbol m root priority rdeps))
                (reduce m roots)))]
    (->> (walk {} (vals roots) 0 rdeps)
         (assoc vars :priorities))))

;;;

(defn ->ordered-map [priorities]
  (-> (fn [a b]
        (compare [(get priorities a) a]
                 [(get priorities b) b]))
      sorted-map-by))

(defn ->bindings [queue seen {:keys [deps register priorities] :as _vars}]
  (loop [m (->ordered-map priorities)
         seen seen
         queue queue]
    (if (empty? queue)
      [seen (keys m) (vals m)]
      (let [dependency (first queue)]
        (if (contains? seen dependency)
          (recur m seen (disj queue dependency))
          (recur (assoc m dependency (get register dependency))
                 (conj seen dependency)
                 (-> (disj queue dependency)
                     (into (get deps dependency)))))))))

(defn let-deps [ks {:keys [deps] :as _vars}]
  (let [ks (set ks)
        *deps (->> (select-keys deps ks)
                   vals
                   (reduce into #{}))]
    (set/difference *deps ks)))
