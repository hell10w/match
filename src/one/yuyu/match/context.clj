(ns one.yuyu.match.context
  (:require [one.yuyu.match.dynamic :as d]))

(defn- push [{:keys [problems]} stage level id opts]
  (->> (assoc opts :id id :level level :stage stage)
       (vswap! problems conj)))

(defn push-warning [ctx stage id & {:as opts}]
  (some-> ctx (push stage :warning id opts)))

(defn push-error [ctx stage id & {:as opts}]
  (some-> ctx (push stage :error id opts)))

(defn ignored-assert? [{:keys [asserts]} assert]
  (and assert (or (not asserts) (not (contains? asserts assert)))))

(defn stop? [{:keys [problems stop-on-error]}]
  (and stop-on-error (seq (filter #(= :error (:level %)) @problems))))

(def continue? (complement stop?))

(defn check-errors [{:keys [problems show-problems?]} specs]
  (letfn [(dump [value]
            (binding [*out* *err*]
              (-> value clojure.pprint/pprint with-out-str print)))
          (group-all [problems]
            (-> (fn [m {:keys [arity clause level] :as problem}]
                  (->> (dissoc problem :arity :clause :level)
                       (update-in m [arity clause level] conj)))
                (reduce nil problems)))]
    (when-let [problems (seq @problems)]
      (let [{:keys [error]} (group-by :level problems)]
        (when show-problems?
          (dump specs)
          (let [problems (group-all problems)]
            (doseq [[arity problems] (into (sorted-map) problems)]
              (doseq [[clause problems] (into (sorted-map) problems)]
                (dump {:arity arity :clause clause :problems problems})))))
        (when (seq error)
          (throw (ex-info "Compilation errors" {:errors error})))))))

(defn normalize-mutual-exclusive [{:keys [default-mutually-exclusive]} value]
  (if (nil? value)
    default-mutually-exclusive
    value))

(defn ctx->errors [{:keys [problems]}]
  @problems)

(defn ->context
  ([& {:keys [asserts
              stop-on-error?
              show-problems?
              default-mutually-exclusive
              short-vars?
              cps?]
       :or {stop-on-error? true
            show-problems? d/*show-problems*
            default-mutually-exclusive true
            cps? false
            short-vars? false}}]
   {:asserts asserts
    :stop-on-error? stop-on-error?
    :show-problems? show-problems?
    :default-mutually-exclusive default-mutually-exclusive
    :cps? cps?
    :short-vars? short-vars?
    :problems (volatile! nil)}))
