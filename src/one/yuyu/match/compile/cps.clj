(ns one.yuyu.match.compile.cps
  (:refer-clojure :exclude [flush])
  (:require [clojure.zip :as zip]
            [clojure.set :as set]))

(defn tree-zipper [root]
  (zip/zipper :op
              :c
              #(assoc %1 :c %2)
              root))

(defn postwalk [f loc]
  (letfn [(visit [loc]
            (let [node (zip/node loc)
                  *node (f node)]
              (cond-> loc (not= node *node) (zip/replace *node))))
          (right? [loc] (zip/right loc))
          (down? [loc] (zip/down loc))
          (up? [loc] (zip/up loc))]
    (loop [loc loc
           dive? true]
      (cond
        (and dive? (down? loc)) (-> loc zip/down (recur true))
        (right? loc)            (-> loc visit zip/right (recur true))
        (up? loc)               (-> loc visit zip/up (recur false))
        :else                   (visit loc)))))

;;;

(defn- ->scope []
  {:usage nil
   :fragments nil
   :order nil})

(defn- mark-usage [{:keys [usage] :as scope} sym]
  (assoc scope :usage (update usage sym (fnil inc 0))))

(defn- add-fragment [scope sym child]
  (-> (mark-usage scope sym)
      (update :order assoc sym (count (:order scope)))
      (update :fragments assoc child sym)))

(defn- add-fragment! [scope child]
  (let [sym (gensym "cps-")]
    (vswap! scope add-fragment sym child)
    sym))

(defn- flush [{:keys [fragments] :as scope}]
  (assoc scope :fragments (zipmap (vals fragments) (keys fragments))))

;;;

(defmulti accumulate-deps :op)
(defmethod accumulate-deps :terminal [node] node)
(defmethod accumulate-deps :default [{:keys [op ks deps c]
                                      :or {deps #{}}
                                      :as node}]
  (let [*deps (cond-> (reduce into deps (map :deps c))
                (= :let op) (set/difference ks))]
    (assoc node :deps *deps)))

;;;

(defn- split-fragments [c scope]
  (let [{:keys [fragments]} @scope]
    (map (fn [child]
           (if-let [sym (get fragments child)]
             (do
               (vswap! scope mark-usage sym)
               `(~sym ~@(:deps child)))
             (let [sym (add-fragment! scope child)]
               `(~sym ~@(:deps child)))))
         c)))

(defmulti deconstruct (fn [_scope node] (:op node)))
(defmethod deconstruct :default [scope {:keys [c] :as node}]
  (assoc node :c (doall (split-fragments c scope))))
(defmethod deconstruct :terminal [_scope node]
  node)

;;;

(defmulti reconstruct (fn [_scope _sym node] (:op node)))
(defmethod reconstruct :default [{:keys [usage] :as scope} sym {:keys [c] :as node}]
  (loop [res []
         scope scope
         c c]
    (if (empty? c)
      (update scope :fragments assoc sym (assoc node :c (seq res)))
      (let [[sym :as child] (first c)
            fragment (get-in scope [:fragments sym])
            erase? (or (= 1 (get usage sym))
                       (< 24 (count (:deps fragment))))
            *child (if-not erase?
                     {:op :cps :b child}
                     fragment)]
        (recur (conj res *child)
               (cond-> scope erase? (update :fragments dissoc sym))
               (next c))))))
(defmethod reconstruct :terminal [scope _sym _node] scope)

(defn- reconstruct-all [{:keys [order fragments] :as scope}]
  (->> (sort-by second order)
       (map first)
       (reduce (fn [scope sym] (reconstruct scope sym (get fragments sym)))
               scope)
       :fragments
       (sort-by first)))

;;;

(defn tree->fragments [tree]
  (let [scope (volatile! (->scope))
        tree (->> (tree-zipper tree)
                  (postwalk accumulate-deps)
                  (postwalk (partial deconstruct scope))
                  (zip/node))
        root (add-fragment! scope tree)]
    (-> (flush @scope)
        (assoc :root root)
        (reconstruct-all))))

(defn process [{:keys [tree] :as spec} context]
  (assoc spec :fragments (if (:cps? context)
                           (tree->fragments tree)
                           [[(gensym "cps-") tree]])))
