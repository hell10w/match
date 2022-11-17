(ns one.yuyu.match.compile.tree.merge
  (:refer-clojure :exclude [merge]))

(defn- merge [a b]
  (let [ensure-list (if (:m b) next list)
        test `(~'and ~(-> a :ts first) ~@(-> b :ts first ensure-list))]
    {:op :switch
     :t  :cond
     :ts (list test)
     :m  true
     :c  (:c b)}))

(defn- if? [{:keys [op ts]}]
  (and (= op :switch)
       (= 1 (count ts))))

(defn- unwind? [{:keys [c] :as node}]
  (and (if? node)
       (if? (first c))
       (= (second c) (second (:c (first c))))))

(defn unwind [node]
  (cond-> node
    (unwind? node) (merge (first (:c node)))))
