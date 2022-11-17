(ns one.yuyu.match.primitives.var
  (:refer-clojure :exclude [var?]))

(defn var? [var] (-> var meta ::var?))

(defn ->var [index base root?]
  (let [m (cond-> {::var? true}
            root? (assoc ::column index))]
    (-> (symbol (str base index))
        (with-meta m))))
