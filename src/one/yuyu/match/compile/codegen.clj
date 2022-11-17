(ns one.yuyu.match.compile.codegen)

(defn ->scope [walk] walk)

(defn continue [scope node]
  (scope node scope))

;;;

(defn- escape [v] (if-not (symbol? v) v `(~'quote ~v)))

(defn- interleave-cases
  ([ks c scope] (interleave-cases ::remove ks c scope))
  ([fallback ks c scope]
   (let [c (map #(continue scope %) c)]
     (cond->> (interleave (conj (vec ks) fallback) c)
       (= fallback ::remove) (remove #(= % ::remove))))))

(defmulti -walk (fn [node _scope] (:op node)))

(defmethod -walk :terminal [{:keys [b]} _scope] b)

(defmethod -walk :backtrack [{:keys [c]} scope]
  `(~'try
     ~(continue scope (first c))
     (~'catch ~'Exception e#
       (~'if-not (one.yuyu.match.runtime/backtrack? e#)
         (~'throw e#)
         ~(continue scope (second c))))))

(defmethod -walk :cps [{:keys [b]} _scope] b)

(defmethod -walk :let [{:keys [b c]} scope]
  `(~'let [~@b] ~(continue scope (first c))))

(defn- make-if [test c scope]
  `(~'if ~test
     ~@(map #(continue scope %) c)))

(defn- make-case [{:keys [r ks ts c]} scope]
  (if (= 1 (count ks))
    (make-if (first ts) c scope)
    `(~'case ~r ~@(interleave-cases ks c scope))))

(defn- make-cond [{:keys [ts c]} scope]
  (if (= 1 (count ts))
    (make-if (first ts) c scope)
    `(~'cond ~@(interleave-cases :else ts c scope))))

(defmethod -walk :switch [{:keys [t] :as node} scope]
  (case t
    :case (make-case node scope)
    :cond (make-cond node scope)))

;;;

(defn- fragment->letfn [[sym tree]]
  `(~sym [~@(:deps tree)] ~(continue (->scope -walk) tree)))

(defn codegen [{:keys [fragments] :as spec} _]
  (let [fns (map fragment->letfn (butlast fragments))
        [_ tree] (last fragments)
        body (continue (->scope -walk) tree)]
    (assoc spec :sexpr (if (empty? fns)
                         body
                         `(letfn [~@fns] ~body)))))
