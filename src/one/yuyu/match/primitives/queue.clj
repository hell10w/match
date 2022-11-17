(ns one.yuyu.match.primitives.queue
  (:refer-clojure :rename {pop *pop}))

(defn ->queue [] (list clojure.lang.PersistentQueue/EMPTY))
(defn ->subq [v] (conj clojure.lang.PersistentQueue/EMPTY v))

(defn push
  ([queue v] (push queue v false))
  ([[fst & rst] v split?]
   (if split?
     (cons fst (cons (->subq v) rst))
     (cons (conj fst v) rst))))

(defn pop [[fst & rst :as q]]
  (cond
    (empty? q) (throw (Exception. "empty queue"))
    (empty? fst) (recur rst)
    :else [(peek fst) (cons (*pop fst) rst)]))

(defn done? [q]
  (every? empty? q))

(comment
  (letfn [(p [v]
            (clojure.pprint/pprint v)
            v)
          (pop-p [q]
            (let [[v q] (pop q)]
              (p v)
              q))]
    (-> (->queue)
        (push :a 1) p
        (push :a 2) p
        (push :a 3) p
        (push :a 4 1) p
        (push :a 5) p
        (push :a 6 1) p
        pop-p p
        pop-p p
        pop-p p
        pop-p p
        pop-p p
        pop-p p
        (push :a 1)
        pop-p p
        p))

  nil)
