(ns one.yuyu.match.predicates.seq
  (:require [one.yuyu.match.primitives.env :as e]))

(defn -ensure-seq [env l]
  (-> env
      (e/push-constraint (list 'seqable? l) :assert :seqable?)
      (e/push-constraint (list 'seq? l) :assert :seq?)
      (e/push-constraint (list 'vector? l) :assert :vector?)
      (e/push-constraint (list 'seq l) :assert :seq)))

(defn *seq+tail [env l r]
  (loop [env (-ensure-seq env l)
         l l
         r r]
    (let [arg (first r)]
      (case arg
        | (e/push-queue env l (last r))
        (let [[*first env] (e/push-assign env (list 'first l))
              [*next env] (e/push-assign env (list 'next l))]
          (recur (e/push-queue env *first arg)
                 *next
                 (next r)))))))

(defn *seq [env l r]
  (let [size (inc (count r))
        size-check `(~'== (~'bounded-count ~size ~l) ~size)]
    (as-> (-ensure-seq env l) $
          (e/push-constraint $ size-check :assert :seq/size)
          (reduce (fn [env [index arg]]
                    (let [[var env] (e/push-assign env (list 'nth l index))]
                      (e/push-queue env var arg)))
                  $
                  (map-indexed vector r)))))
