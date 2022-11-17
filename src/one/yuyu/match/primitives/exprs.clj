(ns one.yuyu.match.primitives.exprs
  (:refer-clojure :exclude [flush key test])
  (:require [clojure.set :as set]
            [one.yuyu.match.primitives.spec :as spec]
            [one.yuyu.match.primitives.analyze :as analyze]))

;;; entities

(defprotocol IExpression
  (root? [_])
  (root [_])
  (key [_])
  (test [_])
  (extract-deps [_])
  (similar? [_ other])
  (w1 [_])
  (w2 [_]))

(defrecord Equality [var val priority deps]
  IExpression
  (root? [_] true)
  (root [_] var)
  (key [_] val)
  (test [_] `(~'= ~var ~(spec/escape val)))
  (extract-deps [_] #{var})
  (similar? [_ other]
    (and (instance? Equality other)
         (= var (:var other))))
  (w1 [_] 0)
  (w2 [_] 0))

(defrecord Constraint [test mutually-exclusive? priority deps]
  IExpression
  (root? [_] false)
  (key [_] test)
  (test [_] test)
  (extract-deps [_] (analyze/search-deps test))
  (similar? [_ other]
    (and (instance? Constraint other)
         mutually-exclusive? (:mutually-exclusive? other)
         (<= priority (:priority other))
         (empty? (set/difference (:deps other) deps))))
  (w1 [_] 0)
  (w2 [_] 1))

(defrecord Body [body backtrack? clause priority deps]
  IExpression
  (root? [_] false)
  (key [_])
  (test [_])
  (extract-deps [_] (analyze/search-deps body))
  (similar? [_ _] false)
  (w1 [_] 1)
  (w2 [_] 2))

(defn ->constraint [test mutually-exclusive?]
  (map->Constraint {:test test :mutually-exclusive? mutually-exclusive?}))

(defn ->equality [var val]
  (map->Equality {:var var :val val}))

(defn ->body [body backtrack? index]
  (map->Body {:type :body
              :body body
              :clause index
              :backtrack? backtrack?}))

;;; building

(defn- -add [{:keys [item->id] :as exprs} expr]
  (let [id (count item->id)]
    (-> exprs
        (update :item->id assoc expr id)
        (update :register assoc id expr))))

(defn push [{:keys [item->id] :as exprs} expr]
  (if-let [id (get item->id expr)]
    [id exprs]
    (recur (-add exprs expr) expr)))

(defn flush [exprs]
  (:register exprs))

;;; accumulator

(defn ->exprs []
  {:item->id {}
   :register {}})

;;; sorting

(defn key-fn [{:keys [priority deps] :as expr}]
  [(w1 expr)
   priority
   (w2 expr)
   (- (count deps))])

;;; aggregation

(defn- -deps->max-priority [deps priorities fallback]
  (if (empty? deps)
    fallback
    (->> (select-keys priorities deps)
         vals
         (apply max))))

(defn provision-deps [exprs {:keys [priorities] :as _vars}]
  (if (empty? priorities)
    exprs
    (let [max-p (->> priorities vals (apply max))]
      (-> (fn [m [k expr]]
            (let [deps (extract-deps expr)]
              (update m k assoc
                      :deps deps
                      :priority (-deps->max-priority deps priorities max-p))))
          (reduce exprs exprs)))))
