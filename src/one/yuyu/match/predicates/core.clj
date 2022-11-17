(ns one.yuyu.match.predicates.core
  (:require [one.yuyu.match.primitives.env :as e]
            [one.yuyu.match.predicates.ins :as ins]))

;;; errors

(defn- bad-arity [env l r]
  (e/push-error env :predicate/wrong-arity :lr [l r]))

(defn- unknown-predicate [env l r]
  (e/push-error env :predicate/unknown :lr [l r]))

;;; builtins

(defn *or
  {:input? (fn [_arg index] (< 0 index))}
  ([env l r]
   (let [arity (dec (count r))]
     (case arity
       2 (let [[_ x & vals] r
               [*ret env] (e/push-assign env (list* 'or l vals))]
           (e/push-queue env *ret x))
       (bad-arity env l r)))))

(defn *id [env l r]
  (let [arity (dec (count r))
        env (e/push-constraint env l)]
    (case arity
      0 env
      1 (e/push-queue env l (second r))
      (bad-arity env l r))))

(defn *copy [env l r]
  (reduce (fn [env arg] (e/push-queue env l arg))
          env
          (next r)))

(defn *mod
  {:inputs {1 #{0} 2 #{1}}}
  ([env l r]
   (let [arity (dec (count r))]
     (case arity
       1 (let [[_ divider] r]
           (e/push-constraint env `(~'zero? (~'rem ~l ~divider))))
       2 (let [[_ output divider] r]
           (-> env
               (e/push-constraint `(~'zero? (~'rem ~l ~divider)))
               (e/push-queue l output)))
       (bad-arity env l r)))))

(defn mk-pred-1 [sym]
  (fn [env l r]
    (let [arity (dec (count r))
          env (e/push-constraint env (list sym l))]
      (case arity
        0 env
        1 (e/push-queue env l (second r))
        (bad-arity env l r)))))

(defn mk-pred-2
  ([sym] (mk-pred-2 sym false))
  ([sym reverse?]
   ^{:inputs {1 #{0} 2 #{0}}}
   (fn [env l r]
     (let [arity (dec (count r))
           form (if reverse?
                  (list sym l (second r))
                  (list sym (second r) l))
           env (e/push-constraint env form)]
       (case arity
         1 env
         2 (e/push-queue env l (nth r 2))
         (bad-arity env l r))))))

(defn mk-pred-2r [sym]
  ^{:inputs {1 #{0} 2 #{1}}}
  (fn [env l r]
    (let [arity (dec (count r))
          form (list sym l (last r))
          env (e/push-constraint env form)]
      (case arity
        1 env
        2 (e/push-queue env l (second r))
        (bad-arity env l r)))))

(comment
  ; todo ?
  #{distinct? every? false? fits-table? ident?
    identical? indexed? isa? nat-int? neg-int?
    nil? pos-int? reader-conditional? reduced?
    reversible? sorted? tagged-literal?
    thread-bound? true?})

(def builtins
  {'= #'*copy
   'id #'*id
   'or #'*or
   'mod #'*mod
   'associative (mk-pred-1 'associative?)
   'boolean (mk-pred-1 'boolean?)
   'bytes (mk-pred-1 'bytes?)
   'class (mk-pred-1 'class?)
   'coll (mk-pred-1 'coll?)
   'counted (mk-pred-1 'counted?)
   'decimal (mk-pred-1 'decimal?)
   'delay (mk-pred-1 'delay?)
   'double (mk-pred-1 'double?)
   'empty (mk-pred-1 'empty?)
   'even (mk-pred-1 'even?)
   'float (mk-pred-1 'float?)
   'fn (mk-pred-1 'fn?)
   'future (mk-pred-1 'future?)
   'future-cancelled (mk-pred-1 'future-cancelled?)
   'future-done (mk-pred-1 'future-done?)
   'ifn (mk-pred-1 'ifn?)
   'inst (mk-pred-1 'inst?)
   'int (mk-pred-1 'int?)
   'integer (mk-pred-1 'integer?)
   'keyword (mk-pred-1 'keyword?)
   'list (mk-pred-1 'list?)
   'map-entry (mk-pred-1 'map-entry?)
   'neg (mk-pred-1 'neg?)
   'null (mk-pred-1 'nil?)
   'number (mk-pred-1 'number?)
   'odd (mk-pred-1 'odd?)
   'pos (mk-pred-1 'pos?)
   'qualified-ident (mk-pred-1 'qualified-ident?)
   'qualified-keyword (mk-pred-1 'qualified-keyword?)
   'qualified-symbol (mk-pred-1 'qualified-symbol?)
   'ratio (mk-pred-1 'ratio?)
   'rational (mk-pred-1 'rational?)
   'realized (mk-pred-1 'realized?)
   'seq (mk-pred-1 'seq?)
   'seqable (mk-pred-1 'seqable?)
   'sequential (mk-pred-1 'sequential?)
   'set (mk-pred-1 'set?)
   'simple-ident (mk-pred-1 'simple-ident?)
   'simple-keyword (mk-pred-1 'simple-keyword?)
   'simple-symbol (mk-pred-1 'simple-symbol?)
   'some (mk-pred-1 'some?)
   'special-symbol (mk-pred-1 'special-symbol?)
   'string (mk-pred-1 'string?)
   'symbol (mk-pred-1 'symbol?)
   'uri (mk-pred-1 'uri?)
   'uuid (mk-pred-1 'uuid?)
   'var (mk-pred-1 'var?)
   'volatile (mk-pred-1 'volatile?)
   'zero (mk-pred-1 'zero?)
   'has (mk-pred-2r 'contains?)
   'contains (mk-pred-2r 'contains?)
   'instance (mk-pred-2 'instance?)
   'satisfies (mk-pred-2 'satisfies?)})

;;; dispatch

(defn- var->ns [v] (-> v meta :ns))
(defn- custom? [v] (and v (not= (var->ns #'seq) (var->ns v))))

(defn- pred->inputs [pred arity]
  (let [{:keys [input? inputs]} (meta pred)
        inputs (get inputs arity)]
    (cond
      input? input?
      inputs (fn [_arg index] (get inputs index false))
      :else  (constantly false))))

(defn- resolve-pred [sym]
  (let [pred (resolve sym)]
    (cond
      (qualified-symbol? sym) pred
      (custom? pred)          pred
      :else                   (get builtins sym))))

(defn dispatch [env l r payload]
  (if-let [pred (some-> r first resolve-pred)]
    (->> (pred->inputs pred (dec (count r)))
         (ins/invoke env pred l r payload))
    (unknown-predicate env l r)))
