(ns dev.repl
  (:refer-clojure :exclude [mod])
  (:require [one.yuyu.match.primitives.env :as e]
            [one.yuyu.match :as m]
            [one.yuyu.match.compile :as c]
            [one.yuyu.match.dynamic :as d]
            [one.yuyu.match.primitives.vars :as vars]
            [one.yuyu.match.predicates.zip :as z]
            [one.yuyu.match.context :as ctx]
            [clojure.core.match :as cm]
            [clojure.zip :as zip])
  (:import (clojure.lang IDeref)))

(defn *visit [f loc]
  (let [node (zip/node loc)
        *node (f node)]
    (cond-> loc (not= node *node) (zip/replace *node))))

(defn visit
  {:inputs {2 #{0}}}
  [env l r]
  (let [[_ f output] r
        [var env] (e/push-assign env `(*visit ~f ~l))]
    (e/push-queue env var output)))

(comment
  (-> '(m/fn F (z/down (id L)) true -> (recur F L true)
             F (visit F (z/right (id L))) _ -> (recur F L true)
             F (visit F (z/up (id L))) _ -> (recur F L false)
             F (visit F L) _ -> L)
      macroexpand)

  (-> '(m/fn postwalk
             (F L -> (postwalk F L true))
             (F (z/down (id L)) true -> (recur F L true)
              F (visit F (z/right (id L))) _ -> (recur F L true)
              F (visit F (z/up (id L))) _ -> (recur F L false)
              F (visit F L) _ -> L))
      macroexpand)

  nil)

;(defn mod [env l r]
;  (let [[_ divider output] r
;        [rem env] (e/push-assign env `(rem ~l ~divider))]
;    (-> env
;        (e/push-queue rem 0)
;        (e/push-queue l output))))

(defn date [env l r]
  (let [[_ Year Month Day] r
        [year env]  (e/push-assign env `(.getYear ^java.util.Date ~l))
        [month env] (e/push-assign env `(.getMonth ^java.util.Date ~l))
        [day env]   (e/push-assign env `(.getDay ^java.util.Date ~l))]
    (-> env
        (e/push-queue year Year)
        (e/push-queue month Month)
        (e/push-queue day Day))))

(defn lists [env l r]
  (let [[_ output] r]
    (cond-> (e/push-constraint env `(~'every? ~'list? ~l))
      output (e/push-queue l output))))

(comment
  (let [fail (Exception.)]
    (loop [counter 5]
      (if (<= counter 0)
        :done
        (if (identical? fail (try (recur (dec counter))))
          (recur (- counter 2))))))

  (let [f (m/fn (or :ok X) -> X)]
    (f nil)
    (f :a))

  ;(let [f (m/fn (date (mod 400 _) _ _) -> true
  ;              (date (mod 100 _) _ _) -> false
  ;              (date (mod 4   _) _ _) -> true
  ;              _                      -> false)])

  (-> '((date (mod 400 _) _ _) -> true
        (date (mod 100 _) _ _) -> false
        (date (mod 4 _) _ _)   -> true
        _                      -> false)
      c/compile)

  (let [f (m/fn (date Y (even M) D) -> [:even-month Y M D]
                (date Y M D)        -> [:date Y M D])]
    (f (java.util.Date. 2006 1 2))
    (f (java.util.Date. 2006 2 2)))

  (binding [d/*var-pattern* :question-mark]
    (let [f (m/fn (or :ok ?x) -> ?x)]
      (f nil)
      (f :a)))

  (binding [d/*var-pattern* :question-mark]
    (-> '([add [add ?x ?y] zero]         ->  [1 ?x ?y]
          [add [mul ?x ?y] zero]         ->  [2 ?x ?y]
          [add ?x          [mul ?y ?z]]  ->  [3 ?x ?y ?z]
          [add ?x          [add ?y ?z]]  ->  [4 ?x ?y ?z]
          [add ?x          zero]         ->  [5 ?x]
          [mul [mul ?x ?y] zero]         ->  [22 ?x ?y]
          [mul ?x          [mul ?y ?z]]  ->  [32 ?x ?y ?z]
          ?x                             ->  [6 ?x])
        c/compile))

  (-> '(([add [add ?x ?y] zero]         ->  [1 ?x ?y]
         [add [mul ?x ?y] zero]         ->  [2 ?x ?y]
         [add ?x          [mul ?y ?z]]  ->  [3 ?x ?y ?z]
         [add ?x          [add ?y ?z]]  ->  [4 ?x ?y ?z]
         [add ?x          zero]         ->  [5 ?x]
         [mul [mul ?x ?y] zero]         ->  [22 ?x ?y]
         [mul ?x          [mul ?y ?z]]  ->  [32 ?x ?y ?z]
         ?x                             ->  [6 ?x]))
      c/spec->fn)

  (-> '(([add [add ?x ?y] zero]         <-  [1 ?x ?y]
         [add [mul ?x ?y] zero]         <-  [2 ?x ?y]
         [add ?x          [mul ?y ?z]]  <-  [3 ?x ?y ?z]
         [add ?x          [add ?y ?z]]  <-  [4 ?x ?y ?z]
         [add ?x          zero]         <-  [5 ?x]
         [mul [mul ?x ?y] zero]         <-  [22 ?x ?y]
         [mul ?x          [mul ?y ?z]]  <-  [32 ?x ?y ?z]
         ?x                             <-  [6 ?x]))
      c/compile-all)

  (time
    (do
      (-> '(([add [add X Y] zero]       <-  [1 X Y]
             [add [mul X Y] zero]       <-  [2 X Y]
             [add X         [mul Y Z]]  <-  [3 X Y Z]
             [add X         [add Y Z]]  <-  [4 X Y Z]
             [add X         zero]       <-  [5 X]
             [mul [mul X Y] zero]       <-  [22 X Y]
             [mul X         [mul Y Z]]  <-  [32 X Y Z]
             X                          <-  [6 X]))
          c/compile-all)
      nil))

  (let [f (m/fn S 0 -> 0
                S 1 -> 1
                S N -> (+ (S S (- N 1)) (S S (- N 2))))]
    ;(f f 5)
    (f f 10))

  (let [f (m/fn fib (0 -> 0
                     1 -> 1
                     N -> (+ (fib (- N 1)) (fib (- N 2)))))]
    ;(f 5)
    (f 10))

  (-> '(((mod 3 (mod 5 X)) -> :fizzbuzz
         (mod 3 X) -> :fizz
         (mod 5 X) -> :buzz
         X -> X))
      c/spec->fn)

  (let [f (m/fn (mod 3 (mod 5 X)) -> :fizzbuzz
                (mod 3 X) -> :fizz
                (mod 5 X) -> :buzz
                X -> X)
        r (range 1 51)]
    (->> (map f r)
         (zipmap r)
         (into (sorted-map))))

  (-> '(fib (0 -> 0
             1 -> 1
             N -> (+ (fib (- N 1)) (fib (- N 2)))))
      c/spec->fn)

  (-> '(fib (0 -> 0
             1 -> 1
             N -> (+ (fib (- N 1)) (fib (- N 2)))))
      c/spec->fn)

  (-> '(S 0 -> 0
        S 1 -> 1
        S N -> (+ (S S (- N 1)) (S S (- N 2))))
      c/spec->fn)

  (-> '(S 0 -> 0
        S 1 -> 1
        S N -> (+ (S S (- N 1)) (S S (- N 2))))
      c/compile)

  (-> '((0 -> 0
         N -> (/ 1 N)))
      c/compile-all)

  (-> '([add [add X Y] zero]       <-  [1 X Y]
        [add [mul X Y] zero]       <-  [2 X Y]
        [add X         [mul Y Z]]  <-  [3 X Y Z]
        [add X         [add Y Z]]  <-  [4 X Y Z]
        [add X         zero]       <-  [5 X]
        [mul [mul X Y] zero]       <-  [22 X Y]
        [mul X         [mul Y Z]]  <-  [32 X Y Z]
        X                          <-  [6 X])
      c/compile)

  (let [ctx (ctx/->context :cps? true)]
    (-> '([add [add X Y] zero]       <-  [1 X Y]
          [add [mul X Y] zero]       <-  [2 X Y]
          [add X         [mul Y Z]]  <-  [3 X Y Z]
          [add X         [add Y Z]]  <-  [4 X Y Z]
          [add X         zero]       <-  [5 X]
          [mul [mul X Y] zero]       <-  [22 X Y]
          [mul X         [mul Y Z]]  <-  [32 X Y Z]
          X                          <-  [6 X])
        (c/compile ctx)
        (dissoc :env)
        (dissoc :tree)))
      ;(dissoc :sexpr))

  (let [ctx (ctx/->context :cps? true)]
    (-> '([add [add X Y] zero]       ->  [1 X Y]
          [add [mul X Y] zero]       ->  [2 X Y]
          [add X         [mul Y Z]]  ->  [3 X Y Z]
          [add X         [add Y Z]]  ->  [4 X Y Z]
          [add X         zero]       ->  [5 X]
          [mul [mul X Y] zero]       ->  [22 X Y]
          [mul X         [mul Y Z]]  ->  [32 X Y Z]
          X                          ->  [6 X])
        (c/compile ctx)
        (dissoc :env)
        ;(dissoc :tree)
        ;(dissoc :sexpr)
        :sexpr))

  (let [ctx (ctx/->context :cps? false)]
    (-> '([add [add X Y] zero]       ->  [1 X Y]
          [add [mul X Y] zero]       ->  [2 X Y]
          [add X         [mul Y Z]]  ->  [3 X Y Z]
          [add X         [add Y Z]]  ->  [4 X Y Z]
          [add X         zero]       ->  [5 X]
          [mul [mul X Y] zero]       ->  [22 X Y]
          [mul X         [mul Y Z]]  ->  [32 X Y Z]
          X                          ->  [6 X])
        (c/compile ctx)
        (dissoc :env)
        ;(dissoc :tree)
        (dissoc :sexpr)))


  (let [f (m/fn (mod (mod _ 5) 3) -> :fizzbuzz
                (mod _ 3) -> :fizz
                (mod _ 5) -> :buzz
                X -> X)
        r (range 1 16)]
    (into (sorted-map) (zipmap r (map f r))))

  (-> '(([add [add X Y] zero]       <-  [1 X Y]))
      c/compile-all)

  (-> '([add [add X Y] zero]       ->  [1 X Y]
        [add [mul X Y] zero]       ->  [2 X Y]
        [add X         [mul Y Z]]  ->  [3 X Y Z]
        [add X         [add Y Z]]  ->  [4 X Y Z]
        [add X         zero]       ->  [5 X]
        [mul [mul X Y] zero]       ->  [22 X Y]
        [mul X         [mul Y Z]]  ->  [32 X Y Z])
      ;X                          ->  [6 X]))
      c/compile)

  (-> '(([add [add X Y] zero]       ->  [1 X Y]
         X                          ->  [6 X]))
      c/compile-all
      first
      :sexpr)

  (-> '(([add [add X Y] zero]       <-  [1 X Y]
         [add X         [mul Y Z]]  <-  [3 X Y Z]
         X                          <-  [6 X]))
      c/compile-all)

  (-> '(([add [add X Y] zero]  ->  [1 X Y]
         [add [mul X Y] zero]  ->  [2 X Y]
         X                     ->  [6 X]))
      c/compile-all)

  (-> '([add [add X Y] zero]  ->  [1 X Y]
        [add [mul X Y] zero]  ->  [2 X Y]
        [add [mul X Y] zero]  ->  [3 X Y]
        X                     ->  [6 X])
      c/compile)

  (-> '([add [add X Y] zero] where (< 10 X)
                             -> [1 X Y]
        [add [mul X Y] zero] -> [2 X Y]
        [add [mul X Y] zero] -> [3 X Y]
        X                    -> [6 X])
      c/compile)


  (-> '(([add [add X Y] zero]  ->  [1 X Y]
         [add [mul X Y] zero]  ->  [2 X Y]
         ;[add [mul X Y] zero]  ->  [3 X Y]
         X                     ->  [6 X]))
      c/compile-all)


  (let [ctx (ctx/->context :cps? true)]
    (-> '([1 | X] true   ->  [true  1 X]
          [2 | X] true   ->  [true  2 X]
          X       X      ->  [6 X]
          [1 | X] false  ->  [false 3 X]
          [2 | X] false  ->  [false 4 X]
          [3 | X] V      ->  [V 5 X]
          X       X      ->  [6 X]
          X       Y      ->  [7 X Y])
        (c/compile ctx)))

  (-> '([1 | X] true   <-  [true  1 X]
        [2 | X] true   <-  [true  2 X]
        X       X      <-  [6 X]
        [1 | X] false  <-  [false 3 X]
        [2 | X] false  <-  [false 4 X]
        [3 | X] V      <-  [V 5 X]
        X       X      <-  [6 X]
        X       Y      <-  [7 X Y])
      c/compile)

  (let [ctx (ctx/->context :cps? true)]
    (-> '([1 | X] true   <-  [true  1 X]
          [2 | X] true   <-  [true  2 X]
          X       X      <-  [6 X]
          [1 | X] false  <-  [false 3 X]
          [2 | X] false  <-  [false 4 X]
          [3 | X] V      <-  [V 5 X]
          X       X      <-  [6 X]
          X       Y      <-  [7 X Y])
        (c/compile ctx)
        :sexpr))

  (-> '(([1 | X] true   ->  [true  1 X]
         X       X      ->  [6 X]
         X       Y      ->  [7 X Y]))
      c/compile-all)

  (-> '(([1 | X] true   ->  [true  1 X]
         [2 | X] true   ->  [true  2 X]
         [1 | X] false  ->  [false 3 X]
         ;[2 | X] false  ->  [false 4 X]
         [3 | X] V      ->  [V 5 X]
         X       X      ->  [6 X]
         X       Y      ->  [7 X Y]))
      (c/compile-all #{:seqable?}))

  (-> '(([1 | X] true   ->  [true  1 X]
         [2 | X] true   ->  [true  2 X]
         [1 | X] false  ->  [false 3 X]
         [3 | X] V      ->  [V 5 X]
         X       X      ->  [6 X]
         X       Y      ->  [7 X Y]))
      (c/compile #{:vector? :seq?}))

  (-> '(([1 | X] true   ->  [true  1 X]
         [2 | X] true   ->  [true  2 X]
         [1 | X] false  ->  [false 3 X]
         [3 | X] V      ->  [V 5 X]
         X       X      ->  [6 X]
         X       Y      ->  [7 X Y]))
      (c/spec->fn))

  (-> '(([1 | X] true   <-  [true  1 X]
         [2 | X] true   <-  [true  2 X]
         [1 | X] false  <-  [false 3 X]
         [3 | X] V      <-  [V 5 X]
         X       X      <-  [6 X]
         X       Y      <-  [7 X Y]))
      (c/spec->fn))

  (-> '(([(symbol N) | (lists S)] -> (cons N S)
         [(list H) | (lists S)] -> (cons nil (cons H S))
         S -> (list nil S)))
      (c/compile-all))

  (let [f ^:vector?
          (m/fn [1 | X] true   ->  [true  1 X]
                [2 | X] true   ->  [true  2 X]
                [1 | X] false  ->  [false 3 X]
                ;[2 | X] false  ->  [false 4 X]
                [3 | X] V      ->  [V 5 X]
                X       X      ->  [6 X]
                X       Y      ->  [7 X Y])]
    (reduce (fn [m [a b]] (assoc m [a b] (f a b)))
            {}
            [[:a :a]
             [:a :b]
             [true true]
             [nil nil]
             [() ()]
             [[1] [1]]]))

  (-> '(([1 | X] true   ->  [true  1 X]
         X       Y      ->  [7 X Y]))
      c/compile-all)

  (macroexpand '(m/fn (even A) -> [:even A]
                      X -> [:other X]))

  (m/fn (even A) -> [:even A]
        X -> [:other X])

  (time
    (do
      (-> '(([1 | X] true   ->  [true  1 X]
             [2 | X] true   ->  [true  2 X]
             X       X      ->  [6 X]
             [1 | X] false  ->  [false 3 X]
             [2 | X] false  ->  [false 4 X]
             [3 | X] V      ->  [V 5 X]
             ;X       X      ->  [6 X]
             X       Y      ->  [7 X Y]))
          c/compile-all)
      nil))

  ;(require '[one.yuyu.match :as m])

  (do
    (-> '(([1 | X] true   ->  [true  1 X]
           [2 | X] true   ->  [true  2 X]
           X       X      ->  [6 X]
           [1 | X] false  ->  [false 3 X]
           [2 | X] false  ->  [false 4 X]
           [3 | X] V      ->  [V 5 X]
           ;X       X      ->  [6 X]
           X       Y      ->  [7 X Y]))
        c/compile-all)
    nil)

  (do
    (-> '(([1 | X] true   ->  [true  1 X]
           [2 | X] true   ->  [true  2 X]
           X       X      ->  [6 X]
           [1 | X] false  ->  [false 3 X Y]
           [2 | X] false  ->  [false 4 X]
           [3 | X] V      ->  [V 5 X]
           ;X       X      ->  [6 X]
           X       Y      ->  [7 X Y]))
        c/compile-all)
    nil)

  (-> '((X X Y -> [7]))
      c/compile-all)

  (do
    (-> '((X Y X -> [7 X Y]))
        c/compile-all)
    nil)

  (-> '((X Y -> [7]))
      c/compile-all)

  (-> '((X Y -> [7]))
      c/compile-all)

  (-> '((X -> 4)
        (X Y -> [7]
         X Y -> [7]
         X Y -> [7]))
      c/spec->fn)

  (-> '((X -> 4)
        (X Y -> 7))
      c/spec->fn)

  (macroexpand '(m/fn A B C -> D))

  (c/spec->fn '((A -> A)
                (A A -> A)))

  (let [f (m/fn (X -> [4 X])
                (X Y -> [7 X Y]))]
    [(f :a) (f :b :c)])

  (let [f (m/fn (X -> 4)
                (X Y -> 7))]
    [(f :a) (f :b :c)])

  (let [f (m/fn X -> X)]
    [(f :a)
     (f :b :c)])

  (let [f (m/fn (mod (mod _ 5) 3) -> :fizzbuzz
                (mod _ 3) -> :fizz
                (mod _ 5) -> :buzz
                X -> X)
        r (range 15)]
    (zipmap r (map f r)))

  (fn* ([$$0]
        (cond
          (zero? (rem $$0 3)) (if (zero? (rem $$0 5))
                                :fizzbuzz
                                :fizz)
          (zero? (rem $$0 5)) :buzz
          :else $$0)))

  (-> '(m/fn (mod (mod _ 5) 3) -> :fizzbuzz
             (mod _ 3) -> :fizz
             (mod _ 5) -> :buzz
             X -> X)
      macroexpand)

  (doseq [n (range 1 101)]
    (println
      (m/match [(clojure.core/mod n 3) (clojure.core/mod n 5)]
               0 0 -> "FizzBuzz"
               0 _ -> "Fizz"
               _ 0 -> "Buzz"
               _ _ -> n)))

  (macroexpand '(m/fn ;(mod 3 (mod 5 X)) -> :fizzbuzz
                      ;(mod 3 X) -> :fizz
                      (mod 5 X) -> :buzz
                      X -> X))


  (let [f (m/fn self (F))])

  (-> '(1 true   ->  :true-1
        2 true   ->  :true-2
        1 false  ->  :false-1
        2 false  ->  :false-2
        2 false  ->  :false-2
        2 false  ->  :false-2
        3 V      ->  [:_-3 V]
        X Y      ->  [7 X Y])
      c/compile)

  (-> '((1 true   ->  :true-1
          2 true   ->  :true-2
          1 false  ->  :false-1
          2 false  ->  :false-2
          3 V      ->  [:_-3 V]
          X       Y      ->  [7 X Y]))
      c/spec->fn)

  (-> '(1 true   ->  :true-1
         2 true   ->  :true-2
         1 false  ->  :false-1
         2 false  ->  :false-2
         3 V      ->  [:_-3 V]
         X       Y      ->  [7 X Y])
      c/spec->fn)

  (-> '[1 true   ->  :true-1
        2 true   ->  :true-2
        1 false  ->  :false-1
        2 false  ->  :false-2
        3 V      ->  [:_-3 V]]
      ;X Y      ->  [7 X Y]
      ;_ _      ->  8]
      c/compile-all)

  (-> '[1 true   ->  :true-1]
      ;X Y      ->  [7 X Y]]
      ;_ _      ->  8]
      c/compile-all)

  (one.yuyu.match.compile/fail)

  (-> '[[1 | X] true   ->  [true  1 X]]
      c/compile-all)

  nil)

(defn contains
  {:inputs {1 #{0} 2 #{0}}}
  ([env l r]
   (let [arity (dec (count r))
         args (next r)
         env (e/push-constraint env (list 'contains? l (first args)))]
     (case arity
       1 env
       2 (e/push-queue env l (second args))))))

;(defn contains
;  {:signatures {1 '(+) 2 '(? ?)}}
;  ([env l [_ key output]]
;   (-> env
;       (e/push-constraint (list 'contains? l key) l key)
;       (e/push-queue l output))))

(comment
  (destructure '[[a b c d] (range 5)])
  (destructure '[[a b c d & es] (range 5)])

  (-> '[[A B] [B A] -> [A B]]
      c/compile-all)

  (-> '[[A B | _] [B A | _] -> [A B]]
      c/compile-all)

  (-> '[(contains [A] X) A -> [X A]]
      c/compile-all)

  (-> '[(contains Y X) (contains X Y) -> [X Y]]
      c/compile-all)

  (-> '[(contains Y X) Y -> [X Y]]
      c/compile-all)

  (-> '[Y (contains Y X) -> [X Y]]
      c/compile-all)

  (-> '[(id X) X -> X]
      c/compile-all)

  (-> '[(cons 1 (cons 2 (cons 3 X))) -> X]
      c/compile-all)

  (-> '[(cons Y X) -> X]
      c/compile-all)

  (-> '[(cons Y X) (= 1 Y) -> X]
      c/compile-all)

  (-> '[(= 1 Y) -> Y]
      c/compile-all)

  (macroexpand '(m/fn (mod 3 (mod 5 X)) -> :fizzbuzz
                      (mod 3 X) -> :fizz
                      (mod 5 X) -> :buzz
                      X -> X))

  (-> '(((zero X) -> [:zero X]
         (even (zero X)) -> [:even 1 X]
         (even (odd X)) -> [:even 2 X]
         (even X) <- [:even 3 X]
         (odd (even X)) -> [:odd X]
         ;(odd (even X))  -> [:odd X]
         X -> [:noop X]))
      c/compile-all)

  (-> '(((even (zero X)) -> [:even 1 X]
         (even (odd X)) -> [:even 2 X]
         (even X) -> [:even 3 X]
         ;(odd (even X))  -> [:odd X]
         _ -> :noop))
      c/compile-all)

  (-> '[(even (zero X)) -> [:even 1 X]
        (even X) -> [:even 3 X]
        _ -> :noop]
      c/compile-all)

  (-> '[(odd (even (zero X)))  -> [:odd X]
        _ -> :noop]
      c/compile-all)

  (-> '[(= 1 Y) -> Y
        _ -> :fallback]
      c/compile-all)

  (-> '[Y (= 1 2 Y Y) -> X]
      c/compile-all)

  (-> '[Y (= Y 1) -> X]
      c/compile-all)

  (-> '[(cons Y X) Y -> X]
      c/compile-all)

  (-> '(((cons 1 2 3 X) -> X))
      c/compile-all)

  (-> '(((satisfies Y X) Y -> [X Y]))
      c/compile-all)

  (-> '((Y (satisfies Y X) -> [X Y]))
      c/compile-all)

  (-> '((Y (contains Y X) -> [X Y]))
      c/compile-all)

  (let [f ^{:vector? true :seq/size true :cps true}
          (m/fn [:black [:red [:red _ _ _] _ _] _ _] -> :balance
                [:black [:red _ _ [:red _ _ _]] _ _] -> :balance
                [:black _ _ [:red [:red _ _ _] _ _]] -> :balance
                [:black _ _ [:red _ _ [:red _ _ _]]] -> :balance
                _                                    -> :balanced)]
    (time
      (dotimes [_ 10000]
        (doseq [[n v] (->> [[:black [:red [:red 1 2 3] 3 4] 5 6]
                            :balance
                            [:black [:red 1 2 [:red 3 4 5]] 6 7]
                            :balance
                            [:black 1 2 [:red [:red 3 4 5] 6 7]]
                            :balance
                            [:black 1 2 [:red 3 4 [:red 5 6 7]]]
                            :balance]
                           (partition 2))]
          (assert (= (f n) v))))))

  (let [f ^{:vector? true :seq/size true}
          (m/fn [:black [:red [:red _ _ _] _ _] _ _] -> :balance
                [:black [:red _ _ [:red _ _ _]] _ _] -> :balance
                [:black _ _ [:red [:red _ _ _] _ _]] -> :balance
                [:black _ _ [:red _ _ [:red _ _ _]]] -> :balance
                _                                    -> :balanced)]
    (time
      (dotimes [_ 10000]
        (doseq [[n v] (->> [[:black [:red [:red 1 2 3] 3 4] 5 6]
                            :balance
                            [:black [:red 1 2 [:red 3 4 5]] 6 7]
                            :balance
                            [:black 1 2 [:red [:red 3 4 5] 6 7]]
                            :balance
                            [:black 1 2 [:red 3 4 [:red 5 6 7]]]
                            :balance]
                           (partition 2))]
          (assert (= (f n) v))))))

  (time
    (dotimes [_ 10000]
      (doseq [[n v] (->> [[:black [:red [:red 1 2 3] 3 4] 5 6]
                          :balance
                          [:black [:red 1 2 [:red 3 4 5]] 6 7]
                          :balance
                          [:black 1 2 [:red [:red 3 4 5] 6 7]]
                          :balance
                          [:black 1 2 [:red 3 4 [:red 5 6 7]]]
                          :balance]
                         (partition 2))]
        (assert (= v (cm/match [n]
                               [(:or
                                  [:black [:red [:red _ _ _] _ _] _ _]
                                  [:black [:red _ _ [:red _ _ _]] _ _]
                                  [:black _ _ [:red [:red _ _ _] _ _]]
                                  [:black _ _ [:red _ _ [:red _ _ _]]])] :balance
                               :else :balanced))))))

  (let [ctx (ctx/->context :cps? true
                           :asserts #{:vector?})]
    (-> '([:black [:red [:red _ _ _] _ _] _ _] -> :balance
          [:black [:red _ _ [:red _ _ _]] _ _] -> :balance
          [:black _ _ [:red [:red _ _ _] _ _]] -> :balance
          [:black _ _ [:red _ _ [:red _ _ _]]] -> :balance
          _                                    -> :balanced)
        (c/compile ctx)))

  (let [ctx (ctx/->context :cps? true
                           :asserts #{:vector? :seq/size})]
    (-> '([:black [:red [:red _ _ _] _ _] _ _] -> :balance
          [:black [:red _ _ [:red _ _ _]] _ _] -> :balance
          [:black _ _ [:red [:red _ _ _] _ _]] -> :balance
          [:black _ _ [:red _ _ [:red _ _ _]]] -> :balance
          _                                    -> :balanced)
        (c/compile ctx)))

  (-> '(([:black [:red [:red _ _ _] _ _] _ _] -> :balance
         [:black [:red _ _ [:red _ _ _]] _ _] -> :balance
         [:black _ _ [:red [:red _ _ _] _ _]] -> :balance
         [:black _ _ [:red _ _ [:red _ _ _]]] -> :balance
         _                                    -> :balanced))
      c/compile-all)

  (macroexpand '(m/match [1 2 3]
                  (even A) B C -> [:even A B C]
                  (odd A) B C -> [:odd A B C]
                  _ _ _ -> :other))

  nil)

(comment
  (-> '(cm/match [n]
                 [(:or [:black [:red [:red a x b] y c] z d]
                    [:black [:red a x [:red b y c]] z d]
                    [:black a x [:red [:red b y c] z d]]
                    [:black a x [:red b y [:red c z d]]])] :balance
                 :else :balanced)
      macroexpand)

  nil)

(comment
  (-> '(m/fn [1 _ _] -> 1
             [2 _ _] -> 2
             [3 _ _] -> 3
             X       -> X)
      (with-meta {:seq/size true :short-vars? true})
      macroexpand)

  (-> '(m/fn [1 _ _] -> 1
             [2 _ _] -> 2
             [3 _ _] -> 3
             X       -> X)
      (with-meta {:cps true :seq/size true})
      macroexpand)

  (-> '(m/fn [1 A _] -> 1
             [2 A _] -> 2
             [3 A _] -> 3
             X       -> X)
      (with-meta {:cps true :seq/size true})
      macroexpand)

  (-> '(m/fn self
             (X F C -> (self F C (empty C)))
             (X F A []      -> A
              X F A [H | T] -> (recur F (F H) T)))
      (with-meta {:cps true})
      macroexpand)

  (-> '(m/fn self
             (F C -> (self F (empty C) C))
             (F A [] -> A
              F A [H | T] -> (recur F (cond-> A (F H) (conj H)) T)))
      macroexpand)


  (let [*reduce (m/fn F A [] -> A
                      F A [H | T] -> (recur F (F A H) T))]
    (*reduce + 0 (range 10)))

  (let [*filter (m/fn self
                      (F C -> (self F (empty C) C))
                      (F A [] -> A
                       F A [H | T] -> (recur F (cond-> A (F H) (conj H)) T)))]
    (*filter even? (vec (range 10))))

  (filter even? (range 10))
  (range 10)

  (let [f (m/fn self
                (0 -> 0
                 N -> (self 0 1 1 N))
                (A B N N -> B
                 A B I N -> (recur B (+ A B) (inc I) N)))
        r (range 10)]
    (zipmap r (map f r)))

  (let [f (m/fn self
                (0 -> 0
                 N -> (self 0 1 (dec N)))
                (A B 0 -> B
                 A B N -> (recur B (+ A B) (dec N))))
        r (range 10)]
    (zipmap r (map f r)))

  (let [tree [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]
        f (m/fn (end R) -> R
                (node L 310) -> (recur (zip/replace L ::replaced))
                (node L) -> (recur (zip/next L)))]
    (f (zip/vector-zip tree)))

  (macroexpand '(m/fn (z/end R) _ _ -> R
                      (z/node L K) {K V | M} (has S K) -> (recur (zip/replace L V) M (conj S K))
                      L M S -> (recur (zip/next L) M S)))

  (let [f (m/fn (z/end R) _ _ -> R
                (z/node K L) {K V | M} (has S K) -> (recur (zip/replace L V) M (conj S K))
                L M S -> (recur (zip/next L) M S))]
    (-> [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]
        zip/vector-zip
        (f {310 311} #{})))

  (let [f (m/fn (z/end R) _ _ -> R
                (z/node K L) {K V | M} (has S K) -> (recur (zip/replace L V) M (conj S K))
                L M S -> (recur (zip/next L) M S))]
    (-> [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]
        zip/vector-zip
        (f {310 311} #{})))

  (macroexpand '(m/fn (z/end R) _ -> R
                      (z/node K L) {K (id V) | M} -> (recur (zip/replace L V) M)
                      L M -> (recur (zip/next L) M)))

  (let [f (m/fn (z/end R) _ -> R
                (z/node K L) {K (id V) | M} -> (recur (zip/replace L V) M)
                L M -> (recur (zip/next L) M))]
    (-> [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]
        zip/vector-zip
        (f {310 0 32 1})))

  (macroexpand '(m/fn (z/end R) -> R
                      (z/node 310 L) -> (recur (zip/replace L ::replaced))
                      L -> (recur (zip/next L))))

  (macroexpand '(m/fn _ (z/end R) -> R
                      {N (id V) | M} (z/node N L) -> (recur M (zip/replace L V))
                      M L -> (recur M (zip/next L))))

  (macroexpand '(m/fn {K A} [K] -> [K A]
                      X _ -> X))

  (macroexpand '(m/fn self
                      (0 -> 0
                       1 -> 1
                       N -> (self 0 1 (dec N)))
                      (_ B 0 -> B
                       A B N -> (recur B (+ A B) (dec N)))))

  (macroexpand '(m/fn self
                      (0 -> 0
                       1 -> 1
                       N -> (self 0 1 N))
                      (A B N N -> B
                       A B I N -> (recur B (+ A B) (inc I) N))))


  (let [*map (m/fn self
                   (F C -> (self F (empty C) C))
                   (F A [] -> A
                    F A [H | T] -> (recur F (conj A (F H)) T)))]
    (*map #(* 2 %) (vec (range 10))))

  (let [*reduce (m/fn self
                      (F [] -> nil
                       F [H | T] -> (self F (F H) T))
                      (F A [] -> A
                       F A [H | T] -> (recur F (F A H) T)))]
    (*reduce + (range 10)))



  (macroexpand '(m/fn (id (zero))   -> [:zero]
                      (id (even X)) -> [:even X]
                      (id (odd X))  -> [:odd X]
                      X             -> [:dunno X]))

  (macroexpand ^:vector? '(m/?fn [add [add ?x ?y] zero]        -> [1 ?x ?y]
                                 [add [mul ?x ?y] zero]        -> [2 ?x ?y]
                                 [add ?x          [mul ?y ?z]] -> [3 ?x ?y ?z]
                                 [add ?x          [add ?y ?z]] -> [4 ?x ?y ?z]
                                 [add ?x          zero]        -> [5 ?x]
                                 ?x -> [6 ?x]))

  (macroexpand '(m/fn (id (zero))   -> [:zero]
                      (zid (even X)) -> [:even X]
                      (id (odd X))  -> [:odd X]
                      X             -> [:dunno X]))

  (let [ctx (ctx/->context ;:stop-on-error? false
              ;:show-problems? false
              ;:default-complementary true
              :default-mutually-exclusive false
              :cps? true)]
    (c/spec->fn '((id (zero (mod X 2))) -> [:zero]
                  (id (even (mod X 3))) -> [:even X]
                  (id (odd (mod X 4)))  -> [:odd X]
                  X                     -> [:dunno X])
                ctx))

  (macroexpand '(m/fn (instance IDeref X) -> @X
                      (null) -> ::nil
                      X -> X))

  (macroexpand '(m/fn (mod 3 (mod 5 _)) -> :fizzbuzz
                      (mod 3 _) -> :fizz
                      (mod 5 _) -> :buzz
                      X -> X))

  (let [f (m/fn (mod 3 (mod 5 _)) -> :fizzbuzz
                (mod 3 _) -> :fizz
                (mod 5 _) -> :buzz
                X -> X)
        r (range 30)]
    (zipmap r (map f r)))

  (macroexpand '(m/fn (z/end R) -> R
                      (z/node 310 (z/right (id L))) -> (recur (zip/replace L ::replaced))
                      L -> (recur (zip/next L))))

  (m/fn (z/end R) -> R
        (z/naode 310 (z/right (id L))) -> (recur (zip/replace L ::replaced))
        L -> (recur (zip/next L)))

  (macroexpand '(m/fn (mod (mod X 5) 3) -> :fizzbuzz
                      (mod X 3) -> :fizz
                      (mod X 5) -> :buzz
                      X -> X))


  (macroexpand '(m/fn A B -> 0))

  (macroexpand '(m/fn (or X Y) X Y -> [X Y]))

  (macroexpand '(m/fn (or [1 | T] X) -> [T X]
                      X -> [:fail X]))

  (macroexpand '(m/fn (or [1 | _] Y) X Y -> [X Y]))

  (macroexpand '(m/fn X Y -> [X Y]))

  nil)

;(fn*
;  ([$$0]
;   (let [$$-1 (clojure.core/rem $$0 3)
;         $$-2 (clojure.core/rem $$0 5)]
;     (cond
;      (clojure.core/zero? $$-1) (if (clojure.core/zero? $$-2)
;                                  :fizzbuzz
;                                  :fizz)
;      (clojure.core/zero? $$-2) :buzz
;      :else $$0))))

;(require '[one.yuyu.match.primitives.env :as e])
;(defn mod [env left right]
;  (let [arity (dec (count right))
;        [_ divider output] right
;        [rem env] (e/push-assign env `(rem ~left ~divider))]
;    (cond-> (e/push-constraint env `(zero? ~rem))
;      (= 2 arity) (e/push-queue left output))))

;(fn* ([$$0]
;      (cond
;        (instance? IDeref $$0) (clojure.core/deref $$0)
;        (nil? $$0) :dev.repl/nil
;        :else $$0)))

;(fn*
;  ([$$0]
;   (if $$0
;     (cond
;       (zero? $$0) [:zero]
;       (even? $$0) [:even $$0]
;       (odd? $$0) [:odd $$0]
;       :else [:dunno $$0])
;     [:dunno $$0])))

;(fn*
;  ([$$0]
;   (let [$$-1 (nth $$0 0)]
;     (if (= $$-1 1)
;       1
;       (let [$$-2 (nth $$0 1)]
;         (if (= $$-2 2)
;           2
;           (let [$$-3 (nth $$0 2)]
;             (if (= $$-3 3)
;               3
;               $$0))))))))

;(fn* ([$$0]
;      (let [$$-1 (first $$0)]
;        (case $$-1
;          1 (let [$$-2 (next $$0)] [1 $$-2])
;          2 (let [$$-2 (next $$0)] [2 $$-2])
;          $$0))))

(comment
  ;; filter
  ((m/fn self
         (F C -> (self F (empty C) C))
         (F A [] -> A
          F A [H | T] -> (recur F (cond-> A (F H) (conj H)) T)))
   even? (vec (range 10)))
  ;;=> [0 2 4 6 8]

  ;; map
  ((m/fn self
         (F C -> (self F (empty C) C))
         (F A [] -> A
          F A [H | T] -> (recur F (conj A (F H)) T)))
   #(* 2 %) (vec (range 5)))
  ;;=> [0 2 4 6 8]

  ;; reduce
  ((m/fn self
         (F [] -> nil
          F [H | T] -> (self F (F H) T))
         (F A [] -> A
          F A [H | T] -> (recur F (F A H) T)))
   + (range 10))
  ;;=> 45

  nil)

(comment
  (macroexpand '(m/fn (-> (xf))
                      (R -> (xf R))
                      (R I where (= I @prev) -> R
                       R I -> (do
                                (vreset! prev I)
                                (xf R I)))))

  nil)

(comment
  (->> [1 2 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 3 4 5 6 6 6 6 7]
       (into [] (dedupe)))

  nil)



(comment
  (macroexpand '(m/fn (mod (mod X 5) 3) -> [:fizzbuzz X]
                      (mod X 3) -> [:fizz X]
                      (mod X 5) -> [:buzz X]
                      X -> X))

  nil)
