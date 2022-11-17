# one.yuyu.match

Pattern matching library.

```
(require '[one.yuyu.match :as m]

(let [f (m/fn (mod (mod _ 5) 3) -> :fizzbuzz
              (mod _ 3) -> :fizz
              (mod _ 5) -> :buzz
              X -> X)
      r (range 1 16)]
  (into (sorted-map) (zipmap r (map f r))))
=> {1 1, 2 2, 3 :fizz, 4 4, 5 :buzz, 6 :fizz, 7 7, 8 8, 9 :fizz, 
    10 :buzz, 11 11, 12 :fizz, 13 13, 14 14, 15 :fizzbuzz}
```

```
(-> '(m/fn (mod (mod _ 5) 3) -> :fizzbuzz
           (mod _ 3) -> :fizz
           (mod _ 5) -> :buzz
           X -> X)
    macroexpand) 
=> (fn* ([$$0] 
         (cond 
           (zero? (rem $$0 3)) (if (zero? (rem $$0 5))
                                 :fizzbuzz 
                                 :fizz)
           (zero? (rem $$0 5)) :buzz 
           :else $$0))) 
```

[More examples](docs/examples.md)

Features:
* allows complex nested destructuring and checks in simultaneous way
* automatic variables reconciling
* multi-arity
* clear API for predicates definition
* various optimization 
* optional controlled backtracking
* optional CPS-transformation to reduce a code duplication
* optional constraints
* detailed compilation errors

Drawbacks and pitfalls:
* :bangbang: backtracking or CPS-transformation cause a broken recursion without any notice 
* predicates take part of control and do some decision-making at compile-time, it's not plain boolean functions
* :heavy_exclamation_mark: tries to generate as simple code as possible what's may not be what is desired
* :heavy_exclamation_mark: all predicates are mutual exclusive by default (it reduces backtracks amount and code duplication)
* :heavy_exclamation_mark: type checks are omitted by default 


----


1. [API](#API)
2. [Algorithm](#Algorithm)
3. [Destructuring](#Destructuring)
   1. [Sequences](#Sequences)
   2. [Maps](#Maps)
4. [Predicates](#Predicates)
   1. [Builtins](#Builtins)
   1. [Zippers](#zip)
   2. [Custom](#Custom)


----

### API <a name="API"></a>

```
(require '[one.yuyu.match :as m]
```

```
(defmacro match [input & spec]) 
(defmacro fn [& spec]) 
(defmacro defn [name & spec])

(defmacro ?match [input & spec]) 
(defmacro ?fn [& spec]) 
(defmacro ?defn [name & spec]) 

(defn fail [])
(defn backtrack? []) 
```

```
spec   := name (clause+)+
spec   := clause+
name   := SYMBOL
clause := head mode body
clause := head mode 'where guard body
head   := arg*
mode   := '-> | '<-
guard  := SEXPR
arg    := SEXPR
body   := SEXPR 
```

The only difference between `match`/`fn`/`defn` and `?match`/`?fn`/`?defn` is regexp to distinguish a logic variables. First group uses `#"^[A-Z].*"`, second one - `#"^\?.*"`.


### Algorithm <a name="Algorithm"></a>

[Here](docs/algorithm.md) 


### Destructuring <a name="Destructuring"></a>

Uses non-conventional way to highlight a tail with `|`.

The major drawback of destructuring is absence of way to define `not-found` fallback for `clojure.core/nth` and `clojure.core/get`. 

Similar behaviour could be emulated with `or` and `id` predicates, but it's not the same.


#### Sequences <a name="Sequences"></a>

* `[Expr ...]` destructs the input with `clojure.core/nth`.
* `[Expr ... | Var]` destructs the input with `clojure.core/first` / `clojure.core/next`

Optional asserts:
* `:seqable?` ~ `(seqable? $$input)`
* `:seq?` ~ `(seq? $$input)`
* `:vector?` ~ `(vector? $$input)`
* `:seq` ~ `(seq $$input)`
* `:seq/size` ~ `(== (bounded-count expected-size $$input) expected-size)`

```
(macroexpand '(m/fn [1 | X] -> [1 X]
                    [2 | X] -> [2 X]
                    X       -> X))
=> (fn* ([$$0]
         (let [$$-1 (first $$0)]
           (case $$-1
             1 (let [$$-2 (next $$0)] [1 $$-2])
             2 (let [$$-2 (next $$0)] [2 $$-2])
             $$0))))
```             

```             
(macroexpand '(m/fn [1 _ _] -> 1
                    [_ 2 _] -> 2
                    [_ _ 3] -> 3
                    X       -> X))
=> (fn* 
     ([$$0]
      (let [$$-1 (nth $$0 0)]
        (if (= $$-1 1)
          1
          (let [$$-2 (nth $$0 1)]
            (if (= $$-2 2)
              2
              (let [$$-3 (nth $$0 2)]
                (if (= $$-3 3)
                  3 
                  $$0))))))))
                
```

#### Maps <a name="Maps"></a>

* `{Key Value ...}` 
* `{Key Value ... | Var]` 

Key have to be constant value, variable from outer scope or value which could be inferred from other argument.

Optional asserts:
* `:map?` ~ `(map? $$input)`
* `:seq` ~ `(seq $$input)`

### Predicates <a name="Predicates"></a>

There is no negation in any form and `or` pseudo-pattern in sense of [`clojure.core.match`](https://github.com/clojure/core.match). 

#### Builtins <a name="Builtins"></a>

Semantic of `=` and `or` are confused by their names. `=` works as logic AND: all arguments have to unify with the input. `or` introduces a new variable with `(clojure.core/or $$input ...<arguments-except-first>)` and unifies this variable with first argument.

Except `=`, `or`, `id`, `mod` all predicates are counterparts of `clojure.core` ones. They take zero or one argument. Corresponding constraint on the input is always emitted and if there is an argument it'll be unified with input.

* `=` 
* `or` 
* `id` 
* `mod` 
* `associative`
* `boolean`
* `bytes`
* `class`
* `coll`
* `counted`
* `decimal`
* `delay`
* `double`
* `empty`
* `even`
* `float`
* `fn`
* `future`
* `future-cancelled`
* `future-done`
* `ifn`
* `inst`
* `int`
* `integer`
* `keyword`
* `list`
* `map-entry`
* `neg`
* `null`
* `number`
* `odd`
* `pos`
* `qualified-ident`
* `qualified-keyword`
* `qualified-symbol`
* `ratio`
* `rational`
* `realized`
* `seq`
* `seqable`
* `sequential`
* `set`
* `simple-ident`
* `simple-keyword`
* `simple-symbol`
* `some`
* `special-symbol`
* `string`
* `symbol`
* `uri`
* `uuid`
* `var`
* `volatile`
* `zero` 
* `(contains +key)`
* `(contains -output +key)`
* `(instance +Class)`
* `(instance +Class -output)`
* `(satisfies +Protocol)`
* `(satisfies +Protocol -output)`

```
(require '[one.yuyu.match :as m])
(macroexpand '(m/fn (id (zero))   -> [:zero]
                    (id (even X)) -> [:even X]
                    (id (odd X))  -> [:odd X]
                    X             -> [:never X]))
=> (fn*
     ([$$0]
      (if $$0                      ;; (id _)
        (cond 
          (zero? $$0) [:zero]      ;; (zero _)
          (even? $$0) [:even $$0]  ;; (even _)
          (odd? $$0) [:odd $$0]    ;; (odd _)
          :else [:never $$0]) 
        [:never $$0])))
```

```
(require '[one.yuyu.match :as m])
(macroexpand '(m/fn (instance IDeref X) -> @X
                    (null) -> ::nil
                    X -> X))
=> (fn* ([$$0] 
         (cond 
           (instance? IDeref $$0) (clojure.core/deref $$0)
           (nil? $$0) :dev.repl/nil 
           :else $$0)))
                    
```

#### Zippers <a name="zip"></a>

```
(require '[one.yuyu.match.predicates.zip :as z]
```

* `z/end` checks input with `clojure.zip/end?` and binds optional output to `(clojure.zip/root $$input)` 
* `z/node` `(z/node -Value)` `(z/node -Value -Loc)`
* `z/loc` `(z/loc -Loc)` `(z/loc -Loc -Value)`
* `z/root` binds output to `(clojure.zip/root $$input)` 
* `z/next` 
* `z/prev`
* `z/up`
* `z/down`
* `z/left`
* `z/right`
* `z/rightmost`
* `z/leftmost`

If any of moving predicate (`next`, `prev`, `up`, `down`, `left`, `right`, `rightmost`, `leftmost`)  used without argument it emits a constraint to ensure moved location exists. 

```
(macroexpand '(m/fn (z/end R) -> R
                    (z/node 310 L) -> (recur (zip/replace L ::replaced))
                    L -> (recur (zip/next L)))) 
=>
(fn*
 ([$$0]
  (if
   (clojure.zip/end? $$0)
   (let [$$-1 (clojure.zip/root $$0)] $$-1)
   (let
    [$$-2 (clojure.zip/node $$0)]
    (if (= $$-2 310) (recur (zip/replace $$0 :dev.repl/replaced)) (recur (zip/next $$0)))))))                    
```

#### Custom <a name="Custom"></a>

Predicate is a function with three arguments `[env l r]`. `env` is a current unification environment, `l` is input variable, and `r` is overall predicate form.

Predicate may 
1. derive new variables by using the input variable
1. add a constraints on the input variable or any variable the predicate have introduced
1. put a unification pairs to the queue


The implementation of builtin predicate `mod`:

``` 
(require '[one.yuyu.match.primitives.env :as e]) 
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
```      

```
(mod/1 +divider)
(mod/2 -value +divider)
```
