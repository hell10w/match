## Algorithm 

Note: it's outdated a bit but not much after CPS have been added.

There is ~~four~~ five major stages:
1. normalization
2. unification
3. building a decision tree and AST
4. CPS
5. codegen

```
(-> '([1 | X] true   ->  [true  1 X]
      [2 | X] true   ->  [true  2 X]
      X       X      ->  [6 X]
      [1 | X] false  ->  [false 3 X]
      [2 | X] false  ->  [false 4 X]
      [3 | X] V      ->  [V 5 X]
      X       X      ->  [6 X]
      X       Y      ->  [7 X Y])
    one.yuyu.compile/compile)   
```


### normalization

Normalization is just splitting clauses by grammar. Normalized clauses from the example above:

```
{:arity 2,
 :clauses ({:head ([1 | X] true), :mode ->, :body [true 1 X], :index 0}
           {:head ([2 | X] true), :mode ->, :body [true 2 X], :index 1}
           {:head (X X), :mode ->, :body [6 X], :index 2}
           {:head ([1 | X] false), :mode ->, :body [false 3 X], :index 3}
           {:head ([2 | X] false), :mode ->, :body [false 4 X], :index 4}
           {:head ([3 | X] V), :mode ->, :body [V 5 X], :index 5}
           {:head (X X), :mode ->, :body [6 X], :index 6}
           {:head (X Y), :mode ->, :body [7 X Y], :index 7}
           {:head (_ _), :mode ->, :body (one.yuyu.match/fail), :auto? true, :index 8})}
```

Arity of clauses is 2.

The last clause (`_ _ -> (one.yuyu.match/fail)`) appears only in a normalised schema because it is an automatically added fallback, despite clauses already contain fallback (`X Y ->  [7 X Y]`).

### unification

Note: It needs to distinguish between input variables (which aren't defined directly) of compiled function and clause variables (logic variables) which need to infer and check. 

The most important part of compilation is unification of implicitly implied input variables and clauses arguments for every clause separately.

Queue of unification consists of left-right pairs. It starts with `<input-var1>=<clause-arg1>` and so on. By unifying pairs one by one it could emit additional pairs to queue. Process terminates when queue become empty.

There are [finite set of expressions](../src/one/yuyu/match/compile/unify.clj) which unification could handle:
1. `<var>=_` wildcard on one side. the equation will be rejected 
1. `_=<var>` same as previous
1. `<var>=()` will be rewritten to `<var>=(empty _)`
1. `<var>={}` same as previous
1. `<var>=(...)` unification will be handled by predicate 
1. `<var>=[...| <var>]` will be interpreted as a seq destructuring
1. `<var>=[...]` same as previous
1. `<var>={...}` will interpreted as a map destructuring
1. `<var>=<const literal>` adds a constraint for `<var>` as `(= <var> <value>)`

Most of the job supposed to be done by predicates. They take both sides of unification (left - input, and right - a list which contains predicate name and its arguments) and realise actual semantic: emit some bindings, constraints or other unification pairs. 

Naming of intermediate variables isn't controlled directly and by that it's guaranteed the same derivation with the same input gives the same variable name. So if there are repetitions of computation and/or constraints in different clauses they're going merge easily in decision tree as much as possible.

For every clause the unification gives a system of bindings (part of them are input ones, part of them are explicitly mentioned in the head or the body as logic variables and some amount of intermediate ones used in derivation process) and set of constrains bounded to particular variables.

In the example above most clauses share same structure. 

For example a sequence `[_ | X]`. It needs to split first value and compare it to 1, 2, 3. Also it needs to bind the tail to variable `X`.

Second argument in the example is boolean in several clauses and needs to compare to `false` and `true`. 

Also the example has a clause with same variable in both arguments:

`X X ->  [6 X]`

If same variable appears more than once it means it should have same value in all places.


#### process

Unification starts with first clause and these equations:
* `$$0 = [1 | X]` 
* `$$1 = true`

First equation emits
* binding `$$-1` with value `(first $$0)`
* binding `$$-2` with value `(next $$0)`
* unification `$$-1 = 1`

After first unification queue still contains two pairs 
* `$$1 = true`
* `$$-1 = 1`

Both contain a constant literals on right side and will emit a simple constraints on already existed variables without emitting new bindings or unification pairs.

Process continues for second clause and so on.

Overall unification result:

```
:env {:vars {:roots {0 $$0, 1 $$1},
             :register {$$-1 (first $$0), $$-2 (next $$0)},
             :deps {$$-1 #{$$0}, $$-2 #{$$0}},
             :rdeps {$$0 #{$$-2 $$-1}},
             :priorities {$$0 0, $$-2 1, $$-1 1, $$1 0}},
      :inputs ($$0 $$1),
      :exprs {0 {:type :equality, :var $$1, :val true, :deps #{$$1}, :priority 0},
              7 {:type :equality, :var $$1, :val false, :deps #{$$1}, :priority 0},
              1 {:type :equality, :var $$-1, :val 1, :deps #{$$-1}, :priority 1},
              4 {:type :body, :body [true 2 $$-2], :clause 1, :deps #{$$-2}, :priority 1},
              13 {:type :body, :body [7 $$0 $$1], :clause 7, :deps #{$$1 $$0}, :priority 0},
              6 {:type :body, :body [6 $$0], :clause 2, :deps #{$$0}, :priority 0},
              3 {:type :equality, :var $$-1, :val 2, :deps #{$$-1}, :priority 1},
              12 {:type :body, :body [6 $$0], :clause 6, :deps #{$$0}, :priority 0},
              2 {:type :body, :body [true 1 $$-2], :clause 0, :deps #{$$-2}, :priority 1},
              11 {:type :body, :body [$$1 5 $$-2], :clause 5, :deps #{$$-2 $$1}, :priority 1},
              9 {:type :body, :body [false 4 $$-2], :clause 4, :deps #{$$-2}, :priority 1},
              5 {:type :constraint, :test (= $$1 $$0), :mutually-exclusive? true, :deps #{$$1 $$0}, :priority 0},
              14 {:type :body, :body (one.yuyu.match/fail), :clause 8, :deps #{}, :priority 1},
              10 {:type :equality, :var $$-1, :val 3, :deps #{$$-1}, :priority 1},
              8 {:type :body, :body [false 3 $$-2], :clause 3, :deps #{$$-2}, :priority 1}},
      :clauses [[0 1 2] [0 3 4] [5 6] [7 1 8] [7 3 9] [10 11] [5 12] [13] [14]]},
```

#### [instantiation](https://www.swi-prolog.org/pldoc/man?section=modes)

Predicate's arguments could be considered as an outputs most of the time. Also predicate always have its implicit input - one of previously inferred variables - left side of the unification pair. However, there are potential cases when particular predicate argument have to be an input.

For example a checking of value presence in a collection:

`(contains Map Key)`

The predicate `contains/2` will emit a constraint expressed with clojure form `(contains? $$input Key)`, and pass a new pair `$$input=Map` to unification queue so `Map` could be deconstructed and checked furtherer, but because of imperative  semantic of clojure's form (it requires computation at runtime, so it's not pure logic) `Key` should have some "ground" value: 
* constant, 
* previously inferred variable,
* clojure variable from outer scope or 
* logic variable which occurs in other arguments of the same clause.

So, in fact the `Key` is always an input in the `contains` predicate. 

Overall obligations for particular variable could be classified as [instantiation pattern](https://www.swi-prolog.org/pldoc/man?section=modes). In prolog there are plenty of them, but here are only two:
* an inputs (counterpart of prolog's `+` ), 
* and an outputs (`--`).

Algorithm of instantiation ensures all inputs are grounded and detects circular dependencies which considered as fatal errors. It happens at compile-time.


### decision tree and AST

From previous step there are "plans" for all clauses. Each plan consists of "expressions", basic operations which need to be done in sequence.

```
:clauses [[0 1 2] [0 3 4] [5 6] [7 1 8] [7 3 9] [10 11] [5 12] [13] [14]],
```

For example first clause `[0 1 2]`:

* `0 {:type :equality, :var $$1, :val true, :deps #{$$1}, :priority 0},`
* `1 {:type :equality, :var $$-1, :val 1, :deps #{$$-1}, :priority 1},`
* `2 {:type :body, :body [true 1 $$-2], :clause 0, :deps #{$$-2}, :priority 1},`

It could be read as "if `$$1` equal to `true` and `$$-1` equal to `1` then execute a body `[true 1 $$-2]`.  If something fails it needs go to second clause and its plan.". 

So building a decision tree is a recursive process of 1) taking the first expression of the first clause, 2) building a `switch` with trying to accommodate as leafs as much of other clauses as possible by comparing them with (1) and 3) use the rest of clauses as "fallbacks" in every leafs of current switch and as fallback of `switch` itself.

If current (1) is terminal expression (type is `:body`) then other remaining clauses is ignored except current clause marked as backtracked. 

All comparations always happen to first expressions of clauses. It helps preserve best-first ordering with respect to depth of assignments used in expressions.

Expressions used to build `switch` need to have same type and share variables on which they operate on.


Building a `switch` for `:equality` is quite definite: just collecting other values as branch keys. If some variable have particular value it wouldn't need to backtrack to other branch ever - it wouldn't be equal anyway.

For `:constraint` it's not so. By default, constraints which satisfy to other restrictions (type, vars sharing, deepness of variables) and used as branches will not be cloned as backtracks to previous ones. 


AST for example above:


```
:tree {:op :switch,
       :t :equality,
       :ts ((= $$1 true) (= $$1 false)),
       :c ({:op :let,
            :b ($$-1 (first $$0)),
            :c {:op :switch,
                :t :equality,
                :ts ((= $$-1 1) (= $$-1 2)),
                :c ({:op :let, :b ($$-2 (next $$0)), :c {:op :terminal, :b [true 1 $$-2]}}
                    {:op :let, :b ($$-2 (next $$0)), :c {:op :terminal, :b [true 2 $$-2]}}
                    {:op :switch,
                     :t :constraint,
                     :ts ((= $$1 $$0)),
                     :c ({:op :terminal, :b [6 $$0]}
                         {:op :switch,
                          :t :equality,
                          :ts ((= $$-1 3)),
                          :c ({:op :let, :b ($$-2 (next $$0)), :c {:op :terminal, :b [$$1 5 $$-2]}}
                              {:op :terminal, :b [7 $$0 $$1]}),
                          :r $$-1,
                          :ks (3)})}),
                :r $$-1,
                :ks (1 2)}}
           {:op :let,
            :b ($$-1 (first $$0)),
            :c {:op :switch,
                :t :equality,
                :ts ((= $$-1 1) (= $$-1 2)),
                :c ({:op :let, :b ($$-2 (next $$0)), :c {:op :terminal, :b [false 3 $$-2]}}
                    {:op :let, :b ($$-2 (next $$0)), :c {:op :terminal, :b [false 4 $$-2]}}
                    {:op :switch,
                     :t :constraint,
                     :ts ((= $$1 $$0)),
                     :c ({:op :terminal, :b [6 $$0]}
                         {:op :switch,
                          :t :equality,
                          :ts ((= $$-1 3)),
                          :c ({:op :let, :b ($$-2 (next $$0)), :c {:op :terminal, :b [$$1 5 $$-2]}}
                              {:op :terminal, :b [7 $$0 $$1]}),
                          :r $$-1,
                          :ks (3)})}),
                :r $$-1,
                :ks (1 2)}}
           {:op :switch,
            :t :constraint,
            :ts ((= $$1 $$0)),
            :c ({:op :terminal, :b [6 $$0]}
                {:op :let,
                 :b ($$-1 (first $$0)),
                 :c {:op :switch,
                     :t :equality,
                     :ts ((= $$-1 3)),
                     :c ({:op :let, :b ($$-2 (next $$0)), :c {:op :terminal, :b [$$1 5 $$-2]}}
                         {:op :terminal, :b [7 $$0 $$1]}),
                     :r $$-1,
                     :ks (3)}})}),
       :r $$1,
       :ks (true false)}
```

### CPS

TODO

### codegen

Final code is generated directly out of AST without any transformations.

```
:sexpr (case $$1
        true (let [$$-1 (first $$0)]
               (case $$-1
                  1 (let [$$-2 (next $$0)] [true 1 $$-2])
                  2 (let [$$-2 (next $$0)] [true 2 $$-2])
                  (if (= $$1 $$0)
                    [6 $$0]
                    (if (= $$-1 3)
                      (let [$$-2 (next $$0)] [$$1 5 $$-2]) 
                      [7 $$0 $$1]))))
        false (let [$$-1 (first $$0)]
                (case $$-1
                   1 (let [$$-2 (next $$0)] [false 3 $$-2])
                   2 (let [$$-2 (next $$0)] [false 4 $$-2])
                   (if (= $$1 $$0)
                     [6 $$0]
                     (if (= $$-1 3) 
                       (let [$$-2 (next $$0)] [$$1 5 $$-2])
                       [7 $$0 $$1]))))
        (if (= $$1 $$0)
          [6 $$0]
          (let [$$-1 (first $$0)]
            (if (= $$-1 3)
              (let [$$-2 (next $$0)] [$$1 5 $$-2])
              [7 $$0 $$1]))))}
```
