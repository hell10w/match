(ns one.yuyu.match.core-test
  (:refer-clojure :exclude [mod])
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [clojure.pprint]
            [one.yuyu.match :as m]
            [one.yuyu.match.primitives.env :as e]
            [one.yuyu.match.predicates.zip :as z]
            [one.yuyu.match.compile.cps :as cps]
            [one.yuyu.match.compile :as c]
            [one.yuyu.match.context :as ctx])
  (:import (clojure.lang IDeref)))

(defn evals-to-true [env l r]
  (let [env (e/push-constraint env l)]
    (case (count r)
      1 env
      2 (e/push-queue env l (second r)))))

(defn date
  "Unpack java.util.Date to date components
  (date/3 -Year -Month -Day)"
  [env l r]
  (let [[_ Year Month Day] r
        [year env]  (e/push-assign env `(.getYear ^java.util.Date ~l))
        [month env] (e/push-assign env `(.getMonth ^java.util.Date ~l))
        [day env]   (e/push-assign env `(.getDate ^java.util.Date ~l))]
    (-> env
        (e/push-queue year Year)
        (e/push-queue month Month)
        (e/push-queue day Day))))

(deftest map-test
  (testing "simple destructuring"
    (let [f (m/fn {:a A :b B | R} -> [A B R])]
      (is (= (f {:a 0 :b 1 :c 2})
             [0 1 {:a 0 :b 1 :c 2}]))
      (is (= (f {:b 1 :c 2})
             [nil 1 {:b 1 :c 2}])))

    (let [f (m/fn {:a (id A) :b (id B) | R} -> [A B R]
                  X -> X)]
      (is (= (f {:a 0 :b 1 :c 2})
             [0 1 {:a 0 :b 1 :c 2}]))
      (is (= (f {:b 1 :c 2})
             {:b 1 :c 2}))))

  (testing "destructuring with constraints"
    (let [f (m/fn {:a (even A) :b B | R} -> [:even A B R]
                  {:a (odd A) :b B | R} -> [:odd A B R]
                  X -> X)]
      (is (= (f {:a 0 :b 1 :c 2})
             [:even 0 1 {:a 0 :b 1 :c 2}]))
      (is (= (f {:a 1 :b 2 :c 3})
             [:odd 1 2 {:a 1 :b 2 :c 3}]))))

  (testing "destructuring with guards"
    (let [f (m/fn {:a A :b B | R} where (even? A) -> [:even A B R]
                  {:a A :b B | R} where (odd? A) -> [:odd A B R]
                  X -> X)]
      (is (= (f {:a 0 :b 1 :c 2})
             [:even 0 1 {:a 0 :b 1 :c 2}]))
      (is (= (f {:a 1 :b 2 :c 3})
             [:odd 1 2 {:a 1 :b 2 :c 3}]))))

  (testing "postponed destructuring till dependency is resolved"
    (let [f (m/fn {K A :b B} K -> [K A B])]
                  ;X _ -> X)]
      (is (= (f {:a 0 :b 1 :c 2} :a)
             [:a 0 1]))
      (is (= (f {:a 0 :b 1 :c 2} :b)
             [:b 1 1])))

    (let [f (m/fn {K A :b B} [K] -> [K A B])]
                  ;X _ -> X)]
      (is (= (f {:a 0 :b 1 :c 2} [:a])
             [:a 0 1]))
      (is (= (f {:a 0 :b 1 :c 2} [:b])
             [:b 1 1])))

    (let [f (m/fn {K A :b B} {P (id K)} [[P]] -> [K A B P]
                  _ _ [[P]] -> P)]
      (is (= (f {:a 0 :b 1 :c 2} {:b :a} [[:b]])
             [:a 0 1 :b]))
      (is (= (f {:a 0 :b 1 :c 2} {:b :a} [[:a]])
             :a)))))

(deftest seq-test
  (testing "seq"
    (let [f ^:vector?
            (m/fn [1 | X] true -> [true  1 X]
                  [2 | X] true -> [true  2 X]
                  X X -> [6 X]
                  [1 | X] false -> [false 3 X]
                  [2 | X] false -> [false 4 X]
                  [3 | X] V -> [V 5 X]
                  X Y -> [7 X Y])]
      (is (= (f :a :b)
             [7 :a :b]))
      (is (= (f [] [])
             [6 []]))
      (is (= (f [] false)
             [7 [] false]))
      (is (= (f [] true)
             [7 [] true]))
      (is (= (f [1 2] true)
             [true 1 '(2)]))
      (is (= (f [1 2 3 4] false)
             [false 3 '(2 3 4)]))
      (is (= (f [4 2 3 4] false)
             [7 [4 2 3 4] false])))

    (let [f ^:vector?
            (m/fn [1 | X] true  -> [true  1 X]
                  [2 | X] true  -> [true  2 X]
                  [1 | X] false -> [false 3 X]
                  [2 | X] false -> [false 4 X]
                  [3 | X] V     -> [V 5 X]
                  X       X     -> [6 X]
                  X       Y     -> [7 X Y])]
      (is (= (f [1 2] true)   [true  1 '(2)]))
      (is (= (f [1 2] false)  [false 3 '(2)]))
      (is (= (f [2 2] true)   [true  2 '(2)]))
      (is (= (f [2 2] false)  [false 4 '(2)]))
      (is (= (f [2]   false)  [false 4 nil]))
      (is (= (f [3 2] false)  [false 5 '(2)]))
      (is (= (f [3 2] true)   [true  5 '(2)]))
      (is (= (f [3 2] nil)    [nil   5 '(2)]))
      (is (= (f :a :a)        [6 :a]))
      (is (= (f "a" "a")      [6 "a"]))
      (is (= (f [1] [1])      [6 [1]]))
      (is (= (f () ())        [6 ()]))
      (is (= (f nil nil)      [6 nil]))
      (is (= (f :a :b)        [7 :a :b]))
      (is (= (f :b :a)        [7 :b :a]))))

  (testing "destructuring and comparing"
    (let [f ^:vector?
            (m/fn [add [add X Y] zero]      -> [1 X Y]
                  [add [mul X Y] zero]      -> [2 X Y]
                  [add X         [mul Y Z]] -> [3 X Y Z]
                  [add X         [add Y Z]] -> [4 X Y Z]
                  [add X         zero]      -> [5 X]
                  X -> [6 X])]
      (is (= (f '[add [add 10 10] zero])
             [1 10 10]))
      (is (= (f '[add [mul 10 11] zero])
             [2 10 11]))
      (is (= (f '[add [mul 10 11] [mul 12 13]])
             [3 '[mul 10 11] 12 13]))
      (is (= (f '[add zero [mul 12 13]])
             [3 'zero 12 13]))
      (is (= (f '[add zero zero])
             [5 'zero]))
      (is (= (f '[mul zero zero])
             [6 '[mul zero zero]])))))

(deftest arity-test
  (testing "multiple arity"
    (let [f (m/fn (X     -> [1 X])
                  (X Y   -> [2 X Y])
                  (X Y Z -> [3 X Y Z]))]
      (is (= (f 1)     [1 1]))
      (is (= (f 1 2)   [2 1 2]))
      (is (= (f 1 2 3) [3 1 2 3])))))

(deftest predicates-test
  (testing "simple predicates with different arity"
    (let [f (m/fn (id (zero)) -> [:zero]
                  (id (even X)) -> [:even X]
                  (id (odd X)) -> [:odd X]
                  X -> [:dunno X])]
      (is (= (f nil) [:dunno nil]))
      (is (= (f 0) [:zero]))
      (is (= (f 1) [:odd 1]))
      (is (= (f 2) [:even 2]))
      (is (= (f 3) [:odd 3]))
      (is (= (f 4) [:even 4])))

    (let [f (m/fn (id (zero)) -> :zero
                  (id (even)) -> :even
                  (id (odd)) -> :odd
                  X -> [:dunno X])]
      (is (= (f nil) [:dunno nil]))
      (is (= (f 0) :zero))
      (is (= (f 1) :odd))
      (is (= (f 2) :even))
      (is (= (f 3) :odd))
      (is (= (f 4) :even)))

    (let [f (m/fn (or X Y) X Y -> [X Y]
                  _ _ _ -> :fail)]
      (is (= (f 0 1 2) :fail))
      (is (= (f 1 1 2) [1 2]))
      (is (= (f nil 1 2) :fail)))

    (let [f (m/fn (or [1 | T] [1 2 3]) -> T
                  X -> [:fail X])]
      (is (= (f nil) [2 3]))
      (is (= (f []) [:fail []]))
      (is (= (f [1 3 4]) [3 4]))))

  (testing "custom predicate"
    (let [f (m/fn (evals-to-true X) -> [:true X]
                  X -> [:fallback X])]
      (is (= (f ()) [:true ()]))
      (is (= (f false) [:fallback false]))
      (is (= (f nil) [:fallback nil]))
      (is (= (f :val) [:true :val])))

    (let [f (m/fn (evals-to-true) -> :true
                  _ -> :fallback)]
      (is (= (f ()) :true))
      (is (= (f false) :fallback))
      (is (= (f nil) :fallback))
      (is (= (f :val) :true))))

  (testing "1+ arity predicates"
    (let [f (m/fn (contains X 1) Y -> [:yes X Y]
                  X Y -> [:no X Y])]
      (is (= (f #{1 2 3} :a)
             [:yes #{1 2 3} :a]))
      (is (= (f #{2 3} :b)
             [:no #{2 3} :b]))
      (is (= (f #{3} :c)
             [:no #{3} :c]))
      (is (= (f nil :d)
             [:no nil :d])))

    (let [f (-> (m/fn (instance IDeref X) -> @X
                      (null) -> ::nil
                      X -> X)
                eval)]
      (is (= (f (volatile! 10)) 10))
      (is (= (f :value) :value))
      (is (= (f nil) ::nil)))

    (let [f (-> (m/fn (instance IDeref X) -> @X
                      (null) -> ::nil)
                eval)]
      (is (= (f (volatile! 10)) 10))
      (is (thrown? Exception (f :value)))
      (is (= (f nil) ::nil)))))

(deftest fib-test
  (let [f8 {0 0 1 1 2 1 3 2 4 3 5 5 6 8 7 13 8 21}
        f9 {0 0 1 1 2 1 3 2 4 3 5 5 6 8 7 13 8 21 9 34}]
    (let [f (m/fn fib
                  (0 -> 0
                   1 -> 1
                   (integer (pos N)) -> (fib 0 1 1 N))
                  (_ B N N -> B
                   A B I N -> (recur B (+ A B) (inc I) N)))
          r (range 10)]
      (is (thrown? Exception (f :x)))
      (is (thrown? Exception (f -1)))
      (is (= f9 (zipmap r (map f r)))))

    (let [f (m/fn fib
                  (0 -> 0
                   1 -> 1
                   (integer (pos N)) -> (fib 0 1 (dec N)))
                  (_ B 0 -> B
                   A B N -> (recur B (+ A B) (dec N))))
          r (range 10)]
      (is (thrown? Exception (f :x)))
      (is (thrown? Exception (f -1))))

    (let [fib (m/fn _ 0 -> 0
                    _ 1 -> 1
                    S (pos N) -> (+ (S S (- N 1)) (S S (- N 2))))]
      (is (thrown? ClassCastException (fib fib :x))))

    (let [fib (m/fn _ 0 -> 0
                    _ 1 -> 1
                    S (integer (pos N)) -> (+ (S S (- N 1)) (S S (- N 2))))
          r (range 9)]
      (is (thrown? Exception (fib fib :x)))
      (is (thrown? Exception (fib fib -1)))
      (is (= f8 (zipmap r (map #(fib fib %) r)))))

    (testing "alias"
      (let [fib (m/fn fib
                      (0 -> 0
                       1 -> 1
                       (integer (pos N)) -> (+ (fib (- N 1)) (fib (- N 2)))))
            r (range 9)]
        (is (thrown? Exception (fib :x)))
        (is (thrown? Exception (fib -1)))
        (is (= f8 (zipmap r (map fib r))))))))

(deftest fizzbuzz-test
  (let [f (m/fn (mod (mod _ 5) 3) -> :fizzbuzz
                (mod _ 3) -> :fizz
                (mod _ 5) -> :buzz
                X -> X)]
    (is (= (map f (range 1 37))
           '(1, 2, :fizz, 4, :buzz, :fizz, 7, 8, :fizz, :buzz,
             11, :fizz, 13, 14, :fizzbuzz, 16, 17, :fizz, 19, :buzz,
             :fizz, 22, 23, :fizz, :buzz, 26, :fizz, 28, 29, :fizzbuzz,
             31, 32, :fizz, 34, :buzz, :fizz)))))

(deftest order-test
  (let [precip (m/fn N where (pos? N) -> (/ 1 N)
                     _ -> 0)]
    (is (= 0 (precip 0)))
    (is (= 1 (precip 1))))

  (let [precip (m/fn 0 -> 0
                     N -> (/ 1 N))]
    (is (= 0 (precip 0)))
    (is (= 1 (precip 1)))))

(deftest date-test
  (let [f (m/fn (date Y (even M) D) -> {:date [Y M D] :month :even}
                (date Y (odd  M) D) -> {:date [Y M D] :month :odd})]
    (is (= (f (java.util.Date. 2006 1 2))
           {:date [2006 1 2] :month :odd}))
    (is (= (f (java.util.Date. 2006 2 2))
           {:date [2006 2 2] :month :even})))

  (let [leap? (m/fn (date (mod _ 400) _ _) -> true
                    (date (mod _ 100) _ _) -> false
                    (date (mod _ 4)   _ _) -> true
                    _                      -> false)]
    (is (= (filter #(leap? (java.util.Date. % 1 2))
                   (range 2000 2050))
          '(2000 2004 2008 2012 2016 2020 2024 2028 2032 2036 2040 2044 2048)))
    (is (= (filter #(leap? (java.util.Date. % 1 2))
                   (range 1600 2022))
           '(1600 1604 1608 1612 1616 1620 1624 1628 1632 1636 1640 1644 1648
             1652 1656 1660 1664 1668 1672 1676 1680 1684 1688 1692 1696 1704
             1708 1712 1716 1720 1724 1728 1732 1736 1740 1744 1748 1752 1756
             1760 1764 1768 1772 1776 1780 1784 1788 1792 1796 1804 1808 1812
             1816 1820 1824 1828 1832 1836 1840 1844 1848 1852 1856 1860 1864
             1868 1872 1876 1880 1884 1888 1892 1896 1904 1908 1912 1916 1920
             1924 1928 1932 1936 1940 1944 1948 1952 1956 1960 1964 1968 1972
             1976 1980 1984 1988 1992 1996 2000 2004 2008 2012 2016 2020)))))

(deftest r-b-tree-test
  (let [f ^:vector?
          (m/fn [:black [:red [:red _ _ _] _ _] _ _] -> :balance
                [:black [:red _ _ [:red _ _ _]] _ _] -> :balance
                [:black _ _ [:red [:red _ _ _] _ _]] -> :balance
                [:black _ _ [:red _ _ [:red _ _ _]]] -> :balance
                _                                    -> :balanced)]
    (is (= (f [:black [:red [:red 1 2 3] 3 4] 5 6])
           :balance))
    (is (= (f [:black [:red 1 2 [:red 3 4 5]] 6 7])
           :balance))
    (is (= (f [:black 1 2 [:red [:red 3 4 5] 6 7]])
           :balance))
    (is (= (f [:black 1 2 [:red 3 4 [:red 5 6 7]]])
           :balance))
    (is (= (f [:black 1 [:red 2 3 [:red 4 5 6]] 7])
           :balanced)))

  (let [f ^{:vector? true :cps true}
          (m/fn [:black [:red [:red _ _ _] _ _] _ _] -> :balance
                [:black [:red _ _ [:red _ _ _]] _ _] -> :balance
                [:black _ _ [:red [:red _ _ _] _ _]] -> :balance
                [:black _ _ [:red _ _ [:red _ _ _]]] -> :balance
                _                                    -> :balanced)]
    (is (= (f [:black [:red [:red 1 2 3] 3 4] 5 6])
           :balance))
    (is (= (f [:black [:red 1 2 [:red 3 4 5]] 6 7])
           :balance))
    (is (= (f [:black 1 2 [:red [:red 3 4 5] 6 7]])
           :balance))
    (is (= (f [:black 1 2 [:red 3 4 [:red 5 6 7]]])
           :balance))
    (is (= (f [:black 1 [:red 2 3 [:red 4 5 6]] 7])
           :balanced))))

(deftest aggregators-test
  (testing "lazy filter"
    (let [f (m/fn self
                  (_ [] -> nil
                   F [H | T] -> (lazy-seq
                                  (cond->> (self F T)
                                    (F H) (cons H)))))]
      (is (= (f even? nil)
             nil))
      (is (= (f even? (vec (range 5)))
             '(0 2 4)))
      (is (= (take 2 (f even? (vec (range 5))))
             '(0 2)))
      (is (= (f even? (vec (range 5)))
             [0 2 4]))
      (is (= (f even? (range 5))
             '(0 2 4)))))

  (testing "filter"
    (let [f (m/fn self
                  (F C -> (self F (empty C) C))
                  (_ A [] -> A
                   F A [H | T] -> (recur F (cond-> A (F H) (conj H)) T)))]
      (is (= (f even? nil)
             nil))
      (is (= (f even? (vec (range 5)))
             [0 2 4]))
      (is (= (f even? (range 5))
             '(4 2 0)))))

  (testing "lazy map"
    (let [f (m/fn self
                  (_ [] -> nil
                   F [H | T] -> (lazy-seq (cons (F H) (self F T)))))]
      (is (= (f #(* 2 %) (range 6))
             '(0 2 4 6 8 10)))
      (is (= (f #(* 2 %) (vec (range 6)))
             [0 2 4 6 8 10]))))

  (testing "map"
    (let [f (m/fn self
                  (F C -> (self F (empty C) C))
                  (_ A [] -> A
                   F A [H | T] -> (recur F (conj A (F H)) T)))]
      (is (= (f #(* 2 %) (range 6))
             '(10 8 6 4 2 0)))
      (is (= (f #(* 2 %) (vec (range 6)))
             [0 2 4 6 8 10]))))

  (testing "reduce"
    (let [f (m/fn self
                  (_ [] -> nil
                   F [H | T] -> (self F (F H) T))
                  (_ A [] -> A
                   F A [H | T] -> (recur F (F A H) T)))]
      (is (= nil (f + nil)))
      (is (= nil (f + ())))
      (is (= 0 (f + '(0))))
      (is (= 3 (f + '(0 1 2))))
      (is (= 45 (f + (range 10)))))))

(deftest zippers-tests
  (let [tree [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]]
    (let [f (m/fn (z/end R) -> R
                  (z/node 310 L) -> (recur (zip/replace L ::replaced))
                  L -> (recur (zip/next L)))]
      (is (= (f (zip/vector-zip tree))
             [0 1 [20 21] [30 [::replaced [3200 3201] [3300]] 32] 4 []]))))

  (let [tree [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]]
    (let [f (m/fn (z/end R) -> R
                  (z/node 310 (z/right (id L))) -> (recur (zip/replace L ::replaced))
                  L -> (recur (zip/next L)))]
      (is (= (f (zip/vector-zip tree))
             [0 1 [20 21] [30 [310 ::replaced [3300]] 32] 4 []]))))

  (let [tree [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]]
    (let [f (m/fn (z/end R) -> R
                  (z/node 310 (z/right (id L))) -> (recur (zip/remove L))
                  L -> (recur (zip/next L)))]
      (is (= (f (zip/vector-zip tree))
             [0 1 [20 21] [30 [310] 32] 4 []]))))

  (let [tree [0 1 [20 21] [30 [310] 32] 4 []]]
    (let [f (m/fn (z/end R) -> R
                  (z/node 310 (z/right L)) -> (recur (zip/replace L ::replaced))
                  L -> (recur (zip/next L)))]
      (is (thrown? Exception (f (zip/vector-zip tree)))))
    (let [f (m/fn (z/end R) -> R
                  (z/node 310 (z/right (id L))) -> (recur (zip/replace L ::replaced))
                  L -> (recur (zip/next L)))]
      (is (= (f (zip/vector-zip tree))
             [0 1 [20 21] [30 [310] 32] 4 []]))))

  (let [tree [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]]
    (let [f (m/fn _ (z/end R) -> R
                  {N (id V) | M} (z/node N L) -> (recur M (zip/replace L V))
                  M L -> (recur M (zip/next L)))]
      (is (= (f {310 ::a 4 ::b 3201 ::c}
                (zip/vector-zip tree))
             [0 1 [20 21] [30 [::a [3200 ::c] [3300]] 32] ::b []])))))

(deftest errors-test
  (let [ctx (ctx/->context :stop-on-error? false
                           :show-problems? false)]
    (is (thrown? Exception (c/spec->fn '((A B C -> D)) ctx)))
    (is (= (ctx/ctx->errors ctx)
           '({:stage :preparation, :unused #{A B C}, :level :warning,
              :id :clause/unused-vars, :arity 3, :clause 0}
             {:stage :preparation, :level :error, :id :clause/undefined-vars,
              :undefined #{D}, :arity 3, :clause 0}))))

  (let [ctx (ctx/->context :stop-on-error? false
                           :show-problems? false)]
    (is (thrown? Exception (c/spec->fn '((A B C)) ctx)))
    (is (= (ctx/ctx->errors ctx)
           '({:stage :preparation, :unused #{A B C}, :level :warning,
              :id :clause/unused-vars, :arity 3, :clause 0}
             {:stage :preparation, :level :error,
              :id :clause/no-mode, :arity 3, :clause 0}))))

  (let [ctx (ctx/->context :stop-on-error? false
                           :show-problems? false)]
    (is (thrown? Exception (c/spec->fn '(A -> A
                                         A A -> A)
                                       ctx)))
    (is (= (ctx/ctx->errors ctx))
        '({:stage :preparation, :level :error,
           :id :clause/inconsistent-arity, :arity 1, :clause 1})))

  (let [ctx (ctx/->context :stop-on-error? false
                           :show-problems? false)]
    (c/spec->fn '((X -> 4)
                  (X Y -> [7]
                   X Y -> [7]
                   X Y -> [7]))
                ctx)
    (is (= (ctx/ctx->errors ctx))
        '({:stage :tree, :level :warning, :id :clause/unreachable,
           :arity 2, :clause 2}
          {:stage :tree, :level :warning, :id :clause/unreachable,
           :arity 2, :clause 1}
          {:stage :preparation, :unused #{X Y}, :level :warning,
           :id :clause/unused-vars, :arity 2, :clause 2}
          {:stage :preparation, :unused #{X Y}, :level :warning,
           :id :clause/unused-vars, :arity 2, :clause 1}
          {:stage :preparation, :unused #{X Y}, :level :warning,
           :id :clause/unused-vars, :arity 2, :clause 0}
          {:stage :preparation, :unused #{X}, :level :warning,
           :id :clause/unused-vars, :arity 1, :clause 0}))

    (let [ctx (ctx/->context :stop-on-error? false
                             :show-problems? false)]
      (is (thrown? Exception (c/spec->fn '(((or [1 | T] X) -> [T X]
                                            X -> [:fail X]))
                                         ctx)))
      (is (= (ctx/ctx->errors ctx))
          '({:stage :tree, :level :warning, :id :clause/unreachable,
             :arity 1, :clause 1}
            {:lr [$$0 (or [1 | T] X)],
             :stage :unification, :level :error, :arg 0,
             :id :predicate/instantiation, :arity 1, :clause 0,
             :vars #{X}})))))

(deftest qm-test
  (testing "destructuring and comparing"
    (let [f ^:vector?
            (m/?fn [add [add ?x ?y] zero]        -> [1 ?x ?y]
                   [add [mul ?x ?y] zero]        -> [2 ?x ?y]
                   [add ?x          [mul ?y ?z]] -> [3 ?x ?y ?z]
                   [add ?x          [add ?y ?z]] -> [4 ?x ?y ?z]
                   [add ?x          zero]        -> [5 ?x]
                   ?x -> [6 ?x])]
      (is (= (f '[add [add 10 10] zero])
             [1 10 10]))
      (is (= (f '[add [mul 10 11] zero])
             [2 10 11]))
      (is (= (f '[add [mul 10 11] [mul 12 13]])
             [3 '[mul 10 11] 12 13]))
      (is (= (f '[add zero [mul 12 13]])
             [3 'zero 12 13]))
      (is (= (f '[add zero zero])
             [5 'zero]))
      (is (= (f '[mul zero zero])
             [6 '[mul zero zero]])))))

(defn *visit [f loc]
  (let [node (zip/node loc)
        *node (f node)]
    (cond-> loc (not= node *node) (zip/replace *node))))

(defn visit
  {:inputs {2 #{0}}}
  ([env l r]
   (let [[_ f output] r
         [var env] (e/push-assign env `(*visit ~f ~l))]
     (e/push-queue env var output))))

(deftest postwalk-test
  (let [tree {:op 1 :c [{:op 11 :c [{:op 111}
                                    {:op 112}]}
                        {:op 12 :c [{:op 121}
                                    {:op 122}]}]}
        postwalk (m/fn postwalk
                       (F L -> (postwalk F L true))
                       (F (z/down (id L))            true -> (recur F L true)
                        F (visit F (z/right (id L))) _    -> (recur F L true)
                        F (visit F (z/up (id L)))    _    -> (recur F L false)
                        F (visit F L)                _    -> L))
        s (volatile! [])]
    (is (= (->> (cps/tree-zipper tree)
                (postwalk (fn [node]
                            (vswap! s conj (:op node))
                            (update node :op inc)))
                (zip/node))
           {:op 2 :c [{:op 12 :c [{:op 112} {:op 113}]}
                      {:op 13 :c [{:op 122} {:op 123}]}]}))
    (is (= @s [111 112 11 121 122 12 1]))))
