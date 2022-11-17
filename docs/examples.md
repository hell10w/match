```
(require '[one.yuyu.match :as m]
```

#### Rewriting a tree leafs with a dictionary

```
(let [f (m/fn (z/end R) _ -> R
              (z/node K L) {K (id V) | M} -> (recur (zip/replace L V) M)
              L M -> (recur (zip/next L) M))]
  (-> [0 1 [20 21] [30 [310 [3200 3201] [3300]] 32] 4 []]
      zip/vector-zip
      (f {310 0 32 1}))) 
=> [0 1 [20 21] [30 [0 [3200 3201] [3300]] 1] 4 []]      
```

``` 
(-> '(m/fn (z/end R) _ -> R
           (z/node K L) {K (id V) | M} -> (recur (zip/replace L V) M)
           L M -> (recur (zip/next L) M))
    macroexpand )
=>
(fn*
 ([$$0 $$1]
  (if (clojure.zip/end? $$0)
   (let [$$-1 (clojure.zip/root $$0)] $$-1)
   (let [$$-2 (clojure.zip/node $$0) 
         $$-3 (clojure.core/get $$1 $$-2)]
    (if $$-3 
     (recur (zip/replace $$0 $$-3) $$1)
     (recur (zip/next $$0) $$1))))))
```

#### Transducer

```
(defn dedupe []
  (fn [xf]
    (let [prev (volatile! ::none)]
      (m/fn (-> (xf))
            (R -> (xf R))
            (R I where (= I @prev) -> R
             R I -> (do
                      (vreset! prev I)
                      (xf R I))))))) 
```

#### Mapreduce

```
;; filter
((m/fn self  
       (F C -> (self F (empty C) C))
       (F A [] -> A
        F A [H | T] -> (recur F (cond-> A (F H) (conj H)) T)))
 even? (vec (range 10)))
;;=> [0 2 4 6 8]
```

```
;; map
((m/fn self
       (F C -> (self F (empty C) C))
       (F A [] -> A
        F A [H | T] -> (recur F (conj A (F H)) T)))
 #(* 2 %) (vec (range 5)))
;;=> [0 2 4 6 8]
```

```
;; reduce
((m/fn self
       (F [] -> nil
        F [H | T] -> (self F (F H) T))
       (F A [] -> A
        F A [H | T] -> (recur F (F A H) T)))
 + (range 10))
;;=> 45 
```
