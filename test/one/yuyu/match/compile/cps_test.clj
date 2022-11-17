(ns one.yuyu.match.compile.cps-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [one.yuyu.match.compile.cps :as cps]))

(deftest postwalk-test
  (let [tree {:op 1 :c [{:op 11 :c [{:op 111}
                                    {:op 112}]}
                        {:op 12 :c [{:op 121}
                                    {:op 122}]}]}
        s (volatile! [])]
    (is (= (->> (cps/tree-zipper tree)
                (cps/postwalk (fn [node]
                                (vswap! s conj (:op node))
                                (update node :op inc)))
                (zip/node))
           {:op 2 :c [{:op 12 :c [{:op 112}
                                  {:op 113}]}
                      {:op 13 :c [{:op 122}
                                  {:op 123}]}]}))
    (is (= @s [111 112 11 121 122 12 1]))))
