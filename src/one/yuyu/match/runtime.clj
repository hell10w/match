(ns one.yuyu.match.runtime)

(def ^{:private true} backtrack (Exception.))

(defn backtrack? [e] (identical? e backtrack))
(defn fail [] (throw backtrack))
