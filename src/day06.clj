(ns day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn all-different?
  [chars]
  (= (count chars) (count (set chars))))

(comment
  (->> (str/trim (slurp (io/resource "day06.txt")))
       (partition 4 1)
       (map-indexed (fn [idx chars]
                      (when (all-different? chars)
                        {:idx idx :chars chars :start-marker (+ idx 4)})))
       (remove nil?)
       (first))

  (->> (str/trim (slurp (io/resource "day06.txt")))
       (partition 14 1)
       (map-indexed (fn [idx chars]
                      (when (all-different? chars)
                        {:idx idx :chars chars :start-marker (+ idx 14)})))
       (remove nil?)))
