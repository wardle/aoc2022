(ns day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(comment
  (def lines (-> (io/resource "day01.txt")
                 slurp
                 (str/split-lines)))
  (def calories (->> (partition-by str/blank? lines)
                     (map #(apply + (map parse-long %)))
                     (remove nil?)))
  (apply max calories)
  (apply + (take 3 (reverse (sort calories)))))


