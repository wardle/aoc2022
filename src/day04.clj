(ns day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-range [s]
  (mapv parse-long (str/split s #"-")))

(defn parse-line [s]
  (mapv parse-range (str/split s #",")))

(defn parse-ranges [s]
  (let [[[a b] [x y]] (parse-line s)]
    [(set (range a (inc b))) (set (range x (inc y)))]))

(defn fully-contains? [[s1 s2]]
  (or (set/subset? s1 s2) (set/subset? s2 s1)))

(defn intersects? [[s1 s2]]
  (boolean (seq (set/intersection s1 s2))))

(comment
  (def input (str/split-lines (slurp (io/resource "day04.txt"))))
  (def lines (map parse-line input))
  ;; number of pairs where one range fully contains another...
  (frequencies (->> input
                    (map parse-ranges)
                    (map fully-contains?)))
  ;; number of pairs where range overlaps at all...
  (frequencies (->> input
                    (map parse-ranges)
                    (map intersects?))))

