(ns day03
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defn make-rucksack [s]
  (let [n-items (count s)]
    (partition (/ n-items 2) s)))

(defn items-in-both [s]
  (apply set/intersection (map set (make-rucksack s))))

(defn priority-item
  "Given a item (x), return the priority score, according to the rules:
  Lowercase item types a through z have priorities 1 through 26.
  Uppercase item types A through Z have priorities 27 through 52."
  [x]
  (let [c (int x)]
    (cond
      (< 96 c 123) (- c 96)
      (< 64 c 91) (- c 38))))

(defn calc-priority [s]
  (let [item (first (items-in-both s))]
    (priority-item item)))


(comment
  (def input (slurp (io/resource "day03.txt")))
  (apply + (->> (str/split-lines input)
                (map calc-priority)))
  (apply + (->> (str/split-lines input)
                (partition 3)
                (map #(apply set/intersection (map set %)))
                (map first)
                (map priority-item))))


