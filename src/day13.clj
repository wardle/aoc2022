(ns day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn compare-packet
  "Returns a negative number, zero, or a positive number when a is logically 'less than', 'equal to', or 'greater than' b."
  [a b]
  (cond
    (and (nil? a) (nil? b)) 0
    (and (number? a) (number? b)) (compare a b)
    (number? a) (compare-packet [a] b)
    (number? b) (compare-packet a [b])
    (and (seq a) (not (seq b))) 1
    (and (not (seq a)) (seq b)) -1
    :else (let [[a' & a-rest] a
                [b' & b-rest] b
                r1 (compare-packet a' b')]
            (if (= 0 r1) (compare-packet a-rest b-rest) r1))))

(def divider-packets #{[[2]] [[6]]})

(comment
  (def data "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]")
  (def data (slurp (io/resource "day13.txt")))

  ;; part 1   => 5390
  (->> (str/split data #"\n\n")
       (map #(str/split % #"\n"))
       (map #(map edn/read-string %))
       (keep-indexed (fn [i [left right]] (when (>= 0 (compare-packet left right)) (inc i))))
       (reduce +))

  ;; part 2   => 19261
  (->> (str/split data #"\n")
       (remove str/blank?)
       (map edn/read-string)
       (concat divider-packets)
       (sort compare-packet)
       (keep-indexed (fn [i v] (when (divider-packets v) (inc i))))
       (reduce *)))