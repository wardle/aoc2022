(ns day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [s]
  (->> (str/split s #" -> ") (mapv #(mapv parse-long (str/split % #",")))))

(defn range*
  "Returns a sequence from 0 to end, inclusive."
  [end]
  (if (>= end 0) (range (inc end)) (range 0 (dec end) -1)))

(defn points*
  "Return a set of all points on a line from [x1 y1] to [x2 y2]. "
  [[x1 y1] [x2 y2]]
  (let [cx (- x2 x1) cy (- y2 y1)]
    (set (if (= cx 0) (for [y (range* cy)] [x1 (+ y1 y)])
                      (for [x (range* cx)] [(+ x1 x) y1])))))

(defn points
  "Returns a set of all points on the line."
  [line]
  (reduce (fn [acc [from to]] (set/union acc (points* from to))) #{} (partition 2 1 line)))

(defn all-points
  "Returns a set of all points on all lines."
  [lines] (set (mapcat points lines)))

(def start-point [500 0])
(def maximum-y 200)

(defn valid-move [blocks [x y]]
  (cond
    (blocks [x y]) nil                                      ;; if block already there, end
    (> y maximum-y) nil                                     ;; fallen out of world
    (not (blocks [x (inc y)])) [0 1]                        ;; fall straight down
    (not (blocks [(dec x) (inc y)])) [-1 1]                 ;; fall to left and down
    (not (blocks [(inc x) (inc y)])) [1 1]                  ;; fall to right and down
    :else [0 0]))                                           ;; sand can't move from here

(defn add-sand [blocks]
  (loop [blocks blocks, n 0, sand start-point]
    (let [move (valid-move blocks sand)]
      (if-not move
        n   ;; block already there, or fallen out of world - return count
        (if (= move [0 0])  ;; block can't move further - so store
          (recur (conj blocks sand) (inc n) start-point)
          (recur blocks n (map + sand move)))))))

(defn draw [blocks]
  (let [min-x (apply min (map first blocks))
        max-x (inc (apply max (map first blocks)))
        max-y (inc (apply max (map second blocks)))]
    (->>(for [x (range min-x max-x)
              y (range max-y)]
          (if (blocks [x y]) "#" "."))
        (partition-all (- max-x min-x))
        (mapv #(apply str %)))))


(comment
  (def input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
  (def input (slurp (io/resource "day14.txt")))
  (def lines (->> (str/split input #"\n")
                  (map parse-line)))
  (def blocks (all-points lines))

  ;; part 1
  (add-sand blocks)   ; => 592 (49 milliseconds)

  ;; part 2
  (def floor-y (+ 2 (apply max (map second blocks))))
  (def blocks' (set/union blocks (points [[-10000 floor-y] [10000 floor-y]])))
  (add-sand blocks')
  (draw blocks))





