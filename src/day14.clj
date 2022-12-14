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

(defn valid-move
  [blocks [x y] {:keys [void-y floor-y]}]
  (let [y' (inc y)]
    (cond
      (blocks [x y]) nil                                    ;; if block already there, end
      (and void-y (= y' void-y)) nil                        ;; fall into the void, if exists
      (and floor-y (= y' floor-y)) [0 0]                    ;; hit a fixed floor, if exists
      (not (blocks [x y'])) [0 1]                           ;; fall straight down
      (not (blocks [(dec x) y'])) [-1 1]                    ;; fall to left and down
      (not (blocks [(inc x) y'])) [1 1]                     ;; fall to right and down
      :else [0 0])))                                        ;; sand can't move from here

(defn add-sand
  [blocks cfg]
  (loop [blocks blocks, n 0, sand start-point]
    (let [move (valid-move blocks sand cfg)]
      (if-not move
        n                                                   ;; block already there, or fallen out of world - return count
        (if (= move [0 0])                                  ;; block can't move further - so store
          (recur (conj blocks sand) (inc n) start-point)
          (recur blocks n (map + sand move)))))))

(comment
  (def input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
  (def input (slurp (io/resource "day14.txt")))

  (def blocks (->> (str/split input #"\n")
                   (map parse-line)
                   all-points))
  ;; part 1
  (time (add-sand blocks {:void-y (+ 10 (apply max (map second blocks)))})) ; => 592 (49 milliseconds)

  ;; part 2
  (time (add-sand blocks {:floor-y (+ 2 (apply max (map second blocks)))}))) ; => 30367  (2313 milliseconds)






