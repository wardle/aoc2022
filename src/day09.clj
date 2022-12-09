(ns day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [s]
  (let [[d n] (str/split s #" ")]
    (repeat (parse-long n) d)))

(def transforms
  {"L" [-1 0] "R" [1 0] "U" [0 1] "D" [0 -1]})

(defn move
  "Given coordinates 'x' 'y', move in direction 'd' (L R U D)"
  [coord d]
  (mapv + (transforms d) coord))

(defn touching? [[x1 y1] [x2 y2]]
  (and (< (abs (- x1 x2)) 2) (< (abs (- y1 y2)) 2)))

(defn move-if-needed
  "Determine the tail position based on the current position and the location of head."
  [current head]
  (if (touching? current head)
    current                                                ;; if tail touching new head position, leave tail in place
    (let [[dx dy] (mapv - head current)]                   ;; otherwise, move tail towards head
      (cond-> current
              (pos? dx) (move "R")
              (neg? dx) (move "L")
              (pos? dy) (move "U")
              (neg? dy) (move "D")))))

(defn move-knots
  "Move a sequence of knots in direction 'd' (L R U D)"
  [[head & knots] d]
  (let [head' (move head d)]
    (loop [result [head'] knots' knots to head']
      (if-let [knot (first knots')]
        (let [moved (move-if-needed knot to)]
          (recur (conj result moved) (rest knots') moved))
        result))))

(defn perform-moves
  "A general purpose rope mover, with configurable number of knots."
  [n-knots moves]
  (reductions move-knots (vec (repeat n-knots [0 0])) moves))


(comment
  (def input (str/split-lines (slurp (io/resource "day09.txt"))))
  (def input (str/split-lines "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"))
  input
  (take 5 input)
  (mapcat parse-line input)

  ;; how many positions does the tail visit at least once?
  (->> input
       (mapcat parse-line)
       (perform-moves 2)
       (map peek)
       (into #{})
       count)

  ;; Simulate your complete series of motions on a larger rope with ten knots.
  ;; How many positions does the tail of the rope visit at least once?
  (->> input
       (mapcat parse-line)
       (perform-moves 10)
       (map peek)
       (into #{})
       count))



