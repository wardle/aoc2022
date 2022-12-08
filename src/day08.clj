(ns day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-grid
  "30373
25512
65332
33549
35390")

(defn parse-line [s]
  (mapv (comp parse-long str) s))

(defn parse-grid [lines]
  (mapv parse-line lines))

(defn to-edge
  [grid x y cx cy]
  (let [width (count (first grid))
        height (count grid)]
    (loop [result [] x' (+ x cx) y' (+ y cy)]
      (if (or (>= x' width) (< x' 0)
              (>= y' height) (< y' 0))
        result
        (recur (conj result (get-in grid [y' x'])) (+ x' cx) (+ y' cy))))))

(defn to-edges
  "Returns paths to the edges of the grid from the coordinate specified."
  [grid x y]
  [(to-edge grid x y 1 0)
   (to-edge grid x y 0 1)
   (to-edge grid x y -1 0)
   (to-edge grid x y 0 -1)])

(defn visible-path? [limit path]
  (or (= 0 (count path))
      (every? #(< % limit) path)))

(defn visible? [grid x y]
  (let [v (get-in grid [y x])
        paths (to-edges grid x y)]
    (boolean (some #(visible-path? v %) paths))))

(defn grid-visible?
  [grid]
  (for [x (range (count (first grid)))
        y (range (count grid))]
    {:x x :y y :visible? (visible? grid x y)}))

(defn untilv
  "Returns the collection up to and including when the predicate is true."
  [pred coll]
  (reduce (fn [acc v]
            (let [acc' (conj acc v)]
              (if (pred v) (reduced acc') acc')))
          [] coll))

(defn scenic-score
  "Calculates a scenic score for a given coordinate"
  [grid x y]
  (let [v (get-in grid [y x])]
    (->> (to-edges grid x y)
         (mapv (fn [path] (untilv #(>= % v) path)))
         (mapv count)
         (reduce *))))

(defn scenic-scores
  [grid]
  (for [x (range (count (first grid)))
        y (range (count grid))]
    {:x x :y y :scenic-score (scenic-score grid x y)}))

(comment
  (def input (str/split-lines example-grid))
  (def input (str/split-lines (slurp (io/resource "day08.txt"))))
  input
  (def grid (parse-grid input))
  (to-edge grid 0 0 1 0)

  ;; how many trees are visible?
  (->> (grid-visible? (parse-grid input))
       (filter :visible?)
       count)

  ;; what is highest scenic score?
  (->> input
       parse-grid
       scenic-scores
       (map :scenic-score)
       (reduce max)))



