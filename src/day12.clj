(ns day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def directions [[1 0] [-1 0] [0 -1] [0 1]])
(defn height [c] (when c (case c \S 96 \E 123 (int c))))
(defn valid-choice? [current target] (and current target (<= (- (height target) (height current)) 1)))
(defn available-choices [mp  [x y]]
  (let [current (get-in mp [y x])]
    (->> directions
         (mapv (fn [[cx cy]]
                 (let [target-x (+ x cx) target-y (+ y cy)
                       target (get-in mp [target-y target-x])]
                   (when (valid-choice? current target)
                     [target-x target-y]))))
         (filterv some?))))

(defn search
  "Returns a set of coordinates for the character 'c'"
  [mp c]
  (set (for [y (range (count mp)) x (range (count (first mp))) :when (= c (get-in mp [y x]))] [x y])))

(defn solve
  "Return shortest path between start-char and end-char in thr grid 'mp'"
  [mp start-char end-char]
  (let [starts (search mp start-char)
        ends (search mp end-char)]
    (loop [choices starts
           visited starts
           step 0]
      (if (some #(contains? ends %) choices)
        step
        (recur (remove visited (set (mapcat #(available-choices mp %) choices)))
               (into visited choices)
               (inc step))))))

(comment
  (def input (mapv vec (str/split-lines (slurp (io/resource "day12.txt")))))
  (def input (mapv vec (str/split-lines "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")))
  ;;part 1
  (solve input \S \E)  ;; = 506   (or 31 for sample)
  ;; part 2
  (solve input \a \E))  ;; = 502   (or 29 for sample)






