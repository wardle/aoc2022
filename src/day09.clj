(ns day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [s]
  (let [[d n] (str/split s #" ")]
    (repeat (parse-long n) d)))

(defn move
  "Given coordinates 'x' 'y', move in direction 'd' by 'n' spaces. Uses a
  coordinate system with 0,0 at bottom left."
  [[x y] d]
  (case d
    "L" [(dec x) y]
    "R" [(inc x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]
    (throw (ex-info "Unknown command" {:cmd d}))))

(defn touching? [[x1 y1] [x2 y2]]
  (and (< (abs (- x1 x2)) 2) (< (abs (- y1 y2)) 2)))

(defn move-tail
  "Determine the tail position based on the current position and the location of head."
  [old-tail head]
  (if (touching? old-tail head)
    old-tail                                                ;; if tail touching new head position, leave tail in place
    (let [[dx dy] (mapv - head old-tail)]                   ;; otherwise, move tail towards head
      (cond-> old-tail
              (pos? dx) (move "R")
              (neg? dx) (move "L")
              (pos? dy) (move "U")
              (neg? dy) (move "D")))))

(defn move-knots
  [[head & knots] d]
  (let [head' (move head d)]
    (loop [result [head']
           knots' knots
           to head']
      (if-let [knot (first knots')]
        (let [moved (move-tail knot to)]
          (recur (conj result moved) (rest knots') moved))
        result))))

(defn perform-moves
  "Returns a sequence of steps, each containing keys :from, :cmd and :to. Moves
  should be a sequence of directions e.g. [\"D\" \"R\" \"U\" \"L\"."
  [moves]
  (:steps (reduce (fn [{:keys [head tail] :as acc} cmd]
                    (let [head' (move head cmd)
                          tail' (move-tail tail head')]
                      (-> acc
                          (assoc :head head' :tail tail')
                          (update :steps conj {:from head :cmd cmd :to head' :tail tail'}))))
                  {:head [0 0] :tail [0 0] :steps []} moves)))

(defn perform-moves2
  "A general purpose rope mover, with configurable number of knots."
  [n-knots moves]
  (:steps (reduce (fn [{:keys [knots] :as acc} cmd]
                    (let [knots' (move-knots knots cmd)]
                      (-> acc
                          (assoc :knots knots')
                          (update :steps conj {:cmd cmd :knots knots'}))))
                  {:knots (repeat n-knots [0 0]) :steps []} moves)))


(comment
  (def input (str/split-lines (slurp (io/resource "day09.txt"))))
  (def input (str/split-lines "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"))
  input
  (take 5 input)
  (mapcat parse-line input)
  
  ;; how many positions does the tail visit at least once?
  (->> input
       (mapcat parse-line)
       perform-moves
       (map :tail)
       (into #{})
       count)

  ;; Simulate your complete series of motions on a larger rope with ten knots.
  ;; How many positions does the tail of the rope visit at least once?
  (->> input
       (mapcat parse-line)
       (perform-moves2 10)
       (map :knots)
       (map peek)
       (into #{})
       count))



