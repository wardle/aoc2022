(ns day10
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defmulti execute (fn [_state {cmd :cmd}] cmd))
(defmethod execute :noop [state cmd] state)
(defmethod execute :addx [state {:keys [param]}] (update state :x + param))

(defn parse-instruction
  "Parse a CPU instruction returning a vector of commands each taking one CPU cycle"
  [s]
  (match (str/split s #"\s")
    ["noop"] [{:cmd :noop}]
    ["addx" n] [{:cmd :noop} {:cmd :addx :param (parse-long n)}]))

(defn execute-all [instructions]
  (reductions (fn [state cmd] (-> state (execute cmd) (update :cycle inc)))
              {:cycle 1 :x 1} instructions))

(def interesting-cycles #{20 60 100 140 180 220})

(defn coords [x] (let [x' (dec x)] [(mod x' 40) (int (/ x' 40))]))

(comment
  (def input (str/split-lines "noop\naddx 3\naddx -5"))
  (def input (str/split-lines "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"))
  (def input (str/split-lines (slurp (io/resource "day10.txt"))))
  input
  (mapcat parse-instruction input)
  (execute {:x 1} {:cmd :noop})
  (execute {:x 1} {:cmd :addx :param 4})
  (->> (execute-all (mapcat parse-instruction input))
       (map (fn [{:keys [cycle x] :as state}] (assoc state :signal-strength (* cycle x))))
       (filter (fn [{:keys [cycle]}] (interesting-cycles cycle)))
       (map :signal-strength)
       (reduce +))
  (->> (execute-all (mapcat parse-instruction input))
       (map (fn [{:keys [cycle x] :as state}]
              (assoc state :coords (coords cycle)
                           :pixel (if (#{(dec x) x (inc x)} (first (coords cycle))) "#" "."))))
       (map :pixel)
       (partition 40)
       (map #(apply str %))))
