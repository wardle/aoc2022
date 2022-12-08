(ns day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def rules
  {[:rock :rock]         :draw
   [:rock :scissors]     :lose
   [:rock :paper]        :win
   [:scissors :rock]     :win
   [:scissors :paper]    :lose
   [:scissors :scissors] :draw
   [:paper :rock]        :lose
   [:paper :scissors]    :win
   [:paper :paper]       :draw})

(def rules*
  (reduce-kv (fn [acc [a b] v]
               (assoc acc [a v] b)) {} rules))

(def lookup
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def outcome-scores
  {:win  6
   :draw 3
   :lose 0})

(def play-scores
  {:rock     1
   :paper    2
   :scissors 3})

(defn parse-game
  [s]
  (map lookup (str/split s #"\s")))

(defn fetch-games
  []
  (-> (slurp (io/resource "day02.txt"))
      str/split-lines))

(defn score-game
  [s]
  (let [[a b] (parse-game s)
        score1 (get outcome-scores (get rules [a b]))
        score2 (get play-scores b)]
    (+ score1 score2)))

(def lookup2
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :lose
   "Y" :draw
   "Z" :win})

(defn parse-game2
  [s]
  (map lookup2 (str/split s #"\s")))

(defn score-game2
  [s]
  (let [[a b] (parse-game2 s)
        score1 (get outcome-scores b)
        score2 (get play-scores (get rules* [a b]))]
    (+ score1 score2)))


(comment
  (get rules [:paper :rock])
  (parse-game "C Z")
  (get rules (parse-game "B Z"))
  (apply + (map score-game (fetch-games)))
  (apply + (map score-game2 (fetch-games))))