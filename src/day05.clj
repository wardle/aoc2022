(ns day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn create-stacks [nstacks]
  (vec (repeat nstacks [])))

(defn add-to-stack [stacks stack-id item]
  (update stacks stack-id conj item))

(defn remove-from-stack [stacks stack-id]
  (update stacks stack-id pop))

(defn initialise-stack [stacks stack-id items]
  (assoc stacks stack-id items))

(defn move-from-stack
  "Move an item from one stack to another"
  [stacks from-stack-id to-stack-id]
  (let [item (peek (get stacks from-stack-id))]
    (-> stacks
        (remove-from-stack from-stack-id)
        (add-to-stack to-stack-id item))))

(defn move-n-from-stack
  "Sequentially moves 'n' items from one stack to another."
  [stacks from-stack-id to-stack-id n]
  (if (= n 0)
    stacks
    (-> stacks
        (move-from-stack from-stack-id to-stack-id)
        (move-n-from-stack from-stack-id to-stack-id (dec n)))))

(defn move-n-from-stack*
  "Move 'n' items from one stack to another as a block"
  [stacks from-stack-id to-stack-id n]
  (let [from-stack (get stacks from-stack-id)
        idx (- (count from-stack) n)
        items (subvec from-stack idx)]
    (-> stacks
        (update from-stack-id subvec 0 idx)
        (update to-stack-id #(apply conj % items)))))

(defn top-of-stacks [stacks]
  (mapv peek stacks))

(defn parse-instruction
  "Parse an instruction such as
       \"move 17 from 6 to 8\""
  [s]
  (let [tokens (str/split s #"\s")
        [verb n _ from _ to] tokens]
    (when (and (= 6 (count tokens)) (every? identity tokens))
      {:verb (keyword verb) :n (parse-long n)
       :from (dec (parse-long from)) :to (dec (parse-long to))})))

(defn execute-instruction
  [stacks s]
  (let [{:keys [verb n from to] :as instruction} (parse-instruction s)]
    (if-not instruction
      stacks
      (case verb
        :move
        (move-n-from-stack* stacks from to n)
        (throw (ex-info "Unknown instruction" instruction))))))

(def init-state
  [["S" "Z" "P" "D" "L" "B" "F" "C"]
   ["N" "V" "G" "P" "H" "W" "B"]
   ["F" "W" "B" "J" "G"]
   ["G" "J" "N" "F" "L" "W" "C" "S"]
   ["W" "J" "L" "T" "P" "M" "S" "H"]
   ["B" "C" "W" "G" "F" "S"]
   ["H" "T" "P" "M" "Q" "B" "W"]
   ["F" "S" "W" "T"]
   ["N" "C" "R"]])

(comment
  (-> (create-stacks 5)
      (initialise-stack 0 [" A "])
      (add-to-stack 2 " A ")
      (add-to-stack 0 " B ")
      (add-to-stack 4 " C ")
      (move-from-stack 4 2)
      (move-from-stack 2 4)
      (move-from-stack 2 4)
      (move-n-from-stack 0 1 2)
      (top-of-stacks))

  (def input (str/split-lines (slurp (io/resource " day05.txt "))))
  (def instructions (remove nil? (map parse-instruction input)))
  init-state
  instructions
  (apply str (->> (reduce execute-instruction init-state input)
                  (mapv peek)))



  (add-to-stack (create-stacks 5) 2 "A")


  input)