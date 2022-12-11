(ns day11
  (:require [clojure.math.numeric-tower :refer [lcm]]))

(def monkeys1
  [{:items  [ 79, 98]
    :op #(* 19 %) :divisor 23 :targets [2 3]}
   {:items [ 54, 65, 75, 74]
    :op #(+ 6 %) :divisor 19 :targets [2 0]}
   {:items [79, 60, 97]
    :op #(* % %) :divisor 13 :targets [1 3]}
   {:items [74]
    :op #(+ 3 %) :divisor 17 :targets [0 1]}])

(def monkeys2
  [{:items [63 57]
    :op #(* 11 %) :divisor 7 :targets [6 2]}
   {:items [82, 66, 87, 78, 77, 92, 83]
    :op inc :divisor 11 :targets [5 0]}
   {:items [97, 53, 53, 85, 58, 54]
    :op #(* 7 %) :divisor 13 :targets [4 3]}
   {:items [50]
    :op #(+ 3 %) :divisor 3 :targets [1 7]}
   {:items [64, 69, 52, 65, 73]
    :op #(+ 6 %) :divisor 17 :targets [3 7]}
   {:items [57, 91, 65]
    :op #(+ 5 %) :divisor 2 :targets [0 6]}
   {:items [67, 91, 84, 78, 60, 69, 99, 83]
    :op #(* % %) :divisor 5 :targets [2 4]}
   {:items [58, 78, 69, 65]
    :op #(+ 7 %) :divisor 19 :targets [5 1]}])


(defn execute-monkey
  [divide? monkeys idx]
  (let [modulus (reduce lcm (map :divisor monkeys))
        {:keys [op divisor targets items]} (get monkeys idx)]
    (reduce (fn [acc item]
              (let [item' (bigint (op item))
                    item'' (if divide? (bigint (/ item' 3)) (mod item' modulus))
                    to-monkey (if (zero? (mod item'' divisor)) (first targets) (second targets))]
                (-> acc
                    (update-in [idx :items] rest)
                    (update-in [idx :count] (fnil inc 0))
                    (update-in [to-monkey :items] conj item''))))
            monkeys
            items)))

(defn execute-cycles [n-cycles monkeys divide?]
  (let [n-monkeys (count monkeys)]
    (reduce (fn [acc v]
              (execute-monkey divide? acc v))
            monkeys
            (take (* n-monkeys n-cycles) (cycle (range n-monkeys))))))

(comment

  (->> (execute-cycles 20 monkeys2 true)
       (map :count)
       sort
       reverse
       (take 2)
       (reduce *))

  (->> (execute-cycles 10000 monkeys2 false)
       (map :count)
       sort
       reverse
       (take 2)
       (reduce *)))


                

  
  