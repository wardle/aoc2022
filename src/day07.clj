(ns day07
  (:require [clojure.core.match :as m]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [{:keys [current-path] :as ctx} line]
  (m/match (str/split line #"\s")
    ["dir" _] ctx                                           ;; NOP
    ["$" "cd" "/"] (assoc ctx :current-path [""])
    ["$" "cd" ".."] (update ctx :current-path pop)
    ["$" "cd" dir] (let [path (conj current-path dir)]
                     (-> ctx
                         (assoc :current-path path)
                         (update :dirs conj (str (str/join "/" path) "/"))))
    ["$" "ls"] ctx                                          ;; NOP
    [size filename] (update ctx :files conj [(str/join "/" (conj current-path filename)) (parse-long size)])
    :else (update ctx :errors conj {:error "Unknown command: " :line line})))

(defn parse-commands
  ([lines]
   (parse-commands {:current-path []} lines))
  ([ctx lines]
   (if-let [line (first lines)]
     (recur (parse-line ctx line) (rest lines))
     (dissoc ctx :current-path))))

(defn size
  "Given a sequence of files (tuples of path and size), determine total size of the
  specified directory."
  [files dir]
  (reduce (fn [acc [path size]]
            (if (str/starts-with? path dir)
              (+ acc size)
              acc)) 0 files))

(comment
  (def input (str/split-lines (slurp (io/resource "day07.txt"))))

  ;; calculate total size of all directories with a size at least 100,000
  (let [{:keys [files dirs]} (parse-commands input)]
    (->> (reduce (fn [acc dir]
                   (conj acc {:dir dir :size (size files dir)})) [] dirs)
         (filter #(<= (:size %) 100000))
         (sort-by :size)
         (map :size)
         (reduce +)))

  ;; calculate total used space
  (def used-space (let [{:keys [files]} (parse-commands input)]
                    (reduce (fn [acc [_ size]]
                              (+ acc size)) 0 files)))
  used-space
  ;; calculate how much space must be created to install update
  (def filesystem 70000000)
  (def unused-space (- filesystem used-space))
  unused-space
  (def required-space 30000000)
  (def to-be-deleted (- required-space unused-space))
  to-be-deleted

  ;; find smallest directory that would free up enough space
  (let [{:keys [files dirs]} (parse-commands input)]
    (->> (reduce (fn [acc dir]
                   (conj acc {:dir dir :size (size files dir)})) [] dirs)
         (filter #(>= (:size %) to-be-deleted))
         (sort-by :size)
         first)))