(ns advent-of-code.2024.day-4
  (:require
   [clojure.string :as str]))

(defn has-word-at-positions?
  [letters word positions]
  (loop [[word-letter & remaining-word] word
         [[row col] & remaining-positions] positions]
    (or (nil? word-letter)
        (let [letter (nth (nth letters row nil) col nil)]
          (and (= word-letter letter)
               (recur remaining-word remaining-positions))))))

(defn has-left-down-up-diagonal?
  [letters row col word]
  (let [positions (map (fn [idx] [(- row idx) (+ col idx)])
                       (range (count word)))]
    (or (has-word-at-positions? letters word positions)
        (has-word-at-positions? letters (str/reverse word) positions))))

(defn has-left-up-down-diagonal?
  [letters row col word]
  (let [positions (map (fn [idx] [(+ row idx) (+ col idx)])
                       (range (count word)))]
    (or (has-word-at-positions? letters word positions)
        (has-word-at-positions? letters (str/reverse word) positions))))

(defn has-left-right-horizontal?
  [letters row col word]
  (let [positions (map (fn [idx] [row (+ col idx)])
                       (range (count word)))]
    (or (has-word-at-positions? letters word positions)
        (has-word-at-positions? letters (str/reverse word) positions))))

(defn has-up-down-vertical?
  [letters row col word]
  (let [positions (map (fn [idx] [(+ row idx) col])
                       (range (count word)))]
    (or (has-word-at-positions? letters word positions)
        (has-word-at-positions? letters (str/reverse word) positions))))

(defn part-1
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map #(.toCharArray %))
       (#(for [row (range (count %))
               col (range (count (first %)))]
           [% row col "XMAS"]))
       (reduce (fn [acc args]
                 (conj acc
                       (apply has-left-up-down-diagonal? args)
                       (apply has-left-down-up-diagonal? args)
                       (apply has-left-right-horizontal? args)
                       (apply has-up-down-vertical? args)))
               [])
       (filter true?)
       (count)))

(defn part-2
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map #(.toCharArray %))
       (#(for [row (range (count %))
               col (range (count (first %)))]
           (and (has-left-up-down-diagonal? % row col "MAS")
                (has-left-down-up-diagonal? % (+ row 2) col "MAS"))))
       (filter true?)
       (count)))

(comment
  (part-1 "input/2024/day-4.txt")
  ;;=> 2593

  (part-2 "input/2024/day-4.txt")
  ;;=> 1950
  :rcf)
