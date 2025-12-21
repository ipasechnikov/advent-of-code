(ns advent-of-code.2024.day-01
  (:require
   [clojure.string :as str]))

(defn parse-input-file
  [input-file-path]
  (let [input-rows (map #(str/split % #"\s+")
                        (str/split-lines (slurp input-file-path)))
        input-lst1 (map #(Integer/parseInt (first %)) input-rows)
        input-lst2 (map #(Integer/parseInt (second %)) input-rows)]
    [input-lst1 input-lst2]))

(defn part-1
  [input-file-path]
  (let [[input-lst1 input-lst2] (parse-input-file input-file-path)
        sorted-input-lst1 (sort input-lst1)
        sorted-input-lst2 (sort input-lst2)
        zipped-input-lsts (map vector sorted-input-lst1 sorted-input-lst2)]
    (reduce (fn [acc [x y]]
              (+ acc (abs (- x y))))
            0
            zipped-input-lsts)))

(defn part-2
  [input-file-path]
  (let [[input-lst1 input-lst2] (parse-input-file input-file-path)
        input-lst2-freq (reduce (fn [acc v]
                                  (update acc v (fnil inc 0)))
                                {}
                                input-lst2)]
    (reduce (fn [acc v]
              (let [v-freq (get input-lst2-freq v 0)]
                (+ acc (* v v-freq))))
            0
            input-lst1)))

(comment
  (part-1 "input/2024/day-01.txt")
  ;;=> 2113135

  (part-2 "input/2024/day-01.txt")
  ;;=> 19097157
  :rcf)
