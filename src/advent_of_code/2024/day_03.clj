(ns advent-of-code.2024.day-03
  (:require
   [clojure.string :as str]))

(defn part-1
  [input-file-path]
  (->> (slurp input-file-path)
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (reduce (fn [acc [_ op-1 op-2]]
                 (let  [op-1 (Integer/parseInt op-1)
                        op-2 (Integer/parseInt op-2)]
                   (+ acc (* op-1 op-2))))
               0)))

(defn part-2
  [input-file-path]
  (->> (slurp input-file-path)
       (re-seq #"do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)")
       (reduce (fn [[acc is-enabled] [opcode op-1 op-2]]
                 (cond
                   (str/starts-with? opcode "mul") (if is-enabled
                                                     (let [op-1 (Integer/parseInt op-1)
                                                           op-2 (Integer/parseInt op-2)]
                                                       [(+ acc (* op-1 op-2)) is-enabled])
                                                     [acc is-enabled])
                   (= opcode "do()") [acc true]
                   (= opcode "don't()") [acc false]
                   :else (throw (UnsupportedOperationException.))))
               [0 true])
       (first)))

(comment
  (part-1 "input/2024/day-03.txt")
  ;;=> 170068701

  (part-2 "input/2024/day-03.txt")
  ;;=> 78683433
  :rcf)
