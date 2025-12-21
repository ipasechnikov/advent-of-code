(ns advent-of-code.2025.day-06
  (:require
   [clojure.string :as str]))

(defn part-1
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (map #(remove str/blank? %))
       (map #(map-indexed vector %))
       (apply concat)
       (group-by first)
       (vals)
       (map #(map second %))
       (map #(let [[split1 split2]
                   (->> (reverse %)
                        (split-at 1))]
               {:operator (case (first split1)
                            "+" +
                            "-" -
                            "*" *
                            "/" /)
                :operands (->> (reverse split2)
                               (map Integer/parseInt))}))
       (reduce (fn [acc {:keys [operator operands]}]
                 (+ acc
                    (apply operator operands)))
               0)))

(defn part-2
  [input-file-path]
  (let [lines
        (->> (slurp input-file-path)
             (str/split-lines))

        raw-operators
        (->> (-> lines reverse first)
             (re-seq #"[+\-*/]\s+"))

        columns-sizes
        (-> (mapv #(-> % count dec) raw-operators)
            (update (-> raw-operators count dec)
                    inc))

        raw-operands
        (->> lines
             (drop-last)
             (map (fn [line]
                    (loop [remaining-line
                           line

                           [column-size & remaining-column-sizes]
                           columns-sizes

                           raw-operands
                           []]
                      (if (-> remaining-line count zero?)
                        raw-operands
                        (let [[operand new-remaining-line]
                              (split-at column-size remaining-line)

                              raw-operand
                              (str/join operand)

                              new-remaining-line
                              (->> new-remaining-line
                                   (drop 1)
                                   (str/join))]
                          (recur
                           new-remaining-line
                           remaining-column-sizes
                           (conj raw-operands
                                 raw-operand))))))))

        operands
        (->> raw-operands
             (map #(map-indexed vector %))
             (apply concat)
             (group-by first)
             (vec)
             (sort-by first)
             (vals)
             (map #(map second %))
             (map (fn [operand-group]
                    (->> operand-group
                         (map #(map-indexed vector %))
                         (apply concat)
                         (group-by first)
                         (vals)
                         (map #(map second %))
                         (map #(-> % str/join str/trim Long/parseLong))
                         (reverse)))))

        operators
        (->> raw-operators
             (map str/trim)
             (map (fn [operator]
                    (case operator
                      "+" +
                      "-" -
                      "*" *
                      "/" /))))]
    (->> (map (fn [operator operands]
                (apply operator operands))
              operators
              operands)
         (apply +))))

(comment

  (part-1 "input/2025/day-06.example.txt")
  ;;=> 4277556

  (part-2 "input/2025/day-06.example.txt")
  ;;=> 3263827

  :rcf)

(comment

  (part-1 "input/2025/day-06.txt")
  ;;=> 3525371263915

  (part-2 "input/2025/day-06.txt")
  ;;=> 6846480843636

  :rcf)
