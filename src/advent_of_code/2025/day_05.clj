(ns advent-of-code.2025.day-05
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (let [[id-ranges available-ids]
        (->> (slurp input-file-path)
             (str/split-lines)
             (split-with (complement str/blank?)))]
    {:ranges
     (mapv (fn [id-range]
             (let [[id-low id-high]
                   (str/split id-range #"-")]
               [(Long/parseLong id-low)
                (Long/parseLong id-high)]))
           id-ranges)

     :available
     (->> available-ids
          (remove str/blank?)
          (mapv Long/parseLong))}))

(defn part-1
  [input-file-path]
  (let [{:keys [ranges available]}
        (read-input input-file-path)]
    (->> available
         (filter (fn [ingredient-id]
                   (first
                    (for [[low high] ranges
                          :when (and (>= ingredient-id low)
                                     (<= ingredient-id high))]
                      ingredient-id))))
         (count))))

(defn part-2
  [input-file-path]
  (let [{:keys [ranges]}
        (read-input input-file-path)

        ranges
        (sort-by identity
                 (fn [[low1 high1]
                      [low2 high2]]
                   (cond
                     (< low1 low2)
                     -1

                     (> low1 low2)
                     1

                     (< high1 high2)
                     -1

                     (> high1 high2)
                     1

                     :else
                     0))
                 ranges)

        merged-ranges
        (loop [[range' & remaining-ranges]
               ranges
               merged-ranges
               []]
          (if (nil? range')
            merged-ranges
            (let [[merged-range
                   new-remaining-ranges]
                  (reduce (fn [[[low1 high1 :as range1] remaining-ranges]
                               [low2 high2 :as range2]]
                            (cond
                              (and (>= low1 low2)
                                   (<= high1 high2))
                              [range2 remaining-ranges]

                              (and (<= low1 low2)
                                   (>= high1 high2))
                              [range1 remaining-ranges]

                              (and (<= low1 low2)
                                   (>= high1 low2)
                                   (<= high1 high2))
                              [[low1 high2]
                               remaining-ranges]

                              (and (>= high1 high2)
                                   (>= low1 low2)
                                   (<= low1 high2))
                              [[low2 high1]
                               remaining-ranges]

                              :else
                              [range1 (conj remaining-ranges range2)]))
                          [range' []]
                          remaining-ranges)

                  new-merged-ranges
                  (conj merged-ranges merged-range)]
              (recur new-remaining-ranges
                     new-merged-ranges))))]
    (reduce (fn [acc [low high]]
              (+ acc
                 (- high low)
                 1))
            0
            merged-ranges)))

(comment

  (part-1 "input/2025/day-05.example.txt")
  ;;=> 3

  (part-2 "input/2025/day-05.example.txt")
  ;;=> 14

  :rcf)

(comment

  (part-1 "input/2025/day-05.txt")
  ;;=> 735

  (part-2 "input/2025/day-05.txt")
  ;;=> 344306344403172

  :rcf)
