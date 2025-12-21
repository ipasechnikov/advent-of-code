(ns advent-of-code.2025.day-03
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map (fn [bank]
              (map #(Character/digit % 10) bank)))))

(defn part-1
  [input-file-path]
  (->> (read-input input-file-path)
       (map (fn [bank]
              (->> bank
                   (map-indexed vector)
                   (reduce (fn [[battery1 battery2 :as acc] [index battery]]
                             (cond
                               (and (< index (-> bank count dec))
                                    (> battery battery1))
                               [battery -1]

                               (> battery battery2)
                               [battery1 battery]

                               :else
                               acc))
                           [-1 -1])
                   (str/join)
                   (Integer/parseInt))))
       (apply +)))

(defn part-2
  [input-file-path]
  (let [digits 12]
    (->> (read-input input-file-path)
         (map (fn [bank]
                (->> bank
                     (map-indexed vector)
                     (reduce (fn [turned-on-batteries [battery-index battery]]
                               (reduce (fn [new-turned-on-batteries
                                            [turned-on-battery-index turned-on-battery]]
                                         (if (and (<= (- digits (inc turned-on-battery-index))
                                                      (- (count bank) (inc battery-index)))
                                                  (> battery turned-on-battery))
                                           (reduced
                                            (take digits
                                                  (concat (conj new-turned-on-batteries battery)
                                                          (repeat -1))))
                                           (conj new-turned-on-batteries turned-on-battery)))
                                       []
                                       (map-indexed vector turned-on-batteries)))
                             (repeat digits -1))
                     (str/join)
                     (Long/parseLong))))
         (apply +))))

(comment

  (part-1 "input/2025/day-03.example.txt")
  ;;=> 357

  (part-2 "input/2025/day-03.example.txt")
  ;;=> 3121910778619

  :rcf)

(comment

  (part-1 "input/2025/day-03.txt")
  ;;=> 17155

  (part-2 "input/2025/day-03.txt")
  ;;=> 169685670469164

  :rcf)
