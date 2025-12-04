(ns advent-of-code.2025.day-4
  (:require
   [clojure.set :refer [difference intersection]]
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map-indexed vector)
       (map (fn [[y line]]
              (for [[x ch]
                    (map-indexed vector line)
                    :when (= ch \@)]
                [x y])))
       (apply concat)
       (into #{})))

(defn part-1
  [input-file-path]
  (let [rolls-of-paper-coords
        (read-input input-file-path)]
    (count
     (for [[x y :as roll-of-paper-coord]
           rolls-of-paper-coords

           :let [adjacent-coords
                 (set
                  (for [i (range -1 2)
                        j (range -1 2)
                        :let [adjacent-coord
                              [(+ x i) (+ y j)]]
                        :when (not= adjacent-coord
                                    roll-of-paper-coord)]
                    adjacent-coord))]

           :when (-> (intersection adjacent-coords
                                   rolls-of-paper-coords)
                     (count)
                     (< 4))]
       roll-of-paper-coord))))

(defn part-2
  [input-file-path]
  (loop [rolls-of-paper-coords
         (read-input input-file-path)

         total-removed-count
         0]
    (let [removed-rolls-of-paper-coords
          (set
           (for [[x y :as roll-of-paper-coord]
                 rolls-of-paper-coords

                 :let [adjacent-coords
                       (set
                        (for [i (range -1 2)
                              j (range -1 2)
                              :let [adjacent-coord
                                    [(+ x i) (+ y j)]]
                              :when (not= adjacent-coord
                                          roll-of-paper-coord)]
                          adjacent-coord))]

                 :when (-> (intersection adjacent-coords
                                         rolls-of-paper-coords)
                           (count)
                           (< 4))]
             roll-of-paper-coord))

          removed-rolls-of-paper-coords-count
          (count removed-rolls-of-paper-coords)]
      (if (zero? removed-rolls-of-paper-coords-count)
        total-removed-count
        (recur (difference rolls-of-paper-coords
                           removed-rolls-of-paper-coords)
               (+ total-removed-count
                  removed-rolls-of-paper-coords-count))))))

(comment

  (part-1 "input/2025/day-4.example.txt")
  ;;=> 13

  (part-2 "input/2025/day-4.example.txt")
  ;;=> 43

  :rcf)

(comment

  (part-1 "input/2025/day-4.txt")
  ;;=> 1464

  (part-2 "input/2025/day-4.txt")
  ;;=> 8409

  :rcf)
