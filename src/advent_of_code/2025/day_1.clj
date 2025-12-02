(ns advent-of-code.2025.day-1
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map (fn [line]
              (let [[rotation-dir rotation-mag]
                    (split-at 1 line)]
                {:dir (str/join rotation-dir)
                 :mag (-> (str/join rotation-mag)
                          (Integer/parseInt))})))))

(defn part-1
  [input-file-path]
  (let [rotations (read-input input-file-path)]
    (loop [arrow-pos
           50

           arrow-pos-zero-count
           0

           [{:keys [dir mag] :as rotation} & remaining-rotations]
           rotations]
      (if (nil? rotation)
        arrow-pos-zero-count
        (let [new-arrow-pos
              (case dir
                "L"
                (- arrow-pos mag)

                "R"
                (+ arrow-pos mag))

              new-arrow-pos-normalized
              (mod new-arrow-pos 100)]
          (recur new-arrow-pos-normalized
                 (cond-> arrow-pos-zero-count
                   (zero? new-arrow-pos-normalized)
                   (inc))
                 remaining-rotations))))))

(defn part-2
  [input-file-path]
  (let [rotations (->> (read-input input-file-path)
                       (reduce (fn [acc {:keys [dir mag]}]
                                 (into acc
                                       (repeat mag {:dir dir
                                                    :mag 1})))
                               []))]
    (loop [arrow-pos
           50

           arrow-pos-zero-count
           0

           [{:keys [dir mag] :as rotation} & remaining-rotations]
           rotations]
      (if (nil? rotation)
        arrow-pos-zero-count
        (let [new-arrow-pos
              (case dir
                "L"
                (- arrow-pos mag)

                "R"
                (+ arrow-pos mag))

              new-arrow-pos-normalized
              (mod new-arrow-pos 100)]
          (recur new-arrow-pos-normalized
                 (cond-> arrow-pos-zero-count
                   (zero? new-arrow-pos-normalized)
                   (inc))
                 remaining-rotations))))))

(comment

  (part-1 "input/2025/day-1.example.txt")
  ;;=> 3

  (part-2 "input/2025/day-1.example.txt")
  ;;=> 6

  :rcf)

(comment

  (part-1 "input/2025/day-1.txt")
  ;;=> 1029

  (part-2 "input/2025/day-1.txt")
  ;;=> 5892

  :rcf)
