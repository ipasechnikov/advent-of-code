(ns advent-of-code.2024.day-02
  (:require
   [clojure.string :as str]))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn parse-input-file
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map #(str/split % #"\s"))
       (map #(map parse-int %))))

(defn find-prev-level-index
  [curr-level-index skip-level-indices]
  (loop [prev-level-index (dec curr-level-index)]
    (if (contains? skip-level-indices prev-level-index)
      (recur (dec prev-level-index))
      prev-level-index)))

(defn find-next-level-index
  [curr-level-index skip-level-indices]
  (loop [next-level-index (inc curr-level-index)]
    (if (contains? skip-level-indices next-level-index)
      (recur (inc next-level-index))
      next-level-index)))

(defn is-safe-level?
  [report curr-level-index prev-level-index next-level-index]
  (let [curr-level (nth report curr-level-index)
        prev-level (nth report prev-level-index nil)
        next-level (nth report next-level-index nil)
        prev-diff (and prev-level
                       (- curr-level prev-level))
        curr-diff (and next-level
                       (- next-level curr-level))]
    (or (nil? curr-diff)
        (and (>= (abs curr-diff) 1)
             (<= (abs curr-diff) 3)
             (or (nil? prev-diff)
                 (= (< prev-diff 0) (< curr-diff 0)))))))

(defn next-iter-states
  [report curr-level-index skip-level-indices]
  (let [prev-level-index (find-prev-level-index curr-level-index skip-level-indices)
        next-level-index (find-next-level-index curr-level-index skip-level-indices)]
    (cond
      (>= next-level-index (count report)) []
      (is-safe-level? report curr-level-index prev-level-index next-level-index) [[next-level-index skip-level-indices]]
      (neg? prev-level-index) [[curr-level-index (conj skip-level-indices next-level-index)]
                               [next-level-index (conj skip-level-indices curr-level-index)]]
      :else [[curr-level-index (conj skip-level-indices prev-level-index)]
             [curr-level-index (conj skip-level-indices next-level-index)]
             [prev-level-index (conj skip-level-indices curr-level-index)]])))

(defn is-safe-with-problem-dampener?
  ([report]
   (is-safe-with-problem-dampener? report 0 0 #{}))
  ([report levels-to-tolerate]
   (is-safe-with-problem-dampener? report levels-to-tolerate 0 #{}))
  ([report levels-to-tolerate curr-level-index skip-level-indices]
   (and (<= (count skip-level-indices) levels-to-tolerate)
        (let [next-states (next-iter-states report curr-level-index skip-level-indices)]
          (or (empty? next-states)
              (loop [[next-state & remaining-states] next-states]
                (and (some? next-state)
                     (or (let [[new-curr-level-index new-skip-level-indices] next-state]
                           (is-safe-with-problem-dampener? report levels-to-tolerate new-curr-level-index new-skip-level-indices))
                         (recur remaining-states)))))))))

(defn part-1
  [input-file-path]
  (->> (parse-input-file input-file-path)
       (filter is-safe-with-problem-dampener?)
       (count)))

(defn part-2
  [input-file-path]
  (->> (parse-input-file input-file-path)
       (filter #(is-safe-with-problem-dampener? % 1))
       (count)))

(comment
  (part-1 "input/2024/day-02.txt")
  ;;=> 202

  (part-2 "input/2024/day-02.txt")
  ;;=> 271
  :rcf)
