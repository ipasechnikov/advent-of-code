(ns advent-of-code.2025.day-11
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map #(str/split % #"[:\s]"))
       (map #(remove str/blank? %))
       (map (fn [[device & outputs]]
              [device outputs]))
       (into {})))

(defn part-1
  [input-file-path]
  (let [devices (read-input input-file-path)]
    (count
     (loop [[curr-path & remaining-queue-paths]
            ['("you")]

            paths
            []]
       (if (nil? curr-path)
         paths
         (let [[curr-device & _]
               curr-path

               outputs
               (get devices curr-device)

               new-remaining-queue-paths
               (into remaining-queue-paths
                     (map #(conj curr-path %)
                          outputs))

               new-paths
               (cond-> paths
                 (= curr-device "out")
                 (conj curr-path))]
           (recur new-remaining-queue-paths
                  new-paths)))))))

(def count-paths
  (memoize
   (fn [devices start end]
     (if (= start end)
       1
       (->> (for [output (get devices start)]
              (count-paths devices output end))
            (reduce + 0))))))

(defn part-2
  [input-file-path]
  (let [devices (read-input input-file-path)]
    (+
     (* (count-paths devices "svr" "fft")
        (count-paths devices "fft" "dac")
        (count-paths devices "dac" "out"))

     (* (count-paths devices "svr" "dac")
        (count-paths devices "dac" "fft")
        (count-paths devices "fft" "out")))))

(comment

  (part-1 "input/2025/day-11.example-1.txt")
  ;;=> 5

  (part-2 "input/2025/day-11.example-2.txt")
  ;;=> 2

  :rcf)

(comment

  (part-1 "input/2025/day-11.txt")
  ;;=> 758

  (part-2 "input/2025/day-11.txt")
  ;;=> 490695961032000

  :rcf)
