(ns advent-of-code.2025.day-02
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (let [strs
        (-> (slurp input-file-path)
            (str/trim)
            (str/split #"[,-]"))]
    (->> strs
         (map #(Long/parseLong %))
         (partition 2))))

(defn part-1
  [input-file-path]
  (->> (read-input input-file-path)
       (map (fn [[s e]]
              (range s (inc e))))
       (apply concat)
       (reduce (fn [acc id']
                 (let [id-str
                       (str id')

                       invalid-id?
                       (when (-> id-str count even?)
                         (->> id-str
                              (split-at (quot (count id-str) 2))
                              (apply =)))]
                   (cond-> acc
                     invalid-id?
                     (+ id'))))
               0)))

(defn part-2
  [input-file-path]
  (->> (read-input input-file-path)
       (map (fn [[s e]]
              (range s (inc e))))
       (pmap (fn [ids-range]
               (reduce (fn [acc id']
                         (let [id-str
                               (str id')

                               invalid-id?
                               (->> (count id-str)
                                    (dec)
                                    (range)
                                    (some (fn [idx]
                                            (let [len (inc idx)]
                                              (when (zero?
                                                     (rem (count id-str)
                                                          len))
                                                (->> id-str
                                                     (partition len)
                                                     (set)
                                                     (count)
                                                     (= 1)))))))]
                           (cond-> acc
                             invalid-id?
                             (+ id'))))
                       0
                       ids-range)))
       (apply +)))

(comment

  (part-1 "input/2025/day-02.example.txt")
  ;;=> 1227775554

  (part-2 "input/2025/day-02.example.txt")
  ;;=> 4174379265

  :rcf)

(comment

  (part-1 "input/2025/day-02.txt")
  ;;=> 38158151648

  (part-2 "input/2025/day-02.txt")
  ;;=> 45283684555

  :rcf)
