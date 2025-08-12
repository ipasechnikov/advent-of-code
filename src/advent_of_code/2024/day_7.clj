(ns advent-of-code.2024.day-7
  (:require
   [clojure.string :as str]))

(defn parse-input-file
  [input-file-path]
  (let [lines (-> input-file-path
                  (slurp)
                  (str/split #"[\n\r]+"))]
    (->> lines
         (map (fn [line] (str/split line #"[:\s]+")))
         (map (fn [line]
                (map (fn [number] (Long/parseLong number))
                     line)))
         (map (fn [[result & operands]] {:result result
                                         :operands operands})))))

(defn operator-combinations
  [operators length]
  (loop [combinations []
         i 0]
    (cond
      (>= i length)
      combinations

      (= i 0)
      (recur
       (map vector operators)
       (inc i))

      :else
      (recur
       (mapcat (fn [operator]
                 (map (fn [combination]
                        (conj combination operator))
                      combinations))
               operators)
       (inc i)))))

(defn calculate-equation [operands operators]
  (assert (= (count operators)
             (-> operands count dec)))
  (assert (>= (count operands) 2))
  (loop [acc (first operands)
         [operand & operands] (rest operands)
         [operator & operators] operators]
    (if (nil? operand)
      acc
      (recur
       (operator acc operand)
       operands
       operators))))

(defn part-1
  [input-file-path]
  (reduce (fn [acc {:keys [result operands]}]
            (if (->> (operator-combinations [+ *] (-> operands count dec))
                     (some #(= result (calculate-equation operands %)))
                     (true?))
              (+ acc result)
              acc))
          0
          (parse-input-file input-file-path)))

(defn part-2
  [input-file-path]
  (reduce (fn [acc {:keys [result operands]}]
            (if (->> (operator-combinations [+ * (fn [a b]
                                                   (Long/parseLong (str a b)))]
                                            (-> operands count dec))
                     (some #(= result (calculate-equation operands %)))
                     (true?))
              (+ acc result)
              acc))
          0
          (parse-input-file input-file-path)))

(comment

  (part-1 "input/2024/day-7.example.txt")
  ;;=> 3749

  (part-2 "input/2024/day-7.example.txt")
  ;;=> 11387

  :rcf)

(comment

  (part-1 "input/2024/day-7.txt")
  ;;=> 21572148763543

  (part-2 "input/2024/day-7.txt")
  ;;=> 581941094529163

  :rcf)
