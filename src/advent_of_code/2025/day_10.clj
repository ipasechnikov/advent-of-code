(ns advent-of-code.2025.day-10
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map (fn [line]
              (str/split line #"\s")))
       (map (fn [line-elements]
              (let [indicator-lights
                    (->> (first line-elements)
                         (filter #(and (not= % \[)
                                       (not= % \])))
                         (map #(= % \#)))

                    buttons
                    (->> line-elements
                         (drop 1)
                         (drop-last)
                         (map read-string)
                         (map vec))

                    joltage
                    (-> (last line-elements)
                        (str/replace #"\{" "[")
                        (str/replace #"\}" "]")
                        (read-string))]
                {:indicator-lights indicator-lights
                 :buttons buttons
                 :joltage joltage})))))


;; =================================================================================================
;; PART 1
;; =================================================================================================

(defn button->int
  [button]
  (reduce (fn [acc indicator-light-idx]
            (+ acc
               (bit-shift-left 1 indicator-light-idx)))
          0
          button))

(defn indicator-lights->int
  [indicator-lights]
  (->> indicator-lights
       (map-indexed vector)
       (reduce (fn [acc [idx indicator-light]]
                 (+ acc
                    (bit-shift-left
                     (if indicator-light 1 0)
                     idx)))
               0)))

(defn sublists
  [n coll]
  (let [coll-vec (vec coll)]
    (for [x (->> (count coll-vec)
                 (bit-shift-left 1)
                 (range))

          :when (= (Integer/bitCount x) n)]
      (vec
       (for [bit-idx (-> coll-vec count range)
             :when (bit-test x bit-idx)]
         (nth coll-vec bit-idx))))))

(defn part-1
  [input-file-path]
  (->> (for [{:keys [indicator-lights buttons]}
             (read-input input-file-path)

             :let [indicator-target-value
                   (indicator-lights->int indicator-lights)

                   button-values
                   (map button->int buttons)]]
         (first
          (for [n (iterate inc 1)
                :let [button-sublists
                      (sublists n button-values)]

                :when (first
                       (filter (fn [button-sublist]
                                 (= (reduce bit-xor 0 button-sublist)
                                    indicator-target-value))
                               button-sublists))]
            n)))
       (apply +)))


;; =================================================================================================
;; PART 2
;; =================================================================================================

(defn order-rows
  [m]
  (->> m
       (sort-by (fn [row]
                  (let [leading-zeros-count
                        (->> row
                             (take-while zero?)
                             (count))]
                    leading-zeros-count)))
       (vec)))

(defn row-echelon-form
  [m]
  (->> m
       (count)
       (range)
       (reduce (fn [acc row-idx]
                 (order-rows
                  (let [row
                        (nth acc row-idx)

                        [pivot-idx pivot-val]
                        (->> row
                             (map-indexed vector)
                             (filter (fn [[_idx v]]
                                       (-> v zero? not)))
                             (first))]
                    (if-not pivot-idx
                      acc
                      (reduce (fn [acc' row-idx']
                                (let [row' (nth acc row-idx')
                                      val' (nth row' pivot-idx)]
                                  (cond-> acc'
                                    (-> val' zero? not)
                                    (update row-idx'
                                            (fn [row']
                                              (let [factor (/ val' pivot-val)]
                                                (->> row
                                                     (map #(* factor %))
                                                     (mapv - row'))))))))
                              acc
                              (range (inc row-idx)
                                     (count m)))))))
               (order-rows m))))

(defn reduced-row-echelon-form
  [ref']
  (->> ref'
       (map-indexed vector)
       (reduce (fn [acc [row-idx row]]
                 (let [[pivot-idx pivot-val]
                       (->> row
                            (map-indexed vector)
                            (filter (fn [[_idx v]]
                                      (-> v zero? not)))
                            (first))]
                   (if (nil? pivot-val)
                     acc
                     (cond-> acc
                       (not= 1 pivot-val)
                       (update row-idx
                               (fn [row]
                                 (let [factor (/ 1 pivot-val)]
                                   (->> row
                                        (mapv #(* factor %))))))

                       (->> row
                            (drop (inc pivot-idx))
                            (filter #(-> % zero? not))
                            (first))
                       (update row-idx
                               (fn [row]
                                 (let [rows-below
                                       (drop (inc row-idx) acc)]
                                   (reduce (fn [row col-idx]
                                             (if-let [row-below
                                                      (->> rows-below
                                                           (filter (fn [row-below]
                                                                     (->> row-below
                                                                          (take col-idx)
                                                                          (every? #(zero? %)))))
                                                           (filter #(-> (nth % col-idx) zero? not))
                                                           (first))]
                                               (let [factor
                                                     (/ (nth row col-idx)
                                                        (nth row-below col-idx))]
                                                 (->> row-below
                                                      (map #(* factor %))
                                                      (mapv - row)))
                                               row))
                                           row
                                           (range (inc pivot-idx)
                                                  (count row))))))))))
               ref')))

(defn pivot-indexes
  [ref']
  (vec
   (for [row (map #(drop-last 1 %) ref')]
     (->> row
          (map-indexed vector)
          (filter (fn [[_col-idx val]]
                    (-> val zero? not)))
          (first)
          (first)))))

(defn free-indexes
  [rref]
  (let [button-matrix
        (mapv #(drop-last 1 %)
              rref)

        pivot-indexes-set
        (-> rref pivot-indexes set)]
    (->> button-matrix first count range
         (filter #(not (contains? pivot-indexes-set %)))
         (vec))))

(defn back-substitution
  [rref free-map]
  (let [pivots
        (pivot-indexes rref)]
    (loop [button-presses
           free-map

           [row-with-index & remaining-rows]
           (->> rref
                (map-indexed vector)
                (reverse))]
      (if (nil? row-with-index)
        button-presses
        (let [[row-idx row]
              row-with-index

              pivot-idx
              (nth pivots row-idx)]
          (if (nil? pivot-idx)
            (recur button-presses
                   remaining-rows)
            (let [new-button-presses
                  (assoc button-presses
                         pivot-idx
                         (->> row
                              (map-indexed vector)
                              (reduce (fn [acc [col-idx col-val]]
                                        (cond
                                          (contains? button-presses col-idx)
                                          (- acc
                                             (* (get button-presses col-idx)
                                                col-val))

                                          (= pivot-idx col-idx)
                                          acc

                                          :else
                                          (+ acc col-val)))
                                      0)))]
              (recur new-button-presses
                     remaining-rows))))))))

(defn sublists2
  [n coll]
  (if (-> n pos? not)
    [[]]
    (for [x coll
          sl (sublists2 (dec n) coll)]
      (conj sl x))))

(defn minimize
  [rref]
  (let [free (free-indexes rref)]
    (if (empty? free)
      (->> (back-substitution rref {})
           (vals)
           (reduce +))
      (->> (/ 500 (count free)) ; The more free variables there are, the less combinations we check.
                                ; It's a dirty hack to improve performance
                                ; 500 is just a magic number
           (range)
           (sublists2 (count free))
           (pmap (fn [free-sublist]
                   (->> (map vector free free-sublist)
                        (into {})
                        (back-substitution rref))))
           (map vals)
           (filter (fn [button-presses-counts]
                     (every? (fn [button-presses-count]
                               (and (-> button-presses-count neg? not)
                                    (integer? button-presses-count)))
                             button-presses-counts)))
           (map (fn [button-presses-counts]
                  (reduce + button-presses-counts)))
           (reduce min ##Inf)))))

(defn part-2
  [input-file-path]
  (let [input
        (read-input input-file-path)

        processed!
        (atom 0)]
    (->> input
         (map-indexed vector)
         (map (fn [[idx {:keys [buttons joltage]}]]
                (let [button-matrix
                      (vec
                       (for [[joltage-idx _joltage-val]
                             (map-indexed vector joltage)]
                         (->> buttons
                              (map-indexed vector)
                              (reduce (fn [acc [button-idx button]]
                                        (if (->> button
                                                 (filter #(= % joltage-idx))
                                                 (first))
                                          (assoc acc button-idx 1)
                                          acc))
                                      (vec
                                       (repeat (count buttons) 0))))))

                      augmented-matrix
                      (mapv conj button-matrix joltage)

                      rref
                      (->> augmented-matrix
                           (row-echelon-form)
                           (reduced-row-echelon-form))

                      result
                      (minimize rref)]

                  ;; Code for debugging
                  ;; Can be used to find bounds for free variables if solution does not diverge
                  (when (= result ##Inf)
                    (throw
                     (ex-info "Bad solution"
                              {:idx idx

                               ;; Input machine
                               :buttons buttons
                               :joltage joltage

                               ;; Liner system parameters
                               :rref rref
                               :augmented-matrix augmented-matrix
                               :pivot-indexes (pivot-indexes rref)
                               :free-indexes (free-indexes rref)})))

                  ;; Track the progress
                  (swap! processed! inc)
                  (println "Processed" @processed! "of" (count input))
                  (println idx ":" result)

                  result)))
         (reduce + 0))))

(comment

  (part-1 "input/2025/day-10.example.txt")
  ;;=> 7

  (part-2 "input/2025/day-10.example.txt")
  ;;=> 33

  :rcf)

(comment

  (part-1 "input/2025/day-10.txt")
  ;;=> 479

  (part-2 "input/2025/day-10.txt")
  ;;=> 19574N

  :rcf)
