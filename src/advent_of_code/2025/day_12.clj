(ns advent-of-code.2025.day-12
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (loop [[line & remaining-lines]
         (->> (slurp input-file-path)
              (str/split-lines))

         shapes
         {}

         regions
         []]
    (cond
      (nil? line)
      {:shapes shapes
       :regions regions}

      (re-find #"^(\d+):" line)
      (let [[_ index]
            (re-find #"^(\d+):" line)

            index
            (Integer/parseInt index)

            shape
            (take-while #(-> % str/blank? not)
                        remaining-lines)

            new-remaining-lines
            (drop (-> shape count inc)
                  remaining-lines)

            new-shapes
            (assoc shapes index shape)]
        (recur new-remaining-lines
               new-shapes
               regions))

      (re-find #"^(\d+)x(\d+):" line)
      (let [[_ w h]
            (re-find #"^(\d+)x(\d+):" line)

            w (Integer/parseInt w)
            h (Integer/parseInt h)

            quantity-of-shapes
            (->> (str/split line #"[:\s+]")
                 (drop 2)
                 (map Integer/parseInt)
                 (map-indexed vector)
                 (into {}))

            new-regions
            (conj regions
                  {:width w
                   :height h
                   :quantity-of-shapes quantity-of-shapes})]
        (recur remaining-lines
               shapes
               new-regions)))))

(defn part-1
  [input-file-path]
  (let [{:keys [shapes regions]}
        (read-input input-file-path)

        shape-areas
        (->> shapes
             (map (fn [[index shape]]
                    (let [shape-area
                          (->> (str/join shape)
                               (filter #(= % \#))
                               (count))]
                      [index shape-area])))
             (into {}))]
    (count
     (for [{:keys [width height quantity-of-shapes] :as region}
           regions

           :let [region-area
                 (* width height)

                 required-area
                 (->> quantity-of-shapes
                      (map (fn [[index quantity]]
                             (let [shape-area
                                   (get shape-areas index)]
                               (* quantity shape-area))))
                      (reduce + 0))]

           :when (<= required-area region-area)]
       region))))

(comment

  (part-1 "input/2025/day-12.example.txt")
  ;;=> 3

  :rcf)

(comment

  (part-1 "input/2025/day-12.txt")
  ;;=> 487

  :rcf)
