(ns advent-of-code.2025.day-7
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (let [lines
        (->> (slurp input-file-path)
             (str/split-lines))

        not-empty-cells
        (for [[y line]
              (map-indexed vector lines)

              [x ch]
              (map-indexed vector line)

              :when (contains? #{\S \^} ch)]
          [[x y] ch])

        {s \S splitters \^}
        (group-by second not-empty-cells)

        max-x
        (-> lines first count dec)

        max-y
        (-> lines count dec)]
    {:s (-> s first first)
     :max-x max-x
     :max-y max-y
     :splitters (->> splitters
                     (map first))}))

(defn part-1
  [input-file-path]
  (let [{:keys [s max-x max-y splitters]}
        (read-input input-file-path)

        splitters
        (set splitters)]
    (loop [beams
           [s]

           beam-split-count
           0]
      (if (empty? beams)
        beam-split-count
        (let [[new-beams beam-split-count-diff]
              (reduce (fn [[new-beams beam-split-count :as _acc]
                           [x y :as _beam]]
                        (if (contains? splitters [x (inc y)])
                          [(cond-> new-beams
                             (>= (dec x) 0)
                             (conj [(dec x) (inc y)])

                             (<= (inc x) max-x)
                             (conj [(inc x) (inc y)]))
                           (inc beam-split-count)]
                          [(cond-> new-beams
                             (<= (inc y) max-y)
                             (conj [x (inc y)]))
                           beam-split-count]))
                      [[] 0]
                      beams)]
          (recur (set new-beams)
                 (+ beam-split-count beam-split-count-diff)))))))

(defn part-2
  [input-file-path]
  (let [{:keys [s max-y splitters]}
        (read-input input-file-path)

        [s-x s-y]
        s

        start-splitter
        (->> splitters
             (filter (fn [[splitter-x splitter-y]]
                       (and (= splitter-x s-x)
                            (> splitter-y s-y))))
             (first))

        graph
        (loop [[splitter & remaining-splitters]
               [start-splitter]

               visited-splitters
               #{start-splitter}

               graph
               {start-splitter []}]
          (if (nil? splitter)
            graph
            (let [[splitter-x splitter-y]
                  splitter

                  left-splitter
                  (->> splitters
                       (filter (fn [[left-splitter-x left-splitter-y]]
                                 (and (= left-splitter-x (dec splitter-x))
                                      (> left-splitter-y splitter-y))))
                       (first))

                  right-splitter
                  (->> splitters
                       (filter (fn [[right-splitter-x right-splitter-y]]
                                 (and (= right-splitter-x (inc splitter-x))
                                      (> right-splitter-y splitter-y))))
                       (first))

                  new-splitters
                  (cond-> remaining-splitters
                    (and (some? left-splitter)
                         (not (contains? visited-splitters left-splitter)))
                    (conj left-splitter)

                    (and (some? right-splitter)
                         (not (contains? visited-splitters right-splitter)))
                    (conj right-splitter))

                  new-visited-splitters
                  (cond-> visited-splitters
                    (some? left-splitter)
                    (conj left-splitter)

                    (some? right-splitter)
                    (conj right-splitter))

                  left-vertex
                  (or left-splitter
                      [(dec splitter-x) max-y])

                  right-vertex
                  (or right-splitter
                      [(inc splitter-x) max-y])

                  new-graph
                  (update graph
                          splitter
                          (fnil conj [])
                          left-vertex
                          right-vertex)]
              (recur new-splitters
                     new-visited-splitters
                     new-graph))))

        timeline-counts!
        (atom {})]
    (letfn [(timeline-count-from
             [node]
             (if-let [cached-timeline-count
                      (get @timeline-counts! node)]
               cached-timeline-count
               (let [adjacent-nodes
                     (get graph node)

                     timeline-count
                     (if-not adjacent-nodes
                       1
                       (->> adjacent-nodes
                            (map timeline-count-from)
                            (apply +)))]
                 (swap! timeline-counts! assoc node timeline-count)
                 timeline-count)))]
      (timeline-count-from start-splitter))))

(comment

  (part-1 "input/2025/day-7.example.txt")
  ;;=> 21

  (part-2 "input/2025/day-7.example.txt")
  ;;=> 40

  :rcf)

(comment

  (part-1 "input/2025/day-7.txt")
  ;;=> 1651

  (part-2 "input/2025/day-7.txt")
  ;;=> 108924003331749

  :rcf)
