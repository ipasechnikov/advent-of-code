(ns advent-of-code.2025.day-08
  (:require
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map #(str/split % #","))
       (map #(mapv Integer/parseInt %))))

(defn part-1
  [input-file-path connections-count]
  (let [boxes
        (read-input input-file-path)

        closest-boxes
        (->> (for [[x1 y1 z1 :as box1] boxes
                   [x2 y2 z2 :as box2] boxes
                   :when (not= box1 box2)
                   :let  [distance
                          (+ (* (- x1 x2) (- x1 x2))
                             (* (- y1 y2) (- y1 y2))
                             (* (- z1 z2) (- z1 z2)))]]
               [#{box1 box2} distance])
             (set)
             (sort-by second)
             (map first)
             (map vec))]
    (->> closest-boxes
         (take connections-count)
         (reduce (fn [acc [box1 box2]]
                   (let [box1-circuit
                         (get acc box1)

                         box2-circuit
                         (get acc box2)]
                     (if-not (= box1-circuit box2-circuit)
                       (let [new-circuit (into box1-circuit box2-circuit)]
                         (reduce (fn [new-acc box]
                                   (assoc new-acc box new-circuit))
                                 acc
                                 new-circuit))
                       acc)))
                 (->> boxes
                      (map (fn [box] {box #{box}}))
                      (apply merge)))
         (vals)
         (set)
         (map count)
         (sort >)
         (take 3)
         (apply *))))

(defn part-2
  [input-file-path]
  (let [boxes
        (read-input input-file-path)

        closest-boxes
        (->> (for [[x1 y1 z1 :as box1] boxes
                   [x2 y2 z2 :as box2] boxes
                   :when (not= box1 box2)
                   :let  [distance
                          (+ (* (- x1 x2) (- x1 x2))
                             (* (- y1 y2) (- y1 y2))
                             (* (- z1 z2) (- z1 z2)))]]
               [#{box1 box2} distance])
             (set)
             (sort-by second)
             (map first)
             (map vec))

        last-boxes!
        (atom [nil nil])]
    (reduce (fn [acc [box1 box2]]
              (let [box1-circuit
                    (get acc box1)

                    box2-circuit
                    (get acc box2)]
                (if-not (= box1-circuit box2-circuit)
                  (let [new-circuit (into box1-circuit box2-circuit)]
                    (reset! last-boxes! [box1 box2])
                    (reduce (fn [new-acc box]
                              (assoc new-acc box new-circuit))
                            acc
                            new-circuit))
                  acc)))
            (->> boxes
                 (map (fn [box] {box #{box}}))
                 (apply merge))
            closest-boxes)
    (let [[[x1 _ _] [x2 _ _]]
          @last-boxes!]
      (* x1 x2))))

(comment

  (part-1 "input/2025/day-08.example.txt" 10)
  ;;=> 40

  (part-2 "input/2025/day-08.example.txt")
  ;;=> 25272

  :rcf)

(comment

  (part-1 "input/2025/day-08.txt" 1000)
  ;;=> 24360

  (part-2 "input/2025/day-08.txt")
  ;;=> 2185817796

  :rcf)
