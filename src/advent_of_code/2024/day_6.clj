(ns advent-of-code.2024.day-6
  (:require
   [clojure.string :as str]))

;;;; Common functions for Part 1 and Part 2

(defn parse-input-file
  [input-file-path]
  (let [lines (-> input-file-path
                  (slurp)
                  (str/split-lines))
        map-width (-> lines first count)
        map-height (count lines)]
    (->> (for [[y line] (map-indexed vector lines)
               [x char] (map-indexed vector line)
               :when (str/index-of "^#" char)]
           [x y char])
         (reduce (fn [acc [x y char]]
                   (case char
                     \^
                     (-> acc
                         (assoc :guard-position [x y])
                         (assoc :guard-direction :up))

                     \#
                     (update acc :obstructions #(conj % [x y]))))
                 {:guard-position nil
                  :guard-direction nil
                  :map-width map-width
                  :map-height map-height
                  :obstructions #{}}))))

(defn guard-rotate
  [map-state]
  (update map-state
          :guard-direction
          #(case %
             :up    :right
             :down  :left
             :left  :up
             :right :down)))

(defn guard-step-forward
  [{:keys [guard-position guard-direction] :as map-state}]
  (let [new-guard-position (case guard-direction
                             :up
                             (update guard-position 1 dec)

                             :down
                             (update guard-position 1 inc)

                             :left
                             (update guard-position 0 dec)

                             :right
                             (update guard-position 0 inc))]
    (assoc map-state :guard-position new-guard-position)))

(defn guard-move
  [{:keys [obstructions] :as map-state}]
  (let [{new-guard-position :guard-position :as new-map-state} (guard-step-forward map-state)]
    (if (contains? obstructions new-guard-position)
      (guard-rotate map-state)
      new-map-state)))

(defn guard-inside-map?
  [{[guard-position-x guard-position-y] :guard-position
    map-width :map-width
    map-height :map-height}]
  (and (not (neg? guard-position-x))
       (not (neg? guard-position-y))
       (< guard-position-x map-width)
       (< guard-position-y map-height)))

;;;; Part 1

(defn part-1
  [input-file-path]
  (loop [map-state (parse-input-file input-file-path)
         visited-positions #{(:guard-position map-state)}]
    (let [{:keys [guard-position] :as new-map-state} (guard-move map-state)]
      (if-not (guard-inside-map? new-map-state)
        (count visited-positions)
        (recur new-map-state
               (conj visited-positions guard-position))))))

;;;; Part 2

(defn stuck-in-loop?
  [paths]
  (let [paths-count (count paths)]
    (loop [split-index (- paths-count 4)]
      (or (let [[split-1 split-2] (split-at split-index paths)]
            (= (take-last (count split-2) split-1)
               split-2))
          (let [new-split-index (dec split-index)]
            (and (pos? new-split-index)
                 (recur new-split-index)))))))

(defn stuck-in-loop-map-configuration?
  [initial-map-state]
  (loop [map-state initial-map-state
         prev-rotate-position nil
         paths []]
    (let [{old-guard-direction :guard-direction} map-state
          {new-guard-direction :guard-direction
           new-guard-position  :guard-position
           :as new-map-state} (guard-move map-state)]
      (cond
        (not (guard-inside-map? new-map-state))
        false

        (= old-guard-direction new-guard-direction)
        (recur new-map-state
               prev-rotate-position
               paths)

        prev-rotate-position
        (or (stuck-in-loop? paths)
            (recur new-map-state
                   new-guard-position
                   (conj paths [prev-rotate-position new-guard-position])))

        :else
        (recur new-map-state
               new-guard-position
               paths)))))

(defn place-obstruction
  [{:keys [guard-position map-width map-height obstructions] :as map-state}
   [x y :as obstruction]]
  (when (contains? obstructions obstruction)
    (throw
     (ex-info (str "Already has obstruction at " obstruction)
              {:map-state map-state})))

  (when (or (neg? x) (>= x map-width)
            (neg? y) (>= y map-height))
    (throw
     (ex-info (str "Obstruction must be inside the map " obstruction)
              {:map-state map-state})))

  (when (= obstruction guard-position)
    (throw
     (ex-info "Cannot place obstruction on guard's position"
              {:map-state map-state
               :obstruction obstruction})))

  (update map-state :obstructions (fn [obstructions]
                                    (conj obstructions obstruction))))

(defn can-place-obstruction?
  [{:keys [guard-position map-width map-height obstructions] :as _map-state}
   [x y :as obstruction]]
  (and (not (contains? obstructions obstruction))
       (not= obstruction guard-position)
       (not (neg? x))
       (not (neg? y))
       (< x map-width)
       (< y map-height)))

(defn part-2
  [input-file-path]
  (let [{:keys [map-width map-height] :as initial-map-state} (parse-input-file input-file-path)]
    (->> (for [x (range map-width)
               y (range map-height)
               :let [obstruction [x y]]
               :when (can-place-obstruction? initial-map-state obstruction)]
           obstruction)
         (pmap #(stuck-in-loop-map-configuration? (place-obstruction initial-map-state %)))
         (filter true?)
         (count))))

(comment
  (part-1 "input/2024/day-6.txt")
  ;;=> 5208

  (part-2 "input/2024/day-6.txt")
  ;;=> 1972
  :rcf)
