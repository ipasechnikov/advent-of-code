(ns advent-of-code.2025.day-09
  (:require
   [clojure.set :refer [subset?]]
   [clojure.string :as str]))

(defn read-input
  [input-file-path]
  (->> (slurp input-file-path)
       (str/split-lines)
       (map #(str/split % #","))
       (map #(mapv Integer/parseInt %))))

(defn part-1
  [input-file-path]
  (let [tiles (read-input input-file-path)]
    (->> (for [[tile-index [x1 y1 :as _tile1]]
               (map-indexed vector tiles)

               [x2 y2 :as _tile2]
               (drop (inc tile-index) tiles)

               :let [w (-> (- x1 x2) abs inc)
                     h (-> (- y1 y2) abs inc)]]
           (* w h))
         (sort)
         (last))))

(defn boundary
  [tiles]
  (->> (for [[tile-index [x1 y1 :as _tile1]]
             (map-indexed vector tiles)

             :let [[x2 y2 :as _tile2]
                   (nth tiles
                        (inc tile-index)
                        (first tiles))

                   edge
                   (cond
                     (= x1 x2)
                     (for [y (range (min y1 y2)
                                    (inc (max y1 y2)))]
                       [x1 y])

                     (= y1 y2)
                     (for [x (range (min x1 x2)
                                    (inc (max x1 x2)))]
                       [x y1]))]]
         edge)
       (apply concat)))

(defn flood-fill
  [polygon]
  (let [sorted-xs
        (->> polygon
             (map first)
             (sort))

        min-x
        (first sorted-xs)

        max-x
        (last sorted-xs)

        sorted-ys
        (->> polygon
             (map second)
             (sort))

        min-y
        (first sorted-ys)

        max-y
        (last sorted-ys)

        boundary-tiles-lookup
        (->> polygon
             (boundary)
             (set))

        all-tiles
        (set
         (for [x (range (dec min-x) (+ max-x 2))
               y (range (dec min-y) (+ max-y 2))]
           [x y]))

        seed
        [(dec min-x) (dec min-y)]]
    (loop [[tile & remaining-tiles :as _tiles-queue]
           [seed]

           polygon-tiles
           all-tiles]
      (cond
        (nil? tile)
        polygon-tiles

        (contains? boundary-tiles-lookup tile)
        (recur remaining-tiles polygon-tiles)

        :else
        (let [[x y]
              tile

              new-tiles-queue
              (cond-> remaining-tiles
                (contains? polygon-tiles [x (inc y)])
                (conj [x (inc y)])

                (contains? polygon-tiles [x (dec y)])
                (conj [x (dec y)])

                (contains? polygon-tiles [(inc x) y])
                (conj [(inc x) y])

                (contains? polygon-tiles [(dec x) y])
                (conj [(dec x) y]))

              new-polygon-tiles
              (disj polygon-tiles tile)]
          (recur new-tiles-queue
                 new-polygon-tiles))))))

(defn part-2
  [input-file-path]
  (let [tiles
        (read-input input-file-path)

        compressed-xs
        (->> (map first tiles)
             (set)
             (sort)
             (map-indexed (fn [compressed-x x] [x compressed-x]))
             (into {}))

        compressed-ys
        (->> (map second tiles)
             (set)
             (sort)
             (map-indexed (fn [compressed-y y] [y compressed-y]))
             (into {}))

        compressed-tiles
        (mapv (fn [[x y]]
                [(get compressed-xs x)
                 (get compressed-ys y)])
              tiles)

        compressed-rectangles
        (->> (for [[tile-index [x1 y1 :as _tile1]]
                   (map-indexed vector tiles)

                   [x2 y2 :as _tile2]
                   (drop (inc tile-index) tiles)

                   :let [[compressed-x1 compressed-y1]
                         [(get compressed-xs x1)
                          (get compressed-ys y1)]

                         [compressed-x2 compressed-y2]
                         [(get compressed-xs x2)
                          (get compressed-ys y2)]

                         corners
                         [[(min compressed-x1 compressed-x2) (min compressed-y1 compressed-y2)]
                          [(max compressed-x1 compressed-x2) (min compressed-y1 compressed-y2)]
                          [(max compressed-x1 compressed-x2) (max compressed-y1 compressed-y2)]
                          [(min compressed-x1 compressed-x2) (max compressed-y1 compressed-y2)]]

                         area
                         (* (-> (- x1 x2) abs inc)
                            (-> (- y1 y2) abs inc))]]
               {:area area
                :corners corners})
             (sort-by :area >))

        polygon-tiles
        (flood-fill compressed-tiles)

        {:keys [area]}
        (->> compressed-rectangles
             (pmap (fn [{:keys [corners area]}]
                     {:area area
                      :tiles (flood-fill corners)}))
             (filter (fn [{:keys [tiles]}]
                       (subset? tiles polygon-tiles)))
             (first))]
    area))

(comment

  (part-1 "input/2025/day-09.example.txt")
  ;;=> 50

  (part-2 "input/2025/day-09.example.txt")
  ;;=> 24

  :rcf)

(comment

  (part-1 "input/2025/day-09.txt")
  ;;=> 4749929916

  (part-2 "input/2025/day-09.txt")
  ;;=> 1572047142

  :rcf)
