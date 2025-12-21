(ns advent-of-code.2024.day-05
  (:require
   [clojure.string :as str]))

(defn parse-input-file
  [input-file-path]
  (let [file-content (slurp input-file-path)
        [rules-str updates-str] (str/split file-content #"(\r?\n){2}")
        rules (->> (str/split-lines rules-str)
                   (map #(str/split % #"\|"))
                   (map (fn [[x-str y-str]] [(Integer/parseInt x-str)
                                             (Integer/parseInt y-str)]))
                   (reduce (fn [acc [x y]]
                             (let [x-rules (acc x)]
                               (if (nil? x-rules)
                                 (assoc acc x #{y})
                                 (assoc acc x (conj x-rules y)))))
                           {}))
        updates (->> (str/split-lines updates-str)
                     (map (fn [pages-str]
                            (map #(Integer/parseInt %)
                                 (str/split pages-str #",")))))]
    [rules updates]))

(defn can-come-after?
  "Check if X page can be printed after Y page according to rules"
  [x y rules]
  (let [x-rules (get rules x)]
    (or (nil? x-rules)
        (not (contains? x-rules y)))))

(defn page-correctly-ordered?
  [pages rules page-index]
  (let [page (nth pages page-index)
        pages-prior (take page-index pages)
        pages-after (drop (inc page-index) pages)]
    (and (every? true? (map #(can-come-after? page % rules) pages-prior))
         (every? true? (map #(can-come-after? % page rules) pages-after)))))

(defn update-correctly-ordered?
  [update rules]
  (loop [page-index 0]
    (or (= page-index (count update))
        (and (page-correctly-ordered? update rules page-index)
             (recur (inc page-index))))))

(defn middle-page-number
  [update]
  (let [middle-page-index (/ (count update) 2)]
    (nth update middle-page-index)))

(defn part-1
  [input-file-path]
  (let [[rules updates] (parse-input-file input-file-path)]
    (->> updates
         (filter #(update-correctly-ordered? % rules))
         (map middle-page-number)
         (apply +))))

(defn order-pages
  [pages rules]
  (let [page-comparator (fn [x y]
                          (if (not (can-come-after? x y rules)) -1 0))]
    (sort-by identity page-comparator pages)))

(defn part-2
  [input-file-path]
  (let [[rules updates] (parse-input-file input-file-path)]
    (->> updates
         (filter #(not (update-correctly-ordered? % rules)))
         (map #(order-pages % rules))
         (map middle-page-number)
         (apply +))))

(comment
  (part-1 "input/2024/day-05.txt")
  ;;=> 4569

  (part-2 "input/2024/day-05.txt")
  ;;=> 6456
  :rcf)
