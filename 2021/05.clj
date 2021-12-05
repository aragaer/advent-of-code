#!/usr/bin/env bb

(defn my-range [s e]
  (cond
    (< s e) (range s (inc e))
    (> s e) (reverse (range e (inc s)))
    :else (repeat s)))

(defn get-plot [[x1 y1 x2 y2]]
  (reduce #(assoc %1 %2 1) {}
          (map vector (my-range x1 x2) (my-range y1 y2))))

(let [lines (map #(map edn/read-string (re-seq #"\d+" %))
                 (line-seq (java.io.BufferedReader. *in*)))
      hor-vert-lines (filter (fn [[x1 y1 x2 y2]]
                               (or (= x1 x2) (= y1 y2))) lines)]
  (doseq [lines [hor-vert-lines lines]]
    (->> lines
         (map get-plot)
         (apply merge-with (constantly nil))
         vals
         (filter nil?)
         count
         println)))
