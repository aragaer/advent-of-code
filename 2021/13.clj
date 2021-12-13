#!/usr/bin/env bb

(defn dump [page]
  (let [x-size (inc (apply max (map first page)))
        y-size (inc (apply max (map second page)))]
    (doseq [y (range y-size)]
      (println (apply str (map #(if (contains? page (list % y)) "#" ".") (range x-size)))))))

(def fold-page #(set (map %2 %1)))

(defn mirror [c v]
  (if (> c v) (- (* v 2) c) c))

(let [lines (line-seq (java.io.BufferedReader. *in*))
      [dots _ ins] (partition-by empty? lines)
      page (set (map #(map edn/read-string %) (map #(str/split % #",") dots)))
      ins (map (fn [ins]
                 (let [dir (nth (seq ins) 11)
                       val (edn/read-string (subs ins 13))]
                   (if (= dir \x)
                     #(fold-page % (fn [[x y]] [(mirror x val) y]))
                     #(fold-page % (fn [[x y]] [x (mirror y val)]))))) ins)]
  (println (count ((first ins) page)))
  (dump (reduce #(%2 %1) page ins)))
