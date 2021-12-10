#!/usr/bin/env bb

(def legal
  (apply assoc {}
         (seq "[](){}<>")))

(def corrupt-costs
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def incomplete-costs
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn process-line [line & open]
  (cond
    (empty? line) open
    (= (first open) (first line)) (recur (rest line) (rest open))
    (contains? legal (first line)) (recur (rest line) (apply list (get legal (first line)) open))
    :else (first line)))

(let [lines (map seq (line-seq (java.io.BufferedReader. *in*)))
      processed (map process-line lines)
      corrupted (remove seq? processed)
      incomplete (filter seq? processed)]
  (->> corrupted
       (map corrupt-costs)
       (reduce +)
       (println))
  (->> incomplete
       (map (fn [i]
              (reduce #(+ (* 5 %1) %2) (map incomplete-costs i))))
       sort
       (#(nth % (-> incomplete
                    count
                    (/ 2)
                    int)))
       (println)))
