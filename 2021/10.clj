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
  (if (empty? line)
    open
    (let [c (first line)]
      (cond
        (= (first open) c) (recur (rest line) (rest open))
        (contains? legal c) (recur (rest line) (conj open (get legal c)))
        :else c))))

(let [lines (map seq (line-seq (java.io.BufferedReader. *in*)))
      processed (map process-line lines)
      {corrupted false
       incomplete true} (group-by seq? processed)]
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
