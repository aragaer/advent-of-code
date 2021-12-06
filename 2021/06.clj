#!/usr/bin/env bb

(let [count-by-age (frequencies (map edn/read-string (str/split (read-line) #",")))]
  (doseq [days [80 256]]
    (println
     (loop [this-count count-by-age
            days days]
       (if (= 0 days)
         (reduce + (vals this-count))
         (recur
          (merge-with +
                      (reduce (fn [counts age]
                                (assoc counts age (get this-count (inc age) 0)))
                              {} (range 9))
                      {6 (get this-count 0 0)
                       8 (get this-count 0 0)})
          (dec days)))))))
