#!/usr/bin/env bb

(let [count-by-age (frequencies (map edn/read-string (str/split (read-line) #",")))
      counts (map #(get count-by-age % 0) (range 9))
      its (iterate (fn [[b0 b1 b2 b3 b4 b5 b6 b7 b8]]
                     (list b1 b2 b3 b4 b5 b6 (+ b7 b0) b8 b0)) counts)]
  (doseq [days [80 256]]
    (println (reduce + (nth its days)))))
