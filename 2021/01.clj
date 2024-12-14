#!/usr/bin/env bb

(let [depths (map edn/read-string
                  (line-seq (java.io.BufferedReader. *in*)))]
  (doseq [l [depths (map + depths (drop 1 depths) (drop 2 depths))]]
    (->> l
         (#(map < % (rest %)))
         (filter true?)
         count
         println)))
