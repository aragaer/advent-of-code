#!/usr/bin/env bb

(let [depths (map edn/read-string
                  (line-seq (java.io.BufferedReader. *in*)))
      sliding (map + depths (drop 1 depths) (drop 2 depths))]
  (->>
   (map < depths (rest depths))
   (filter identity)
   count
   println)
  (->>
   (map < sliding (rest sliding))
   (filter identity)
   count
   println))
