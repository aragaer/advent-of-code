#!/usr/bin/env bb

(require '[clojure.string :as string])

(let [course (map (fn [x]
                    (let [parts (string/split x #" ")]
                      [(first parts) (edn/read-string (second parts))]))
                  (line-seq (java.io.BufferedReader. *in*)))]
  (let [[pos depth] (reduce (fn [[pos depth] [dir val]]
                              (case dir
                                "forward" [(+ pos val) depth]
                                "up" [pos (- depth val)]
                                "down" [pos (+ depth val)]))
                            [0 0] course)]
    (println (* pos depth)))
  (let [[pos depth aim] (reduce (fn [[pos depth aim] [dir val]]
                                  (case dir
                                    "forward" [(+ pos val) (+ depth (* aim val)) aim]
                                    "up" [pos depth (- aim val)]
                                    "down" [pos depth (+ aim val)]))
                                [0 0 0] course)]
    (println (* pos depth))))
