#!/usr/bin/env bb

(defn step [pairs rules]
  (apply merge-with +
         (map (fn [[pair count]]
                (let [result (get rules pair)]
                  (if (nil? result)
                    {pair count}
                    {(first result) count, (second result) count})))
              pairs)))

(defn count-pair [[[f s] count]]
  (if (= f s)
    {f count}
    {f (/ count 2) s (/ count 2)}))

(let [poly (seq (read-line))
      _ (read-line)
      rules (into {} (map (fn [line]
                            (let [[[a b] _ [c]] (str/split line #" ")]
                                  [[a b] [[a c] [c b]]]))
                          (line-seq (java.io.BufferedReader. *in*))))
      pairs (frequencies (map list poly (rest poly)))
      polys (iterate #(step % rules) pairs)]
  (doseq [x [10 40]]
    (->> (nth polys x)
         (map count-pair)
         (apply merge-with + (count-pair [[(first poly) (last poly)] 1]))
         vals
         sort
         (#(- (last %) (first %)))
         println)))
