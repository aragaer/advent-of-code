#!/usr/bin/env bb

(defn neighs [[y x]]
  [[(dec y) x]
   [y (dec x)]
   [y (inc x)]
   [(inc y) x]])

(defn get-next-basin [cave-map]
  (loop [q [(first (keys cave-map))]
         cave-map cave-map
         basin-size 0]
    (if (empty? q)
      [basin-size cave-map]
      (if (= 9 (get cave-map (first q) 9))
        (recur (rest q) cave-map basin-size)
        (recur (remove #(= 9 (get cave-map % 9))
                       (concat (rest q) (neighs (first q))))
               (dissoc cave-map (first q))
               (inc basin-size))))))

(let [lines (line-seq (java.io.BufferedReader. *in*))
      cave-data (mapv #(apply vector (map #(- (int %) 48) %)) lines)
      x-size (count (first cave-data))
      y-size (count cave-data)
      cave-map (into {} (remove #(= 9 (second %))
                                (for [x (range x-size)
                                      y (range y-size)]
                                  [[y x] (get-in cave-data [y x] 9)])))]
  (->>
   (for [[p here] cave-map]
     (if (< here (apply min (map #(get cave-map % 9) (neighs p))))
       (inc here)
       0))
   (reduce +)
   println)
  (->>
   (loop [cave-map cave-map
          basin-sizes []]
     (if (empty? cave-map)
       basin-sizes
       (let [[this-basin new-map] (get-next-basin cave-map)]
         (recur new-map
                (conj basin-sizes this-basin)))))
   (sort >)
   (take 3)
   (reduce *)
   println))
