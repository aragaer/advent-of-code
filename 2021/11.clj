#!/usr/bin/env bb

(defn neighs [[y x]]
  (for [dy [-1 0 1]
        dx [-1 0 1]]
    [(+ y dy) (+ x dx)]))

(defn do-flashes [octopi flashed]
  (let [flash-now (set/difference (set (filter #(< 9 (get octopi %)) (keys octopi))) flashed)
        light-now (apply concat (for [octopus flash-now]
                                  (->> octopus
                                       (neighs)
                                       (filter #(contains? octopi %)))))]
    (if (empty? flash-now)
      [(count flashed) (merge octopi (zipmap flashed (repeat 0)))]
      (recur
       (reduce #(update-in %1 [%2] inc) octopi light-now)
       (set/union flashed flash-now)))))

(defn step [octopi]
  (do-flashes
   (reduce #(update-in %1 [%2] inc) octopi (keys octopi))
   (set [])))

(let [lines (map seq (line-seq (java.io.BufferedReader. *in*)))
      data (mapv #(apply vector (map #(- (int %) 48) %)) lines)
      x-size (count (first data))
      y-size (count data)
      octopi (into {} (for [x (range x-size)
                            y (range y-size)]
                        [[y x] (get-in data [y x])]))
      steps (iterate (comp step second) [0 octopi])
      flashes (map first steps)]
  (println (reduce + (take 101 flashes)))
  (println (count (take-while #(> (* x-size y-size) %) flashes))))
