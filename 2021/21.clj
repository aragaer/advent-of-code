#!/usr/bin/env bb

(defn roll [dice]
  [dice (inc (mod dice 100))])

(defn roll-3 [dice]
  (nth (iterate (fn [[tot dice]]
                  (let [[res dice] (roll dice)]
                    [(+ tot res) dice]))
                [0 dice]) 3))

(defn move [pos dice]
  (inc (mod (+ pos dice -1) 10)))

(defn take-turn [pos dice]
  (let [[res dice] (roll-3 dice)]
    [(move pos res) dice]))

(defn deterministic-game [positions]
  (loop [roll-count 0
         points [0 0]
         positions positions
         dice 1]
    (let [[pos dice] (take-turn (first positions) dice)
          roll-count (+ 3 roll-count)
          pts (+ pos (first points))]
      (if (<= 1000 pts)
        (* roll-count (second points))
        (recur roll-count
               [(second points) pts]
               [(second positions) pos]
               dice)))))

(def quantum-roll
  (for [r1 (range 1 4) r2 (range 1 4) r3 (range 1 4)]
    (+ r1 r2 r3)))

(def quantum-game
  (memoize (fn [positions points player]
             (let [pos (nth positions player)
                   pts (nth points player)
                   q-pos (map #(move pos %) quantum-roll)
                   {finished true
                    unfinished false} (group-by #(<= 21 (+ pts %)) q-pos)]
               (apply merge-with +
                      {player (count finished)}
                      (map #(quantum-game (update-in positions [player] (constantly %))
                                          (update-in points [player] (constantly (+ pts %)))
                                          (- 1 player)) unfinished))))))

(let [positions (mapv #(edn/read-string (second (str/split % #":"))) (line-seq (java.io.BufferedReader. *in*)))]
  (println (deterministic-game positions))
  (println (apply max (vals (quantum-game positions [0 0] 0)))))
