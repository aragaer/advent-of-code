#!/usr/bin/env bb

(defn read-boards []
  (read-line)
  (loop [boards []
         board []
         input (read-line)]
    (case input
      nil (map #(concat % (apply map list %)) (conj boards board))
      "" (recur (conj boards board) [] (read-line))
      (recur boards (conj board (->> (str/split input #" ")
                                     (mapv edn/read-string)
                                     (remove nil?)))
             (read-line)))))

(defn get-score [board]
  (->> board
       flatten
       (remove nil?)
       (reduce +)))

(let [numbers (map edn/read-string (str/split (read-line) #","))
      boards (read-boards)]
  (loop [boards boards
         numbers numbers
         incompletes (repeat (count boards) true)]
    (let [number (first numbers)
          boards (map #(map #(replace {number nil} %) %) boards)
          nil-boards (for [board boards]
                       (->> board
                            (map #(every? nil? %))
                            (every? false?)))
          new-winners (map not= incompletes nil-boards)
          winner-count (count (filter false? nil-boards))]
      (when number
        (when (and (not (every? false? new-winners))
                   (or
                    (= 1 winner-count)
                    (every? false? nil-boards)))
          (let [winner (->> (map vector boards new-winners)
                            (filter second)
                            first
                            first
                            get-score)]
            (println (* number (/ winner 2)))))
        (recur boards (rest numbers) nil-boards)))))
