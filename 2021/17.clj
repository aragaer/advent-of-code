#!/usr/bin/env bb

(defn max-height [vy]
  (int (* vy (inc vy) 1/2)))

(defn y-hits [vy [_ _ my My]]
  (if (< vy 0)
    (->> [0 0 vy]
         (iterate (fn [[i y vy]] [(inc i) (+ y vy) (dec vy)]))
         (take-while (fn [[i y _]] (>= y my)))
         (drop-while (fn [[i y _]] (> y My)))
         (map first))
    (->> [0 0]
         (iterate (fn [[i y]] [(inc i) (- y vy i)]))
         (take-while (fn [[i y]] (>= y my)))
         (drop-while (fn [[i y]] (> y My)))
         (map (fn [[i _]] (+ i (* 2 vy)))))))

(defn x-at [vx t]
  (if (> vx t)
    (- (max-height vx) (max-height (- vx t)))
    (max-height vx)))

(let [tgt (flatten (map #(map edn/read-string
                              (str/split (subs % 2) #"\.\."))
                        (str/split (subs (read-line) 13) #", ")))
      [mx Mx my My] tgt
      maxvy (max (if (< 0 My) My 0)
                 (if (< 0 my) 0 (dec (- my))))
      good (remove nil?
                   (for [vx (range (inc Mx))
                         vy (range my (inc maxvy))]
                     (let [ys (y-hits vy tgt)
                           xs (filter #(<= mx % Mx) (map #(x-at vx %) ys))]
                       (if (empty? xs)
                         nil
                         [vx vy]))))]
  (->> good
       (map second)
       (map max-height)
       (apply max)
       (println))
  (println (count good)))
