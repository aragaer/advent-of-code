#!/usr/bin/env bb

(def all-positions [:l2 :l :ab :bc :cd :r :r2])
(def live-at {"A" :a "B" :b "C" :c "D" :d})
(def lives-here (zipmap (vals live-at) (keys live-at)))
(def move-cost {"A" 1 "B" 10 "C" 100 "D" 1000})
(def blocked-by
  {#{:a :b} [:ab]
   #{:a :c} [:ab :bc]
   #{:a :d} [:ab :bc :cd]
   #{:b :c} [:bc]
   #{:b :d} [:bc :cd]
   #{:c :d} [:cd]
   #{:a :bc} [:ab]
   #{:a :cd} [:ab :bc]
   #{:b :cd} [:bc]
   #{:c :ab} [:bc]
   #{:d :ab} [:bc :cd]
   #{:d :bc} [:cd]})

(defn room-is-good [cave room]
  (let [owner (get lives-here room)]
    (every? #{owner} (get cave room))))

(def path
  (memoize
   (fn [move]
     (cond-> (blocked-by (set (replace {:l :a :l2 :a :r :d :r2 :d} move)) [])
       (contains? move :l2) (conj :l)
       (contains? move :r2) (conj :r)))))

(def move-dist
  (memoize
   (fn [move]
     (cond-> (+ 2  (* 2 (count (path move))))
       (contains? move :l2) dec
       (contains? move :r2) dec))))

(defn way-is-clear [cave move]
  (every? empty? (map cave (path move))))

(defn do-move [cave move-from move-to]
  (let [who (first (get cave move-from))
        room-depth (cave :room-depth)
        dist (+ (move-dist #{move-from move-to})
                (if (contains? lives-here move-from)
                  (- room-depth (count (cave move-from)))
                  (- (dec room-depth) (count (cave move-to)))))]
    [(* (get move-cost who) dist)
     (assoc cave
            move-from (rest (cave move-from))
            move-to (conj (cave move-to) who))]))

(defn generate-moves [cost cave]
  (let [{outside-free true
         outside-waiting false} (group-by #(empty? (cave %)) all-positions)
        {inside-good true
         inside-ready false} (group-by #(room-is-good cave %) (keys lives-here))
        out-moves (for [from inside-ready
                        to outside-free]
                    [from to])
        in-moves (filter #(some #{(second %)} inside-good)
                         (map #(vector % (live-at (first (cave %)))) outside-waiting))
        possible-moves (filter #(way-is-clear cave (set %)) (concat in-moves out-moves))
        moves (map (fn [[from to]]
                     (let [[move-cost new-cave] (do-move cave from to)]
                       [(+ cost move-cost) new-cave]))
                   possible-moves)]
    (into (sorted-map)
          (for [cost (set (map first moves))]
            {cost (set (map second (filter #(= cost (first %)) moves)))}))))

(defn finished? [cave]
  (every? #(and (room-is-good cave %) (= (get cave :room-depth) (count (get cave %)))) (keys lives-here)))

(defn solve [states]
  (let [[cost [cave & caves]] (first states)
        new-states (if (empty? caves)
                     (dissoc states cost)
                     (update states cost rest))]
    (cond
      (finished? cave) cost
      (empty? states) (println "no solutions")
      :else (recur (merge-with set/union new-states (generate-moves cost cave))))))

(def extra [["D" "D"] ["C" "B"] ["B" "A"] ["A" "C"]])

(let [_ (read-line)
      _ (read-line)
      f (remove #{""} (str/split (read-line) #"#| "))
      s (remove #{""} (str/split (read-line) #"#| "))
      rooms (map (partial list) f s)
      cave (merge (sorted-map :room-depth 2)
                  (zipmap all-positions (repeat '()))
                  (zipmap [:a :b :c :d] rooms))
      updated-cave (into (assoc cave :room-depth 4)
                         (zipmap [:a :b :c :d]
                                 (map (fn [[a b] [c d]] [a c d b]) rooms extra)))]
  (doseq [cave [cave updated-cave]]
    (println (solve (sorted-map 0 [cave])))))
