#!/usr/bin/env bb

(defn rotate-around-axis [v axis rot]
  (let [[x y z] v
        fixed (nth v axis)
        [o1 o2] (take 2 (drop (inc axis) [x y z x y z]))
        [r1 r2] (take 2 (drop rot [o1 o2 (- o1) (- o2) o1]))]
    (vec (take 3 (drop (- 3 axis) [fixed r1 r2 fixed r1 r2])))))

(defn get-coord [r]
  (if (pos? r)
    (let [i (dec r)]
      #(nth % i))
    (let [i (dec (- r))]
      #(unchecked-negate (nth % i)))))

(defn rotate-coord [r]
  (let [[a1 a2 a3] (map get-coord r)]
    (memoize (fn [c] [(a1 c) (a2 c) (a3 c)]))))

(def all-rotations-f
  (map rotate-coord (set
                     (for [rx (range 4)
                           ry (range 4)
                           rz (range 4)]
                       (rotate-around-axis
                        (rotate-around-axis
                         (rotate-around-axis [1 2 3] 0 rx)
                         1 ry)
                        2 rz)))))

(defn rotate [dots]
  (for [rotation all-rotations-f]
    (map rotation dots)))

(defn translate [p o] (mapv unchecked-add p o))
(defn pos-diff [p1 p2] (mapv unchecked-subtract p1 p2))
(defn get-some? [x] (if (false? x) false x))
(defn get-non-empty? [x] (if (empty? x) false x))
(defn move [scanner pos]
  (assoc scanner :sees (map #(translate % pos) (:sees scanner))))
(defn dist [p1 p2] (reduce #(if (pos? %2) (+ %1 %2) (- %1 %2)) 0 (pos-diff p1 p2)))
(defn distances [dots] (frequencies (for [d1 dots d2 dots] (dist d1 d2))))

(defn in-range? [[x y z]]
  (and (<= -1000 x 1000)
       (<= -1000 y 1000)
       (<= -1000 z 1000)))

(defn select-sum [m k]
  (reduce + (vals (select-keys m k))))

(defn footprint-matches? [d1 d2 threshold]
  (let [common (remove #{0} (set/intersection (set (keys d1)) (set (keys d2))))
        [n1 n2] (map #(select-sum % common) [d1 d2])]
    (and (<= threshold n1)
         (<= threshold n2))))

(defn match-beacon-footprint [beacon-footprint scanner]
  (filter #(footprint-matches? beacon-footprint (get (:footprints-per-beacon scanner) %) 11) (:sees scanner)))

(defn overlaps [scanner1 scanner2]
  (if (footprint-matches? (:footprint scanner1) (:footprint scanner2) 132)
    (let [s1l (map unchecked-negate (:location scanner1))]
      (some (fn [[beacon1 footprint]]
              (some (fn [[rotation to-check]]
                      (let [o (pos-diff beacon1 (rotation to-check))
                            orientation (map rotation (:sees scanner2))
                            renamed-keys (zipmap (:sees scanner2) orientation)
                            overlap (set (filter in-range? (map #(translate % o) orientation)))]
                        (if (and (<= 12 (count overlap))
                                 (set/subset? overlap (set (:sees scanner1))))
                          (assoc (update scanner2
                                         :footprints-per-beacon
                                         #(set/rename-keys % renamed-keys))
                                 :sees orientation
                                 :location (translate o (map - s1l)))
                          false))) (for [m (match-beacon-footprint footprint scanner2)
                                         r all-rotations-f] [r m])))
            (:footprints-per-beacon scanner1)))))

(defn solve [data]
  (loop [fixed [(first data)]
         unfixed (rest data)
         new-fixed [(first data)]]
    (let [overlap-now (remove nil? (for [u unfixed]
                                     (some get-some? (map #(overlaps % u) new-fixed))))]
      (cond
        (empty? unfixed) fixed
        (empty? overlap-now) (println "Can't find anything to overlap with" (map :num new-fixed))
        :else (recur (concat fixed overlap-now)
                     (map #(nth data %) (set/difference (set (map :num unfixed)) (set (map :num overlap-now))))
                     overlap-now)))))

(defn read-scanner [lines]
  (let [[_ _ num _] (str/split (first lines) #" ")
        sees (map #(vec (map edn/read-string (str/split % #","))) (rest lines))]
    {:num (edn/read-string num)
     :sees sees
     :footprints-per-beacon (into {} (for [b sees] [b (frequencies (map #(dist % b) sees))]))
     :footprint (distances sees)}))

(let [lines (line-seq (java.io.BufferedReader. *in*))
      data (mapv read-scanner (keep-indexed #(if (even? %1) %2) (partition-by empty? lines)))
      data (update-in data [0 :location] (constantly [0 0 0]))]
  (let [fixed (solve data)]
    (->> fixed
         (map #(move % (:location %)))
         (map :sees)
         (map set)
         (apply set/union)
         count
         println)
    (->> fixed
        (map :location)
        distances
        keys
        (reduce max)
        println)))
