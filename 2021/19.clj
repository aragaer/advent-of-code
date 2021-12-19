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

(defn rotate-coord [[r1 r2 r3]]
  (let [[a1 a2 a3] (map get-coord [r1 r2 r3])]
    (memoize (fn [c] [(a1 c) (a2 c) (a3 c)]))))

(def all-rotations-f
  (let [all-rotations (set
                       (for [rx (range 4)
                             ry (range 4)
                             rz (range 4)]
                         (rotate-around-axis
                          (rotate-around-axis
                           (rotate-around-axis [1 2 3] 0 rx)
                           1 ry)
                          2 rz)))]
  (map rotate-coord all-rotations)))

(defn rotate [dots]
  (for [rotation all-rotations-f]
    (map rotation dots)))

(defn get-rotation [from to]
  (for [rotation all-rotations-f]
    (let [t1 (set to)
          t2 (set (map rotation from))]
      (count (set/intersection t1 t2)))))

(defn translate [p o] (mapv unchecked-add p o))
(defn pos-diff [p1 p2] (mapv unchecked-subtract p1 p2))
(defn get-some? [x] (if (nil? x) false x))
(defn get-non-empty? [x] (if (empty? x) false x))

(defn in-range? [[x y z]]
  (and (<= -1000 x 1000)
       (<= -1000 y 1000)
       (<= -1000 z 1000)))

(defn overlaps [scanner1 scanner2]
  ;(println "Check for overlap between" (:num scanner1) "and" (:num scanner2))
  (let [s1l (map unchecked-negate (:location scanner1))]
    (some get-non-empty?
          (pmap (fn [orientation]
                  (some get-some?
                        (pmap (fn [beacon1]
                                (loop [to-check (map #(translate % s1l) orientation)]
                                  (if (> 12 (count to-check))
                                    nil
                                    (let [beacon2 (first to-check)
                                          o (pos-diff beacon1 beacon2)
                                          translation (map #(translate % o) to-check)
                                          overlap (set (filter in-range? translation))]
                                      (if (and (<= 12 (count overlap))
                                               (set/subset? overlap (:sees scanner1)))
                                        (assoc scanner2
                                               :sees (set orientation)
                                               :location o)
                                        (recur (rest to-check)))))))
                              (:sees scanner1)))) (:rots scanner2)))))

(defn solve [data]
  (loop [fixed [(first data)]
         unfixed (rest data)
         new-fixed [(first data)]]
    (let [overlap-now (remove nil? (for [u unfixed]
                                     (some get-some? (map #(overlaps % u) new-fixed))))]
      ;(println (count new-fixed) (count unfixed) (count fixed) (count overlap-now))
      ;(println (map :num overlap-now))
      (cond
        (empty? unfixed) fixed
        (empty? overlap-now) (println "Can't find anything to overlap with" (map :num new-fixed))
        :else (recur (concat fixed overlap-now)
                     (map #(nth data %) (set/difference (set (map :num unfixed)) (set (map :num overlap-now))))
                     overlap-now)))))

(defn move [scanner pos]
  (assoc scanner :sees (map #(translate % pos) (:sees scanner))))
(defn d1 [v1 v2] (if (< v1 v2) (- v2 v1) (- v1 v2)))
(defn dist [p1 p2] (reduce + (map d1 p1 p2)))

(defn read-scanner [lines]
  (let [[_ _ num _] (str/split (first lines) #" ")
        sees (set (map #(vec (map edn/read-string (str/split % #","))) (rest lines)))]
    {:num (edn/read-string num)
     :sees sees
     :location nil
     :rots (rotate sees)}))

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
        (#(for [f % s %] (dist f s)))
        (reduce max)
        println)))
