#!/usr/bin/env bb

(defn go-up-until [path stop-at]
  (cond
    (empty? path) path
    (= stop-at (last path)) path
    :else (recur (pop path) stop-at)))

(defn get-next-node [n path direction]
  (if (vector? (get-in n path))
    (conj path direction)
    (let [up (go-up-until path direction)]
      (if (empty? up)
        nil
        (conj (pop up) (- 1 direction))))))

(defn get-next-leaf [n path direction]
  (let [next (get-next-node n path direction)]
    (cond
      (nil? next) nil
      (vector? (get-in n next)) (recur n next direction)
      :else next)))

(defn snail-explode [n where]
  (let [this (get-in n where)
        left (get-next-leaf n (conj where 0) 1)
        right (get-next-leaf n (conj where 1) 0)
        add-to-left (if (nil? left) identity #(update-in % left + (first this)))
        add-to-right (if (nil? right) identity #(update-in % right + (second this)))]
    (-> n
        (update-in where (constantly 0))
        add-to-left
        add-to-right)))

(defn snail-split [n where]
  (let [value (get-in n where)
        left (int (Math/floor (/ value 2)))
        right (int (Math/ceil (/ value 2)))]
    (update-in n where (constantly [left right]))))

(defn snail-reduce [n]
  (defn walk [p] (get-next-leaf n p 0))
  (let [digit-paths (take-while some? (rest (iterate walk [])))
        d4 (take 1 (filter #(= 5 (count %)) digit-paths))
        too-large (take 1 (filter #(<= 10 (get-in n %)) digit-paths))]
    (cond
      (not (empty? d4)) (recur (snail-explode n (pop (first d4))))
      (not (empty? too-large)) (recur (snail-split n (first too-large)))
      :else n)))

(defn magnitude [n]
  (if (vector? n)
    (+ (* 3 (magnitude (first n)))
       (* 2 (magnitude (second n))))
    n))

(def snail-add (comp snail-reduce vector))

(let [numbers (map edn/read-string (line-seq (java.io.BufferedReader. *in*)))
      result (reduce snail-add numbers)]
  (println (magnitude result) result)
  (println
   (reduce max
           (for [f (range (count numbers))
                 s (range (count numbers))]
             (if (= f s)
               0
               (magnitude (snail-add (nth numbers f) (nth numbers s))))))))
