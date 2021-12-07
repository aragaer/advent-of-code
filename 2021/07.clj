#!/usr/bin/env bb

(defn abs [x]
  (if (neg? x)
    (unchecked-negate x)
    x))

(defn total-cost [pos positions cost-func]
  (reduce + (map #(cost-func pos %) positions)))

(defn change-at-pos [pos positions cost-func]
  (let [cost-left (total-cost (dec pos) positions cost-func)
        cost-right (total-cost (inc pos) positions cost-func)
        cost-here (total-cost pos positions cost-func)
        dleft (if (< cost-left cost-here) 1 -1)
        dright (if (< cost-right cost-here) -1 1)]
    (+ dleft dright)))

(defn find-zero [range cost-func]
  (loop [l 0 h range]
    (if (<= h (inc l))
      l
      (let [m (+ l (bit-shift-right (- h l) 1))]
        (if (< (cost-func m) 0)
          (recur (inc m) h)
          (recur l m))))))

(let [positions (map edn/read-string (str/split (read-line) #","))]
  (doseq [cost-func [(fn [p x] (abs (- x p)))
                     (fn [p x] (let [d (abs (- x p))]
                                 (/ (* d (inc d)) 2)))]]
    (println (total-cost
              (find-zero (apply max positions)
                         #(change-at-pos % positions cost-func))
              positions cost-func))))
