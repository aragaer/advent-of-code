#!/usr/bin/env bb

(defn decode [input output]
  (let [input (map set input)
        len5 (filter #(= (count %) 5) input)
        len6 (filter #(= (count %) 6) input)
        c1 (first (filter #(= (count %) 2) input))
        c7 (first (filter #(= (count %) 3) input))
        c4 (first (filter #(= (count %) 4) input))
        c8 (first (filter #(= (count %) 7) input))
        c9 (first (filter #(empty? (set/difference c4 %)) len6))
        a (first (set/difference c7 c1))
        e (first (set/difference c8 c9))
        c2 (first (filter #(contains? % e) (filter #(= (count %) 5) input)))
        f (first (set/difference c1 c2))
        c (first (remove #(= % f) c1))
        c6 (first (remove #(contains? % c) len6))
        c5 (set (remove #(= % e) c6))
        c3 (first (remove #(or (= % c5) (= % c2)) len5))
        f (first (set/difference c1 c2))
        b (first (set/difference c9 c3))
        g (first (set/difference c9 (set/union c4 c7)))
        c0 (set (set [a b c e f g]))
        d (first (set/difference c8 c0))
        decoded {c0 0 c1 1 c2 2 c3 3 c4 4 c5 5 c6 6 c7 7 c8 8 c9 9}
        output (map #(get decoded (set %)) output)]
    (reduce + (map * output [1000 100 10 1]))))

(let [lines (line-seq (java.io.BufferedReader. *in*))
      inputs (map #(str/split (first (str/split % #" \| ")) #" ") lines)
      outputs (map #(str/split (second (str/split % #" \| ")) #" ") lines)]
  (->> outputs
       (flatten)
       (map count)
       (filter (fn [x] (some #(= x %) [2 3 4 7])))
       count
       println)
  (->> (map decode inputs outputs)
       (reduce +)
       println))
