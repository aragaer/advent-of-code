#!/usr/bin/env bb

(declare read-packet)

(defn read-literal-packet [bits]
  (loop [result 0
         bits (drop 6 bits)]
    (if (empty? bits)
      result
      (let [group (take 5 bits)
            new-number (Integer/parseInt (apply str (rest group)) 2)
            new-result (+ (* result 16) new-number)]
        (if (= \0 (first group))
          [new-result (drop 5 bits)]
          (recur new-result (drop 5 bits)))))))

(defn read-operator-packet-mode0 [bits]
  (let [tot-length (Integer/parseInt (apply str (take 15 (drop 7 bits))) 2)
        new-length (- (count bits) 22 tot-length)]
    (loop [bits (drop 22 bits) result []]
      (if (<= (count bits) new-length)
        [result bits]
        (let [[this-result new-bits] (read-packet bits)]
          (recur new-bits (conj result this-result)))))))

(defn read-operator-packet-mode1 [bits]
  (let [tot-count (Integer/parseInt (apply str (take 11 (drop 7 bits))) 2)]
    (loop [bits (drop 18 bits) result []]
      (if (= tot-count (count result))
        [result bits]
        (let [[this-result new-bits] (read-packet bits)]
          (recur new-bits (conj result this-result)))))))

(def ops
  [#(reduce + %)
   #(reduce * %)
   #(apply min %)
   #(apply max %)
   nil
   #(if (> (first %) (second %)) 1 0)
   #(if (< (first %) (second %)) 1 0)
   #(if (= (first %) (second %)) 1 0)])

(defn read-packet [bits]
  (let [version (Integer/parseInt (apply str (take 3 bits)) 2)
        type (Integer/parseInt (apply str (take 3 (drop 3 bits))) 2)
        read-func (cond
                    (= 4 type) read-literal-packet
                    (= \0 (nth bits 6)) read-operator-packet-mode0
                    :else read-operator-packet-mode1)
        [packet bits] (read-func bits)]
    [{:version version :op (nth ops type) :data packet} bits]))

(defn sum-versions [{:keys [version data]}]
  (if (vector? data)
    (+ version (reduce + (map sum-versions data)))
    version))

(defn eval-packet [{:keys [op data]}]
  (if (nil? op)
    data
    (op (map eval-packet data))))

(let [hexes (map #(Integer/parseInt % 16) (str/split (read-line) #""))
      bits (seq (apply str (map #(format "%04d" (Integer/parseInt (Integer/toString % 2))) hexes)))
      [packet _] (read-packet bits)]
  (println (sum-versions packet))
  (println (eval-packet packet)))

