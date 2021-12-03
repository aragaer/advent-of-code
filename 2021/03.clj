#!/usr/bin/env bb

(require '[clojure.string :as string])

(defn get-freqs [readings]
  (let [reading-len (count (first readings))]
    (map (fn [i] (frequencies (map #(subs % i (inc i)) readings))) (range reading-len))))

(defn to-int [reading]
  (Integer/parseInt reading 2))

(defn analyze [readings func]
  (let [freqs (get-freqs readings)]
    (to-int (string/join (map #(func (% "0") (% "1")) freqs)))))

(defn analyze2 [readings func]
  (loop [readings readings
         idx 0]
        (if (= 1 (count readings))
          (to-int (first readings))
          (let [freqs (frequencies (map #(subs % idx (inc idx)) readings))
                z (freqs "0" 0)
                o (freqs "1" 0)
                keep-if (if (func z o) "0" "1")]
            (recur (filter (fn [reading]
                             (= (subs reading idx (inc idx)) keep-if)) readings)
                   (inc idx))))))

(let [readings (line-seq (java.io.BufferedReader. *in*))
      gamma (analyze readings (fn [z o] (if (> z o) "0" "1")))
      epsilon (analyze readings (fn [z o] (if (< z o) "0" "1")))
      oxygen (analyze2 readings >)
      co2 (analyze2 readings <=)]
  (println (* gamma epsilon))
  (println (* oxygen co2)))
