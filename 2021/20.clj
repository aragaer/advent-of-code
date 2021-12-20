#!/usr/bin/env bb

(defn dump [[image b]]
  (doseq [line image]
    (println (apply str line)))
  (println "border" b)
  (println))

(defn add-border [image border-item]
  (let [w (count (first image))]
    (apply vector
           (vec (repeat (+ 2 w) border-item))
           (conj (mapv #(apply vector border-item (conj % border-item)) image)
                 (vec (repeat (+ 2 w) border-item))))))

(defn enhance [enh]
  (memoize
   (fn [cut]
     (->> cut
          flatten
          (replace {\# 1 \. 0})
          (reduce #(+ (* 2 %1) %2))
          (nth enh)))))

(defn cut [image [x y]]
  [(subvec (nth image (dec y)) (dec x) (+ 2 x))
   (subvec (nth image y) (dec x) (+ 2 x))
   (subvec (nth image (inc y)) (dec x) (+ 2 x))])

(defn step [[image b] enhance]
  (let [image (add-border image b)
        ex (add-border image b)
        h (count image)
        w (count (first image))]
    [(vec (for [y (range h)]
            (mapv #(enhance (cut ex [(inc %) (inc y)])) (range w))))
     (enhance (repeat 9 b))]))

(let [enhance (enhance (seq (read-line)))
      step #(step % enhance)
      _ (read-line)
      data (mapv vec (line-seq (java.io.BufferedReader. *in*)))
      images (iterate step [data \.])]
  (doseq [n [2 50]]
    (->> n
         (nth images)
         first
         flatten
         (filter #{\#})
         count
         println)))
