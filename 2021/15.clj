#!/usr/bin/env bb

(defn neighs [[x y] x-size y-size]
  (set
   (concat
    (if (= x 0)
      [] [[(dec x) y]])
    (if (= y 0)
      [] [[x (dec y)]])
    (if (= (inc x) x-size)
      [] [[(inc x) y]])
    (if (= (inc y) y-size)
      [] [[x (inc y)]]))))

(defn fill [cave x-size y-size]
  (let [nns (zipmap (keys cave)
                    (map #(neighs % x-size y-size) (keys cave)))]
    (loop [filled {[0 0] 0}
           layer [[0 0]]]
      (if (empty? layer)
        filled
        (let [layer-neighs (select-keys nns layer)
              new-layer (apply set/union (vals layer-neighs))
              values (apply merge-with min
                            (map (fn [[here positions]]
                                   (zipmap positions
                                           (map #(unchecked-add (get cave %) (get filled here)) positions)))
                                 layer-neighs))
              {seen true
               unseen false} (group-by #(contains? filled %) new-layer)
              improved (filter #(< (get values %) (get filled %)) seen)
              layer-update (concat unseen improved)]
          (recur (merge filled (select-keys values layer-update))
                 layer-update))))))

(let [lines (map seq (line-seq (java.io.BufferedReader. *in*)))
      data (mapv #(apply vector (map #(- (int %) 48) %)) lines)
      x-size (count (first data))
      y-size (count data)
      cave (into {} (for [x (range x-size)
                          y (range y-size)]
                      [[y x] (get-in data [y x])]))
      increased-cave (apply merge
                           (for [x (range 5)
                                 y (range 5)]
                             (into {}
                                   (map (fn [[[kx ky] v]]
                                          [[(+ kx (* x x-size))
                                            (+ ky (* y y-size))]
                                           (+ (mod (+ v x y -1) 9) 1)])
                                        cave))))]
  (println
   (get
    (fill cave x-size y-size)
    [(dec x-size) (dec y-size)]))
  (println
   (get
    (fill increased-cave (* x-size 5) (* y-size 5))
    [(dec (* x-size 5)) (dec (* y-size 5))])))
