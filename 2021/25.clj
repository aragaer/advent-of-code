#!/usr/bin/env bb

(defn change-sets [keys-set vals-set change-map]
  (let [inv (zipmap (vals change-map) (keys change-map))]
    (map set [(replace change-map keys-set) (replace inv vals-set)])))

(defn step [{:keys [h w e s f] :as state}]
  (let [e-dest (into {} (map (fn [[y x]] [[y (mod (inc x) w)] [y x]]) e))
        can-move-east (set/intersection f (set (keys e-dest)))
        [f e] (change-sets f e (select-keys e-dest can-move-east))
        s-dest (into {} (map (fn [[y x]] [[(mod (inc y) h) x] [y x]]) s))
        can-move-south (set/intersection f (set (keys s-dest)))
        [f s] (change-sets f s (select-keys s-dest can-move-south))]
    [(+ (count can-move-east) (count can-move-south))
     (assoc state :e e :f f :s s)]))

(let [data (mapv vec (line-seq (java.io.BufferedReader. *in*)))
      width (count (first data))
      height (count data)
      {south \v
       east \>
       free \.} (group-by #(get-in data %) (for [x (range width)
                                                 y (range height)]
                                             [y x]))
      state {:h height :w width :e (set east) :s (set south) :f (set free)}]
  (->> [100 state]
       (iterate #(step (second %)))
       (take-while #(< 0 (first %)))
       count
       println))
