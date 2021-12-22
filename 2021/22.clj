#!/usr/bin/env bb

(defn read-step [step]
  (let [[op where] (str/split step #" ")
        [bx by bz] (map #(map edn/read-string (str/split (subs % 2) #"\.\.")) (str/split where #","))]
    [(= "on" op) bx by bz]))

(defn vol [block] (reduce * (map #(- (second %) (first %) -1) block)))
(defn total-vol [m] (reduce + (map vol m)))
(defn subdim? [[m1 M1] [m2 M2]] (<= m2 m1 M1 M2))

(defn cut-dim [[m1 M1] [m2 M2]]
  (cond
    (<= m2 m1 M1 M2) [[m1 M1]]
    (< m1 m2 M2 M1) [[m1 (dec m2)] [m2 M2] [(inc M2) M1]]
    (<= m2 m1 M2) (if (= M1 M2) [[m1 M2]] [[m1 M2] [(inc M2) M1]])
    (<= m2 M1 M2) (if (= m1 m2) [[m2 M1]] [[m1 (dec m2)] [m2 M1]])))

(defn cut-block [block1 block2]
  (let [i (map cut-dim block1 block2)]
    (if (every? some? i)
      (let [[ix iy iz] i]
        (remove #(every? true? (map subdim? % block2))
                (for [bx ix by iy bz iz] [bx by bz])))
      [block1])))

(defn apply-step [blocks [op & block]]
  (let [new (mapcat #(cut-block % block) blocks)]
    (if op (conj new block) new)))

(defn reduce-step [[action [mx Mx] [my My] [mz Mz]]]
  [action
   [(max mx -50) (min Mx 50)]
   [(max my -50) (min My 50)]
   [(max mz -50) (min Mz 50)]])

(let [steps (map read-step (line-seq (java.io.BufferedReader. *in*)))
      reduced-steps (filter #(every? (partial apply <=) (rest %)) (map reduce-step steps))]
  (doseq [s [reduced-steps steps]]
    (->> (reduce apply-step [] s)
         total-vol
         println)))
