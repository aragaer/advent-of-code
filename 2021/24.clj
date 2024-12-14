#!/usr/bin/env bb
(def iter-len 18)
(def param-offsets [5 15 4])

(defn read-ins [line]
  (let [[line & comments] (str/split line #";")
        [op & args] (str/split line #" ")
        args (map edn/read-string args)]
    (vec (conj args op))))

(defn converge [[stack diffs] param]
  (let [pop? (not= 1 (nth param 2))
        prev (if (empty? stack) [0 0] (first stack))
        diff (+ (second prev) (first param))
        good? (> 9 diff)]
    (if pop?
      (assert good? "param isn't good but does pop the stack")
      (assert (not good?) "param is good but does not pop the stack"))
    [(if good?
       (rest stack)
       (conj stack param))
     (if good?
       (assoc diffs [(nth prev 3) (nth param 3)] diff)
       diffs)]))

(defn num-pair [cmp diff]
  (let [low (max 1 (- 1 diff))
        high (min 9 (- 9 diff))
        val (cmp low high)]
    [val (+ val diff)]))

(defn diffs-to-number [diffs func]
  (let [res (into {} (map #(zipmap (first %) (func (second %))) diffs))]
    (apply str (map res (range 14)))))

(let [insns (map read-ins (line-seq (java.io.BufferedReader. *in*)))
      [p1 p2 p3] (for [o param-offsets]
                   (map #(last (nth insns %)) (range o (count insns) iter-len)))
      params (mapv vector p1 p2 p3 (range))
      [_ diffs] (reduce converge '() params)]
  (doseq [f [(partial num-pair max)
             (partial num-pair min)]]
    (println (diffs-to-number diffs f))))
