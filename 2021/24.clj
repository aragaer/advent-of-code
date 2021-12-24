#!/usr/bin/env bb

(defn binop [op]
  (fn [[regs input] reg var]
    [(assoc regs reg (op (regs reg) (get regs var var)))
     input]))

(def my-func
  (memoize (fn [v n [v1 v2 v3]]
             (let [x (mod v 26)
                   x (+ x v1)
                   x (if (= x n) 0 1)
                   y (inc (* x 25))]
               (+ (* (quot v v3) y) (* (+ v2 n) x))))))

(defn my-func-2 [v n [v1 v2 v3]]
  (let [x (if (empty? v) 0 (first v))
        v (if (= v3 26) (rest v) v)]
    (if (= (+ v1 x) n)
      v
      (conj v (+ v2 n)))))

(def params
  [[12 15 1] [14 12 1] [11 15 1] [-9 12 26] [-7 15 26]
   [11 2 1] [-1 11 26] [-16 15 26] [11 10 1] [-15 2 26]
   [10 0 1] [12 0 1] [-4 15 26] [0 15 26]])

(def ALU
  {"inp" (fn [[regs input] reg]
           (let [param (nth params (- (count params) (count input)))
                 my (my-func-2 (:m regs) (first input) param)
                 regs (assoc regs reg (first input) :m my)]
             (println regs param (reduce #(+ (* %1 26) %2) 0 (reverse my)))
             [regs (subvec input 1)]))
   "add" (binop +)
   "div" (binop quot)
   "mod" (binop mod)
   "mul" (binop *)
   "eql" (binop #(if (= %1 %2) 1 0))
   "prn" (fn [[regs input]]
           (println regs)
           [regs input])
   })

(def arg-reader {"x" :x "y" :y "z" :z "w" :w})

(defn read-arg [arg]
  (get arg-reader arg (edn/read-string arg)))

(defn read-ins [line]
  (let [[line & comments] (str/split line #";")
        [op & args] (str/split line #" ")
        args (map read-arg args)]
    (vec (conj args op))))

(defn execute [state [op & args]]
  (apply (ALU op) state args))

(defn converge [[stack res1 res2] param]
  (let [pop? (= 26 (nth param 2))
        prev (if (empty? stack) [0 0] (first stack))
        diff (+ (second prev) (first param))
        good? (> 9 diff)
        new-stack (cond-> stack
                    pop? rest
                    (not good?) (conj param))
        new-res1 (if good?
                  (let [nums (if (> 0 diff)
                               [9 (+ 9 diff)]
                               [(- 9 diff) 9])]
                    (assoc res1
                           (nth prev 3) (first nums)
                           (nth param 3) (second nums)))
                  res1)
        new-res2 (if good?
                  (let [nums (if (> 0 diff)
                               [(- 1 diff) 1]
                               [1 (+ 1 diff)])]
                    (assoc res2
                           (nth prev 3) (first nums)
                           (nth param 3) (second nums)))
                  res2)]
    [new-stack new-res1 new-res2]))

(let [insns (map read-ins (line-seq (java.io.BufferedReader. *in*)))
      regs (zipmap [:x :y :z :w :m] (repeat 0))
      iter-steps (map #(* % 18) (range (quot (count insns) 18)))
      par-x (map #(last (nth insns (+ % 5))) iter-steps)
      par-y (map #(last (nth insns (+ % 15))) iter-steps)
      par-z (map #(last (nth insns (+ % 4))) iter-steps)
      params (mapv vector par-x par-y par-z (range))]
  (let [[_ res1 res2] (reduce converge '() params)]
    (println (apply str (map res1 (range 14))))
    (println (apply str (map res2 (range 14))))))
