#!/usr/bin/env bb

(defn large? [cave]
  (Character/isUpperCase (first cave)))

(def count-paths
  (memoize
   (fn [links here visited]
     (if (= here "end")
       1
       (let [candidates (set/difference (get links here) (set (remove large? visited)))]
         (reduce + (map #(count-paths links % (conj visited here)) candidates)))))))

(def count-paths-can-repeat
  (memoize
   (fn [links here visited]
     (if (= here "end")
       1
       (let [visited-small (set (remove large? visited))
             can-visit-again (set/intersection (get links here)
                                               (set/difference visited-small #{"start" "end"}))
             candidates (set/difference (get links here) visited-small)]
         (+ (reduce + (map #(count-paths-can-repeat links % (conj visited here)) candidates))
            (reduce + (map #(count-paths links % (conj visited here)) can-visit-again))))))))

(let [lines (map #(str/split % #"-") (line-seq (java.io.BufferedReader. *in*)))
      links (reduce (fn [links link]
                      (let [c1 (first link)
                            c2 (second link)
                            d1 (get links c1 (set ()))
                            d2 (get links c2 (set ()))]
                        (assoc links
                               c1 (conj d1 c2)
                               c2 (conj d2 c1))))
                    {} lines)]
  (println (count-paths links "start" (set [])))
  (println (count-paths-can-repeat links "start" (set []))))
