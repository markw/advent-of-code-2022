(def input (.split (slurp "input.txt") "\n"))

(def grid (mapv (fn [s] 
                  (mapv #(Integer. %)
                        (.split s ""))) input))

(defn visible-1?
  [grid r c dr dc]
  (let [tree (get-in grid [r c])]
    (loop [r r c c]
      (let [next-r (+ r dr) 
            next-c (+ c dc)
            next-tree (get-in grid [next-r next-c])]
        (cond
          (nil? next-tree) true
          (<= tree next-tree) false
          :default (recur next-r next-c))))))

(defn visible?
  [grid r c]
  (or 
    (visible-1? grid r c 0 1)
    (visible-1? grid r c 0 -1)
    (visible-1? grid r c 1 0)
    (visible-1? grid r c -1 0)))

(println "part 1"
  (count
    (filter true?
      (for [r (range (count grid))
            c (range (count grid))]
        (visible? grid r c)))))

(defn scenic-score-1
  [grid r c dr dc]
  (let [tree (get-in grid [r c])]
    (loop [score 0 r r c c]
      (let [next-r (+ r dr) 
            next-c (+ c dc)
            next-tree (get-in grid [next-r next-c])]
        (cond
          (nil? next-tree) score
          (<= tree next-tree) (inc score)
          :default (recur (inc score) next-r next-c))))))

(defn scenic-score
  [grid r c]
  (* 
    (scenic-score-1 grid r c 0 1)
    (scenic-score-1 grid r c 0 -1)
    (scenic-score-1 grid r c 1 0)
    (scenic-score-1 grid r c -1 0)))

(println "part 2"
  (apply max
         (for [r (range (count grid))
               c (range (count grid))]
           (scenic-score grid r c))))
