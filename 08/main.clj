(def input (.split (slurp "input.txt") "\n"))

(def grid (mapv (fn [s] 
                  (mapv #(Integer. %)
                        (.split s ""))) input))

(defn visible-on-row?
  [grid r c indices]
  (let [cell (get-in grid [r c])
        cells (mapv #(get-in grid [r %]) indices)]
    (every? #(> cell %) cells)))

(defn visible-from-left?
  [grid r c]
  (visible-on-row? grid r c (range 0 c)))

(defn visible-from-right?
  [grid r c]
  (visible-on-row? grid r c (range (inc c) (count grid))))

(defn visible-on-col?
  [grid r c indices]
  (let [cell (get-in grid [r c])
        cells (mapv #(get-in grid [% c]) indices)]
    (every? #(> cell %) cells)))

(defn visible-from-above?
  [grid r c]
  (visible-on-col? grid r c (range 0 r)))

(defn visible-from-below?
  [grid r c]
  (visible-on-col? grid r c (range (inc r) (count grid))))

(defn visible?
  [grid r c]
  (or
    (visible-from-left? grid r c)
    (visible-from-right? grid r c)
    (visible-from-above? grid r c)
    (visible-from-below? grid r c)))

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
            t (get-in grid [next-r next-c])]
        (cond
          (nil? t) score
          (<= tree t) (inc score)
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
