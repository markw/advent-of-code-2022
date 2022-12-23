(defn max-y [chamber]
  (if (empty? chamber) 
    -1
    (second (last chamber))))

(defn render-chamber
  [chamber]
  (loop [y (max-y chamber)]
    (when (<= 0 y)
      (print "|")
      (doseq [x (range 7)]
        (print (if (contains? chamber [x y]) "#" ".")))
      (println "|")
      (recur (dec y))))
  (println "+-------+\n"))

(defn on-another-rock? [chamber rock]
  (some #(contains? chamber %) rock))
  
(defn fall [chamber rock]
  ;(println "fall" chamber rock)
  (let [new-rock (mapv (fn [[x y]] [x (dec y)]) rock)
        min-y (apply min (map second new-rock))]
    (if (or
          (on-another-rock? chamber new-rock)
          (< min-y 0)) 
        rock new-rock)))

(defn in-bounds?
  [rock]
  (every?
    #(and (<= 0 %)(>= 6 %))
    (map first rock)))

(defn left [rock]
  (let [new-rock (mapv (fn [[x y]] [(dec x) y]) rock)]
    (if (in-bounds? new-rock) new-rock rock)))

(defn right [rock]
  (let [new-rock (mapv (fn [[x y]] [(inc x) y]) rock)]
    (if (in-bounds? new-rock) new-rock rock)))

(defn jet-rock [ch rock chamber]
  (let [new-rock (if (= ch \<)(left rock)(right rock))]
    ;(println "jet-rock" new-rock)
    (if (on-another-rock? chamber new-rock)
      rock
      new-rock)))
  
(defn make-rock [chamber shape]
  (let [y (+ 4 (max-y chamber))]
    ;(println "make-rock" shape "highest y" (max-y chamber))
    (condp = shape
      :minus (for [x [2 3 4 5]] [x y])
      :plus  (list [3,y] [3,(inc y)] [3,(+ 2 y)] [2 (inc y)] [4 (inc y)])
      :L (list [2 y][3 y][4 y][4 (inc y)][4 (+ 2 y)])
      :I (list [2 y][2 (+ 1 y)][2 (+ 2 y)][2 (+ 3 y)])
      :square (list [2 y][2 (+ 1 y)][3 y][3 (+ 1 y)]))))

(defn make-chamber []
  (sorted-set-by 
    (fn [a b]
      (let [c (compare (second a)(second b))]
        (if (not= 0 c) c (compare a b))))))

(defn solve
  [file-name]
  (loop [add-count 0
         chamber (make-chamber)
         shapes (cycle [:minus :plus :L :I :square])
         rock (make-rock chamber (first shapes))
         jet? true
         jets (cycle (map char (.trim (slurp file-name))))]
    ;(println "rock" rock "chamber" chamber)
    (let [shape (first shapes)]
      ;(println "rock" rock "shape" shape jet? (first jets))
      (cond
        (= 2022 add-count) chamber
        jet? (do
               ;(println "jet" (first jets) "rock" rock)
               ;(when (= 9 add-count)
                 ;(render-chamber (apply conj chamber rock))
               (recur add-count chamber shapes (jet-rock (first jets) rock chamber) false (rest jets)))
        :default
        (let [new-rock (fall chamber rock)]
          ;(println "new-rock" new-rock "rock" rock "shape" shape)
          (if (identical? new-rock rock)
            (let [new-chamber (apply conj chamber rock)
                  new-shapes (rest shapes)]
              ;(println "new-chamber" new-chamber)
              (recur (inc add-count)
                new-chamber
                new-shapes
                (make-rock new-chamber (first new-shapes))
                true
                jets))
            (recur add-count chamber shapes new-rock true jets)))))))

;(render-chamber (solve "sample.txt"))
(println "part 1" ((comp inc second last)(solve "input.txt")))
