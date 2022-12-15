(defn make-range
  [a b]
  (let [[a' b'] (sort [a b])]
    (range a' (inc b')))) 

(defn segment
  [[x1 y1][x2 y2]]
  (into #{}
    (if (= x1 x2)
      (for [y (make-range y1 y2)] [x1 y])
      (for [x (make-range x1 x2)] [x y1]))))

(defn parse-pair
  [s]
  (mapv #(Integer. %) (.split s ",")))

(defn parse-line
  [line]
  (mapv parse-pair (.split line " -> ")))

(defn rock-line
  [line]
  (loop [[a b & t :as points] (parse-line line) rocks #{}]
    (if (nil? b)
      rocks
      (recur (rest points) 
             (apply conj rocks (segment a b))))))

(defn part-1
  [rocks]
  (let [start [500 0]
        abyss (apply max (map second rocks))]

    (loop [[x y :as xy] start occupied rocks sand-count 0]
      (let [down [x (inc y)]
            down-left [(dec x)(inc y)]
            down-right [(inc x)(inc y)]
            open? #(not (contains? occupied %))]
        ;(println abyss x y)
        (cond 
          (> y abyss)
          sand-count
          
          (open? down)
          (recur down  occupied sand-count)

          (open? down-left)
          (recur down-left occupied sand-count)

          (open? down-right)
          (recur down-right occupied sand-count)
          
          :default
          (recur start 
                 (conj occupied xy)
                 (inc sand-count)))))))

(defn part-2
  [rocks]
  (let [start [500 0]
        floor (+ 2 (apply max (map second rocks)))]
        
    (loop [[x y :as xy] start occupied rocks sand-count 0]
      (let [down [x (inc y)]
            down-left [(dec x)(inc y)]
            down-right [(inc x)(inc y)]
            open? #(and 
                     (> floor (second %))
                     (not (contains? occupied %)))]
        (cond 
          (open? down)
          (recur down occupied sand-count)

          (open? down-left)
          (recur down-left occupied sand-count)

          (open? down-right)
          (recur down-right occupied sand-count)
          
          :default
          (if (= xy start)
            (inc sand-count)
            (recur start 
                   (conj occupied xy)
                   (inc sand-count))))))))

(let [rocks (reduce 
              #(apply conj %1 (rock-line %2))
              #{}
              (.split (slurp "input.txt") "\n"))]
  (println "part 1" (part-1 rocks))
  (println "part 2" (part-2 rocks)))
