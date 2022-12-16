(defn parse-line
  [line]
  (let [matches (re-matches 
                  #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
                  line)]
    (mapv
      (fn [[sensor beacon]] 
        (vector
          (Integer. sensor) 
          (Integer. beacon))) 
      (partition 2 (mapv matches (range 1 5))))))

(defn abs [x] (Math/abs x))

(defn distance
  [[x1 y1][x2 y2]]
  (+
   (abs (- x1 x2))
   (abs (- y1 y2))))

(defn exclude-range
  [[x1 y1 :as xy1][x2 y2 :as xy2] y]
  (let [dist (distance xy1 xy2)
        diff (- dist (abs (- y y1)))
        min-x (- x1 diff)
        max-x (+ x1 diff)]
    (if (> min-x max-x)
      nil
      [min-x max-x])))

(defn widen
  [[min1 max1 :as a][min2 max2 :as b]]
  ;(println "***" a b)
  (cond
    (empty? a) b
    (= 1 (count a)) a
    (and (< min2 min1)(> max2 max1)) b
    (and (> min2 min1)(< max2 max1)) a
    (> min2 max1) [(inc max1)]
    (< min1 min2) [min1 max2]
    (> max2 max1) [min1 max2]
    (< max2 max1) a))
    
(println "Part 2" 
  (let [report (mapv parse-line (.split (slurp "input.txt") "\n"))
        beacons (set (mapv second report))]

    (loop [y 4000000]
      (when (zero? (mod y 100000))(println y))
      (let [excluded (sort
                      (filter
                        some?
                        (map
                          (fn [[sensor beacon]] (exclude-range sensor beacon y))
                          report)))
            widened (reduce widen [] excluded)]
        (if (= 1 (count widened))
          (+ y (* 4000000(first widened)))
          (recur (dec y)))))))
        
