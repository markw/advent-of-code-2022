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

(defn excluded
  [[x1 y1 :as xy1][x2 y2 :as xy2] y]
  (let [dist (distance xy1 xy2)
        diff (- dist (abs (- y y1)))
        min-x (- x1 diff)
        max-x (+ x1 diff)]
    ;(println diff min-x max-x)
    (for [x (range min-x (inc max-x))] [x y])))

;(println (excluded [2 3][5 7] 9))         

(println
  (let [report (mapv parse-line (.split (slurp "input.txt") "\n"))
        beacons (set (mapv second report))
        excluded (set
                  (filter
                    not-empty
                    (mapcat
                      (fn [[sensor beacon]] (excluded sensor beacon 2000000))
                      report)))
        beacons-in-excluded (filter 
                              #(contains? excluded %)
                              beacons)]
    
    (- (count excluded) 
       (count beacons-in-excluded)))) 
        
