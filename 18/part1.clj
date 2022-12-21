(defn cube
  [x1 y1 z1]
  (let [x2 (inc x1)
        y2 (inc y1)
        z2 (inc z1)
        front   (set (for [x [x1 x2] y [y1 y2]] [x   y z1]))
        back    (set (for [x [x1 x2] y [y1 y2]] [x   y z2]))
        bottom  (set (for [x [x1 x2] z [z1 z2]] [x  y1  z]))
        top     (set (for [x [x1 x2] z [z1 z2]] [x  y2  z]))
        left    (set (for [y [y1 y2] z [z1 z2]] [x1  y  z]))
        right   (set (for [y [y1 y2] z [z1 z2]] [x2  y  z]))]
    (list front back bottom top left right)))
                

(defn parse-input
  [file-name]
  (map 
    #(apply cube %)
    (map
      (fn [s]
        (mapv 
          #(Integer. %)
          (.split s ",")))
      (.split (slurp file-name) "\n"))))


(defn solve
  [file-name]
  (let [cubes (parse-input file-name)]
    (count (filter 
             (fn [[k v]](= 1 v))
             (frequencies (reduce concat cubes))))))

(prn (solve "input.txt"))
