(defn toInt [s] 
  (if (empty? s) 0 (Integer. s)))

(def lines (.split (slurp "input.txt") "\n"))

(def calories
  (reverse
    (sort
      (map 
        #(reduce + (map toInt %))
        (partition-by 
          empty?
          lines))))) 

(println "part 1" (first calories))
(println "part 2" (reduce + (take 3 calories)))

;; do a single-pass instead

(def single-pass-calories
  (reverse
    (sort
      (loop
        [total 0 acc [] lines lines]
        (if (empty? lines)
          acc
          (let [n (toInt (first lines))]
            (if (zero? n)
              (recur 0 (conj acc total)(rest lines))
              (recur (+ n total) acc (rest lines)))))))))


(println "part 1 (single-pass)" (first single-pass-calories))
(println "part 2 (single-pass)" (reduce + (take 3 single-pass-calories)))
        

