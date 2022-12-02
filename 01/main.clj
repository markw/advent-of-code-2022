(defn toInt [s] 
  (if (empty? s) 0 (Integer. s)))

(def calories
  (reverse
    (sort
      (map 
        #(reduce + (map toInt %))
        (partition-by 
          empty?
          (.split (slurp "input.txt") "\n")))))) 

(println "part 1" (first calories))
(println "part 2" (reduce + (take 3 calories)))
