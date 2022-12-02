(defn convert-part1 
  [s]
  (condp = s
   "A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors))

(defn convert-part2 
  [s]
  (condp = s
   "A" :rock
   "B" :paper
   "C" :scissors
   "X" :lose
   "Y" :draw
   "Z" :win))

(defn parse
  [f]
  (fn [s] (mapv f (.split s " "))))

(defn score
  [[a b]]
  (+
    (condp = b :rock 1 :paper 2 :scissors 3)
    (condp = [a b]
     [:rock :paper] 6
     [:paper :scissors] 6
     [:scissors :rock] 6
     [:paper :rock] 0
     [:scissors :paper] 0
     [:rock :scissors] 0
     3)))

(defn choose
  [[a b]]
  [a (condp = [a b]
       [:rock :lose]     :scissors
       [:rock :win]      :paper
       [:paper :lose]    :rock
       [:paper :win]     :scissors
       [:scissors :lose] :paper
       [:scissors :win]  :rock
       a)])
  
;; (def input ["A Y" "B X" "C Z"])
(def input (.split (slurp "input.txt") "\n"))

(println "part 1" (reduce + (mapv score (mapv (parse convert-part1) input))))
(println "part 2" (reduce + (mapv score (mapv choose (mapv (parse convert-part2) input)))))
