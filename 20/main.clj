(defn parse-input
  [file-name]
  (vec 
    (map-indexed #(vector (Integer. %2) %1) 
                 (.split (slurp file-name) "\n"))))

(defn move
  [coll index x]
  (let [max-index (dec (count coll))
        temp-index (rem (+ index x) max-index)
        new-index (if (neg? temp-index) (+ temp-index max-index) temp-index)
        distance (- new-index index)]
    (loop [mixed coll i index d distance]
      (cond
        (zero? d) mixed

        (neg? d)
        (recur
          (assoc mixed 
                 i (get mixed (dec i))
                 (dec i) (get mixed i))
          (dec i)
          (inc d))

        :else
        (recur
          (assoc mixed 
                 i (get mixed (inc i))
                 (inc i) (get mixed i))
          (inc i)
          (dec d))))))

(defn mix
  [coll]
  (loop [coll coll mixed coll]
    (if (empty? coll)
      mixed
      (let [[h & t] coll]
        (recur
          t
          (move mixed 
                (.indexOf mixed h)
                (first h)))))))

(defn find-zero
  [coll]
  (loop [[h & t] coll n 0]
    (if (zero? (first h))
      n
      (recur t (inc n)))))


(let [coll (parse-input "input.txt")
      mixed (mix coll)
      zero-index (find-zero mixed)]
  ;(println "zero-index" zero-index)
  (println "part 1" 
           (reduce
             (fn [acc index]
               (+ acc (first (get mixed (rem index (count coll))))))
             0
             (mapv 
               #(+ zero-index %) 
               [1000 2000 3000]))))
