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
  ([coll] (mix coll coll))
  ([coll mixed]
   (loop [coll coll mixed mixed]
     (if (empty? coll)
       mixed
       (let [[h & t] coll]
         (recur
           t
           (move mixed 
                 (.indexOf mixed h)
                 (first h))))))))

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

(let [coll (mapv 
             (fn [[value index]] [(* value 811589153) index])
             (parse-input "input.txt"))
      mixed (loop [mixed coll n 10]
              (if (zero? n) mixed
                (recur (mix coll mixed) (dec n))))
      zero-index (find-zero mixed)]
  ;(println "zero-index" zero-index)
  (println "part 2"
           (reduce
             (fn [acc index]
               (+ acc (first (get mixed (rem index (count coll))))))
             0
             (mapv 
               #(+ zero-index %) 
               [1000 2000 3000]))))
