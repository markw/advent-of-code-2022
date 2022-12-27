(defn parse-input
  [file-name]
  (map-indexed #(vector (Integer. %2) %1) 
              (.split (slurp file-name) "\n")))

(defn move
  [alist index entry]
  (let [max-index (dec (count alist))
        x (first entry)
        temp-index (rem (+ index x) max-index)
        new-index (if (neg? temp-index) (+ temp-index max-index) temp-index)]
    (doto alist
      (.remove entry)
      (.add new-index entry))
    alist))

(defn mix
  [coll mixed]
  (loop [coll coll mixed mixed]
    (if (empty? coll)
      mixed
      (let [[h & t] coll]
        (recur
          t
          (move mixed 
                (.indexOf mixed h)
                h))))))

(defn find-zero
  [alist]
  (loop [n 0]
    (if (zero? (first (.get alist n)))
      n
      (recur (inc n)))))

(time
  (let [coll (parse-input "input.txt")
        mixed (mix coll (java.util.ArrayList. coll))
        zero-index (find-zero mixed)]
    (println "part 1" 
             (reduce
               (fn [acc index]
                 (+ acc (first (.get mixed (rem index (count coll))))))
               0
               (mapv 
                 #(+ zero-index %) 
                 [1000 2000 3000])))))

(time
  (let [coll (mapv 
               (fn [[value index]] [(* value 811589153) index])
               (parse-input "input.txt"))
        mixed (loop [mixed (java.util.ArrayList. coll) n 10]
                (if (zero? n) mixed
                  (recur (mix coll mixed) (dec n))))
        zero-index (find-zero mixed)]
    ;(println "zero-index" zero-index)
    (println "part 2"
             (reduce
               (fn [acc index]
                 (+ acc (first (.get mixed (rem index (count coll))))))
               0
               (mapv 
                 #(+ zero-index %) 
                 [1000 2000 3000])))))
