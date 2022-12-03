(require 'clojure.set)
(defn priority
  [ch]
  (if (Character/isLowerCase ch)
    (- (int ch) 96)
    (- (int ch) 38)))

(defn find-priority
  [sets]
  (priority
    (first
      (apply clojure.set/intersection sets))))
  
(defn find-compartment-priority 
  [s]
  (let [length (/ (count s) 2)]
    (find-priority 
      (map set (partition length s)))))

(defn find-badge-priority 
  [ss]
  (find-priority (map set ss)))

(def input (.split (slurp "input.txt") "\n"))

(println "part 1"
  (reduce +
    (map 
      find-compartment-priority 
      input)))

(println "part 2"
  (reduce +
    (map 
      find-badge-priority
      (partition 3 input))))
