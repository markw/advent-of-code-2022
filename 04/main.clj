(defn parse-section
  [ss]
  (mapv #(Integer. %)(.split ss "-")))

(defn parse-pair
  [s]
  (mapv parse-section (.split s ",")))

(defn fully-contains?
  [[[a b][c d]]]
  (or 
    (and (<= a c)(>= b d))
    (and (>= a c)(<= b d))))

(defn disjunct?
  [[[a b][c d]]]
  (or 
    (and (< a c)(< b c))
    (and (> a d)(> b d))))

(def overlaps? (complement disjunct?))

(def pair (map parse-pair (.split (slurp "input.txt") "\n")))

(assert (disjunct? (parse-pair "1-5,6-11")))
(assert (disjunct? (parse-pair "6-11,1-5")))

(assert (overlaps? (parse-pair "1-7,7-11")))
(assert (overlaps? (parse-pair "83-89,9-83")))

(println "part 1" (count (filter fully-contains? pair)))
(println "part 2" (count (filter overlaps? pair)))

