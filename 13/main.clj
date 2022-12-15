(def IN_ORDER -1)
(def OUT_OF_ORDER 1)
(def DONT_KNOW 0)

(defn parse-input
  [file-name]
  (map read-string
       (filter
         not-empty
         (.split (slurp file-name) "\n"))))

(defn in-order?
  [a b]
  (loop [a a b b]
    (let [a' (first a)
          b' (first b)]
      (cond
        (and (nil? a')(nil? b'))
        DONT_KNOW

        (nil? a') 
        IN_ORDER

        (nil? b') 
        OUT_OF_ORDER

        (and (coll? a')(coll? b'))
        (let [result (in-order? a' b')]
          (if (= DONT_KNOW result)
            (in-order? (rest a) (rest b))
            result))

        (and (coll? a')(int? b'))
        (let [result (in-order? a' (vector b'))]
          (if (= DONT_KNOW result)
            (in-order? (rest a) (rest b))
            result))

        (and (int? a')(coll? b'))
        (let [result (in-order? (vector a') b')]
          (if (= DONT_KNOW result)
            (in-order? (rest a) (rest b))
            result))

        (< a' b') 
        IN_ORDER

        (> a' b') 
        OUT_OF_ORDER

        :default (recur (rest a)(rest b))))))


(let [packets (parse-input "input.txt")]
  (println "part 1"
    (reduce +
      (map-indexed
        (fn [index [a b]] (if (= IN_ORDER (in-order? a b)) (inc index) 0))
        (partition 2 packets))))
  (println "part 2"
    (let [sorted (sort in-order? (conj packets [[2]] [[6]]))]
      (* (inc (.indexOf sorted [[2]]))
         (inc (.indexOf sorted [[6]]))))))
    

(comment ")

(assert (= IN_ORDER (in-order? [1,1,3,1,1] [1,1,5,1,1])))
(assert (= IN_ORDER (in-order? [[1],[2,3,4]] [[1],4])))
(assert (= OUT_OF_ORDER (in-order? [7,7,7,7] [7,7,7])))
(assert (= OUT_OF_ORDER (in-order? [9] [[8,7,6]])))
(assert (= IN_ORDER (in-order? [[4,4],4,4] [[4,4],4,4,4])))
(assert (= IN_ORDER (in-order? [] [3])))
(assert (= OUT_OF_ORDER (in-order? [[[]]] [[]])))
(assert (= OUT_OF_ORDER (in-order? [1,[2,[3,[4,[5,6,7]]]],8,9] [1,[2,[3,[4,[5,6,0]]]],8,9])))

;; my test cases
(assert (= DONT_KNOW (in-order? [1 2 3][1 2 3])))
(assert (= DONT_KNOW (in-order? [1 2 3][[1][2][3]])))

(assert (= OUT_OF_ORDER (in-order? 
                          [[1,2][2,3]]
                          [[1,2][2,2]])))

(assert (= IN_ORDER (in-order? 
                      [[1,2][2,3][3,4]]
                      [[1,2][2,3][4]])))

(assert (= IN_ORDER (in-order? 
                      [1,2,3]
                      [[1,2][2,3][3,4]])))

(assert (=  IN_ORDER (in-order? 
                        [[1][2][3][]]
                        [1,2,3,4])))

")
