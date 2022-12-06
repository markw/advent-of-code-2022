(def input (slurp "input.txt"))

(defn packet?
  [cs packet-size]
  (= packet-size (count (set (take packet-size cs)))))

(defn find-packet
  [s size]
  (loop [n 0 s s]
    (if (packet? s size)
      (+ size n)
      (recur (inc n)(rest s)))))

(println "part 1" (find-packet input 4))
(println "part 2" (find-packet input 14))
