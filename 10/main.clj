(def input (.split (slurp "input.txt") "\n"))

(defn parse-instruction
  [s]
  (let [tokens (.split s " ")]
    (condp = (first tokens)
      "noop" [1 0]
      "addx" [2 (Integer. (second tokens))])))

(defn parse-program
  [ss]
  (reduce
    (fn [acc [n x]]
      (let [[n0 x0] (last acc)]
        (conj acc [(+ n n0)(+ x x0)])))
    [[0 1]]
    (map parse-instruction ss)))

(defn register-x
  [program n]
  (second 
    (last 
      (take-while #(> n (first %)) program))))

(defn signal-strength
  [input n]
  (* n (register-x input n)))

(defn draw-pixel
  [program n]
  (let [sprite-pos (register-x program (inc n))
        pixel-pos (mod n 40)]
    (if (and (>= pixel-pos (dec sprite-pos))
             (<= pixel-pos (inc sprite-pos))) "#" ".")))

(defn render
  [pixels]
  (doseq [line (partition 40 pixels)]
    (println (reduce str line))))

(let [program (parse-program input)]
  (println "part 1"
    (reduce +
      (mapv 
        (partial signal-strength program) 
        [20 60 100 140 180 220])))

  (println "part 2") 
  (render (map (partial draw-pixel program)(range 240))))
