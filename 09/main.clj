(defn parse-move
  [s]
  (let [[a b] (.split s " ")]
    (repeat (Integer. b) a)))

(defn move-h
  [[r c] move]
  (condp = move
    "U" [(dec r) c]
    "D" [(inc r) c]
    "L" [r (dec c)]
    "R" [r (inc c)]))

(defn adjacent?
  [[hr hc] [tr tc]]
  (let [dr (Math/abs (- tr hr))
        dc (Math/abs (- tc hc))]
    (and (< dr 2)(< dc 2))))

(defn move-t
  [[hr hc :as h] [tr tc :as t]]
  (let [dr (- hr tr)
        dc (- hc tc)]
    (if (adjacent? h t)
      t
      (condp = [dr dc]
        [ 0  2] [tr (inc tc)]
        [ 1  2] [(inc tr) (inc tc)]
        [-1  2] [(dec tr) (inc tc)]
        [ 0 -2] [tr (dec tc)]
        [ 1 -2] [(inc tr) (dec tc)]
        [-1 -2] [(dec tr) (dec tc)]
        [ 2  0] [(inc tr) tc]
        [ 2  1] [(inc tr) (inc tc)]
        [ 2 -1] [(inc tr) (dec tc)]
        [-2  0] [(dec tr) tc]
        [-2  1] [(dec tr) (inc tc)]
        [-2 -1] [(dec tr) (dec tc)]
        [-2 -2] [(dec tr) (dec tc)]
        [-2  2] [(dec tr) (inc tc)]
        [ 2 -2] [(inc tr) (dec tc)]
        [ 2  2] [(inc tr) (inc tc)]))))

(defn do-moves-part-1
  [moves]
  (loop [moves moves h [0 0] t [0 0] visited #{[0 0]}]
    (let [move (first moves)]
      (if (nil? move)
        (count visited)
        (let [new-h (move-h h move)
              new-t (move-t new-h t)]
          (recur (rest moves) new-h new-t (conj visited new-t)))))))
  
(defn adjust-tails
  [positions visited]
  (loop [index 1 positions positions visited visited]
    (if (>= index (count positions))
      {:positions positions :visited visited}
      (let [h (get positions (dec index))
            t (get positions index)
            next-t (move-t h t)]
        (recur 
          (inc index)
          (assoc positions index next-t)
          (assoc visited index (conj (get visited index) next-t)))))))

(defn do-move-part-2
  [state move]
  (let [positions (:positions state)
        visited (:visited state)]
    (adjust-tails 
      (assoc positions 0 (move-h (first positions) move))
      visited)))

(def positions (vec (repeat 10 [0 0])))
(def visited   (vec (repeat 10 #{})))

(let [moves (mapcat parse-move (.split (slurp "input.txt") "\n"))]
  (println "part 1" (do-moves-part-1 moves))
  (println "part 2"
    (count 
      (last (:visited
              (reduce
                do-move-part-2
                {:positions positions :visited visited}
                moves))))))
