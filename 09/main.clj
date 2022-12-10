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
  (if (adjacent? h t)
    t
    (let [dr (- hr tr)
          dc (- hc tc)
          new-tr (cond
                   (pos? dr) (inc tr)
                   (neg? dr) (dec tr)
                   :default tr)
          new-tc (cond
                   (pos? dc)(inc tc)
                   (neg? dc)(dec tc)
                   :default tc)]
      [new-tr new-tc])))

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

(defn do-move
  [state move]
  (let [{positions :positions visited :visited} state]
    (adjust-tails 
      (assoc positions 0 (move-h (first positions) move))
      visited)))

(defn init
  [n]
  {:positions (vec (repeat n [0 0]))
   :visited   (vec (repeat n #{}))})

(let [lines (.split (slurp "input.txt") "\n")
      moves (mapcat parse-move lines)]
  (println "part 1"
    (count 
      (last 
        (:visited
          (reduce do-move (init 2) moves)))))
  (println "part 2"
    (count 
      (last 
        (:visited
          (reduce do-move (init 10) moves))))))
