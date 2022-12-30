(defn parse-line [s] (mapv char s))

(defn parse-input
  [file-name]
  (.split (slurp file-name) "\n"))

(defn parse-board 
  [input]
  (mapv parse-line (take-while not-empty input)))

(defn parse-moves 
  [input]
  (let [s (last (vec input))]
    (loop [[h & t] s token "" acc []]
      (cond
        (nil? h) 
        (conj acc (Integer. token))

        (or (= \R h)(= \L h)) 
        (recur t ""
               (apply conj acc [ (Integer. token) h]))
        
        :else
        (recur t (str token h) acc)))))

(defn tile?
  [board pos]
  (let [tile (get-in board pos)]
    (or (= \. tile)(= \# tile))))

(defn start-row
  [board c]
  (loop [r 0]
    (if (tile? board [r c]) 
      [r c] 
      (recur (inc r)))))

(defn last-row
  [board c]
  (loop [r (dec (count board))]
    (if (tile? board [r c])
      [r c]
      (recur (dec r)))))

(defn start-col
  [board r]
  (loop [c 0]
    (if (tile? board [r c])
      [r c]
      (recur (inc c)))))

(defn last-col
  [board r]
  (loop [c (dec (count (get board r)))]
    (if (tile? board [r c])
      [r c]
      (recur (dec c)))))

(defn start-pos
  [board [r c :as pos] dir]
  (condp = dir
    :right  (start-col board r)
    :left  (last-col board r)
    :down (start-row board c)
    :up (last-row board c)))

(defn start [board] (start-pos board [0 0] :right))

(defn next-pos
  [[r c] dir]
  (condp = dir
    :right [r (inc c)]
    :down  [(inc r) c]
    :left  [r (dec c)]
    :up    [(dec r) c]))

(defn turn-left
  [dir]
  (condp = dir
    :left  :down
    :right :up
    :up    :left
    :down  :right))

(defn turn-right
  [dir]
  (condp = dir
    :left  :up
    :right :down
    :up    :right
    :down  :left))

(defn get-next-tile
  [board [r c :as pos] dir]
  (let [next-pos (next-pos pos dir)
        next-tile (get-in board next-pos)]
    (if (or 
          (nil? next-tile)
          (= \space next-tile))
      (let [wrap-pos (start-pos board next-pos dir)
            wrap-tile (get-in board wrap-pos)]
        [wrap-pos wrap-tile])
      [next-pos next-tile])))

(defn do-move
  [board [r c :as pos] distance dir]
  ;(println "do-move" pos distance dir)
  (loop [pos pos n distance]
    (if (zero? n)
      pos
      (let [[next-pos next-tile] (get-next-tile board pos dir)]
        (if (= \# next-tile)
          pos
          (recur next-pos (dec n)))))))

(defn do-all-moves
  [board moves]
  (loop [[h & t] moves [r c :as pos] (start board) dir :right]
    (if (nil? h) 
      [r c dir]
      (condp = h
        \L (recur t pos (turn-left dir))
        \R (recur t pos (turn-right dir))
        (recur t (do-move board pos h dir) dir)))))

(defn password
  [[r c dir]]
  (+
    (* 1000 (inc r))
    (* 4 (inc c))
    (condp = dir
      :right 0
      :down 1
      :left 2
      :up 3)))

(let [input (parse-input "input.txt")
      board (parse-board input)
      moves (parse-moves input)]
  (println (password (do-all-moves board moves))))
