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

(defn between?
  [[r c][r1 c1][r2 c2]]
  (cond
    (= c1 c2) (and (= c c1)(>= r r1)(<= r r2))
    (= r1 r2) (and (= r r1)(>= c c1)(<= c c2))
    :else false))

(defn wrap-around
  [[r c :as pos] dir]
  ;(println "wrap-around" pos dir)
  (condp = dir
    :down
    (cond
      (between? pos [149 50][149 99]) [(+ c 100) 49 :left] ;; bottom
      (between? pos [49 100][49 149]) [(- c 50) 99  :left]  ;; rhs
      (between? pos [199  0][199 49]) [0 (+ c 100) :down] ;; back
      :else (throw (Exception. (str "Invalid edge " pos " " dir)))) 

    :right
    (cond
      (between? pos [0 149][49 149]) [(- 149 r) 99  :left]  ;; rhs
      (between? pos [50 99][99 99]) [49  (+ r 50) :up]      ;; front
      (between? pos [150 49][199 49]) [149 (- r 100) :up] ;; back
      (between? pos [100 99][149 99]) [(- 149 r) 149 :left] ;; bottom
      :else (throw (Exception. (str "Invalid edge " pos " " dir)))) 

    :up
    (cond
      (between? pos [0 100][0 149])   [199 (- c 100) :up]  ;; rhs
      (between? pos [100  0][100 49]) [(+  50 c) 50 :right] ;; lhs
      (between? pos [  0 50][ 0  99]) [(+ 100 c) 0 :right] ;; top
      :else (throw (Exception. (str "Invalid edge " pos " " dir)))) 

    :left
    (cond
      (between? pos [100  0][149 0])[(- 149 r) 50 :right] ;; lhs
      (between? pos [ 0 50][49 50]) [(- 149 r) 0 :right] ;; top
      (between? pos [50 50][99 50]) [100 (- r 50) :down] ;; front
      (between? pos [150 0][199 0]) [0 (- r 100) :down] ;; back
      :else (throw (Exception. (str "Invalid edge " pos " " dir)))))) 

(defn start [board] [0 (.indexOf (first board) \.)])

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
      (let [[r' c' dir'] (wrap-around pos dir)
            tile' (get-in board [r' c'])]
        [[r' c'] tile' dir'])
      [next-pos next-tile dir])))

(defn do-move
  [board [r c :as pos] distance dir]
  ;(println "do-move" pos distance dir)
  (loop [pos pos n distance dir dir]
    (if (zero? n)
      [pos dir]
      (let [[pos' tile' dir'] (get-next-tile board pos dir)]
        (if (= \# tile')
          [pos dir]
          (recur pos' (dec n) dir'))))))

(defn do-all-moves
  [board moves]
  (loop [[h & t] moves [r c :as pos] (start board) dir :right]
    (if (nil? h) 
      [r c dir]
      (condp = h
        \L (recur t pos (turn-left dir))
        \R (recur t pos (turn-right dir))
        (let [[pos' dir'] (do-move board pos h dir)]
          (recur t pos' dir'))))))

(defn password
  [[r c dir]]
  ;(println "r" r "c" c "dir" dir)
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
