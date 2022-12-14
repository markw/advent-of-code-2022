(defn parse-grid
  [file-name]
  (mapv #(mapv int %) (.split (slurp file-name) "\n")))

(defn find-start-and-end
  [grid]
  (into {}
    (filter identity
      (for [r (range (count grid))
            c (range (count (first grid)))]
        (case (char (get-in grid [r c]))
          \E {:end   [r c]}
          \S {:start [r c]}
          nil)))))

(defn elevation
  [e]
  (case e
    83 (int \a)
    69 (int \z)
    e)) 

(defn can-move-to?
  [grid from-pos to-pos]
  (let [from-elevation (get-in grid from-pos)
        to-elevation (get-in grid to-pos)]
    (and (some? from-elevation)
         (let [delta (- (elevation to-elevation) (elevation from-elevation))]
          (<= delta 1)))))

(defn neighbors
  [grid pos]
  (filter 
    #(can-move-to? grid % pos)
    (for [r [pos] 
          c [[-1 0][1 0][0 1][0 -1]]] 
      (mapv + r c))))

(defn solve
  [part grid {:keys [start end]}]
  (loop [step 1 seen #{end} current #{end}]
    (let [adjacent (set (mapcat #(neighbors grid %) current))]
      (if (or (and (= 1 part)(contains? adjacent start))
              (and (= 2 part)(some #(= (int \a)(get-in grid %)) adjacent)))
        step
        (recur (inc step)
               (apply conj seen adjacent)
               (set (filter #(not (contains? seen %)) adjacent)))))))

(let [grid (parse-grid "input.txt")
      init (find-start-and-end grid)]
  (println "part 1" (solve 1 grid init))
  (println "part 2" (solve 2 grid init)))
