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

(defn legal-step?
  [grid from to]
  (if-let [from-elevation (elevation (get-in grid from))]
    (let  [to-elevation (elevation (get-in grid to))
           delta (- to-elevation from-elevation)]
      (<= delta 1))))

(defn neighbors
  [grid pos]
  (filter 
    #(legal-step? grid % pos)
    (for [r [pos] 
          c [[-1 0][1 0][0 1][0 -1]]] 
      (mapv + r c))))

(defn solve
  [part grid {:keys [start end]}]
  (loop [step 1 seen #{end} current #{end}]
    (let [adjacent (mapcat #(neighbors grid %) current)
          not-seen (set ((group-by #(contains? seen %) adjacent) false))]
      (if (or (and (= 1 part)(contains? not-seen start))
              (and (= 2 part)(some #(= (int \a)(get-in grid %)) not-seen)))
        step
        (recur (inc step)
               (apply conj seen adjacent)
               not-seen)))))

(let [grid (parse-grid "input.txt")
      init (find-start-and-end grid)]
  (println "part 1" (solve 1 grid init))
  (println "part 2" (solve 2 grid init)))
