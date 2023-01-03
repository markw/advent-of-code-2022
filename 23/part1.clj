(require '[clojure.set :as sets])

(defn parse-input
  [file-name]
  (mapv
    #(mapv char %)
    (.split (slurp file-name) "\n")))

(defn find-elves
  [grid]
  (set
    (for [r (range (count grid))
          c (range (count (first grid)))
          :when (= \# (get-in grid [r c]))] 
      [r c])))

(defn rotate
  [[h & t :as a]]
  (cond
    (nil? h) a
    (nil? t) a
    :else (conj (vec t) h)))

(def NW [-1 -1])
(def N  [-1  0])
(def NE [-1  1])
(def E  [ 0  1])
(def SE [ 1  1])
(def S  [ 1  0])
(def SW [ 1 -1])
(def W  [ 0 -1])

(defn add
  [rc]
  (fn [rc'] (mapv + rc rc')))

(def rules [[N NE NW]
            [S SE SW]
            [W NW SW]
            [E NE SE]])

(def neighbor-rules (partition 2 (flatten rules)))

(defn has-no-neighbors?
  [elves elf]
  (not-any?
    (set (mapv (add elf) neighbor-rules))
    elves))

(defn propose-moves
  [elves rules]
  (let [proposed-moves
        (mapv
          (fn [e]
            (if (has-no-neighbors? elves e)
              [e e]
              (loop [[r & t] rules]
                (if (nil? r)
                  [e e]
                  (let [candidates (mapv (add e) r)]
                    (if (not-any? (set candidates) elves)
                      ;(do
                        ;(println e " -> " (first candidates))
                      [e (first candidates)]
                      (recur t)))))))
          elves)
        
        valid-dests 
        (set 
          (map first
            (filter
              (fn [[k v]] 
                (= v 1))
              (frequencies (map second proposed-moves)))))]
    
    (mapv
      (fn [[from to]]
        (if (contains? valid-dests to)
          to
          from))
      proposed-moves)))

(defn empty-tiles
  [elves]
  (let [[r0 r1] (apply (juxt min max) (map first elves))
        [c0 c1] (apply (juxt min max) (map second elves))]
    ;(println r0 r1 c0 c1)
    (- 
      (* 
        (inc (- r1 r0))
        (inc (- c1 c0)))
      (count elves))))
    
(time 
  (println
    (let [elves (find-elves (parse-input "input.txt"))]
      (loop [n 10 elves elves rules rules]
        (if (zero? n)
          (empty-tiles elves)
          (let [elves' (propose-moves elves rules)]
            (recur (dec n) elves' (rotate rules))))))))


          


