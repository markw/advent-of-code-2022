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

(time 
  (println
    (let [elves (find-elves (parse-input "sample.txt"))]
      (loop [n 0 elves elves rules rules]
        (when (zero? (mod n 10))
          (println "n=" n))
        (let [elves' (propose-moves elves rules)]
          (if (= elves elves')
            (inc n)
            (recur (inc n) elves' (rotate rules))))))))
