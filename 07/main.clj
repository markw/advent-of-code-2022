(def input (.split (slurp "input.txt") "\n"))

(defn parse-line
  [s]
  (let [tokens (.split s " ")]
    (condp = (first tokens)
      "$" (vec (rest tokens))
      "dir" (vec tokens)
      [(Integer. (first tokens))(second tokens)])))

(defn cd
  [path s]
  (if (= s "..")
    (pop path)
    (conj path s)))

(defn add-file-to-fs
  [fs path size name]
  (assoc-in fs (conj path name) size))

(defn build-fs
  [input]
  (loop [input input fs {} path []]
    (if (empty? input)
      fs
      (let [[a b] (parse-line (first input))]
        (cond
          (number? a) (recur (rest input) (add-file-to-fs fs path a b) path)
          (= "cd" a)  (recur (rest input) fs (cd path b))
          :default (recur (rest input) fs path))))))

(defn calc-dir-size
  [name files]
  (reduce
    (fn [acc [k v]]
      (if (map? v)
        (+ acc (calc-dir-size k v))
        (+ acc v)))
    0
    files))

(defn calc-all-dir-sizes
  [fs]
  (reduce
    (fn [acc [k v]]
      (if (map? v)
        (concat 
          (conj acc (calc-dir-size k v))
          (calc-all-dir-sizes v))
        acc))
    []
    fs))

(let [all-dir-sizes (calc-all-dir-sizes (build-fs input))]
  (println "part 1" (reduce + (filter #(<  % 100000) all-dir-sizes)))
  (println "part 2" 
    (let [unused-space (- 70000000 (apply max all-dir-sizes))]
      (first (sort (filter #(< 30000000 (+ unused-space %)) all-dir-sizes))))))
