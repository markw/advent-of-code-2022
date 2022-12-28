(defn parse-line
  [s]
  (let [tokens (.split s ":")]
    [(first tokens) (vec (.split (.trim (second tokens )) " "))]))

(defn parse-input
  [file-name]
  (into {} (map parse-line (.split (slurp file-name) "\n"))))

(defn evaluate
  [left op right]
  (condp = op
    "+" (+ left right)
    "*" (* left right)
    "-" (- left right)
    "/" (/ left right)))

(defn resolve-expr
  [tree k]
  (if (number? k)
    k
    (let [expr (get tree k)]
      (if (= 3 (count expr))
        (evaluate 
          (resolve-expr tree (first expr))
          (second expr)
          (resolve-expr tree (last expr)))
        (Long. (first expr))))))

(defn parent
  [tree s]
  (first
    (filter 
      (fn [[k v]]
        (or (= s (first v))(= s (last v))))
      tree)))

(defn all-parents
  [tree s]
  (loop [s s acc []]
    (let [p (parent tree s)]
      (if (nil? p)
        acc
        (recur (first p) (conj acc p))))))

(defn invert
  [op]
  (condp = op
    "+" "-"
    "-" "+"
    "*" "/"
    "/" "*"))

(defn rewrite
  [tree [parent [left op right]] s]
  (let [term (if (= s right) left right)
        resolved (resolve-expr tree term)]
    (cond
      (= parent "root")
      [s [resolved]]

      (and (= s right)(= "/" op))
      [s [resolved "/" parent]]

      (and (= s right)(= "-" op))
      [s [resolved "-" parent]]

      :else
      [s [parent (invert op) resolved]])))

(defn rewrite-all
  [tree solve-for]
  (loop [[h & t] (all-parents tree solve-for)  solve-for solve-for acc []]
    (if (nil? h)
      (into {} acc)
      (let [new-expr (rewrite tree h solve-for)
            [parent' [left' op' right']] new-expr]
        (if (number? left')
          (recur t right' (conj acc new-expr))
          (recur t left' (conj acc new-expr)))))))
           
(let [input (parse-input "input.txt")]
  (time (println "part 1" (resolve-expr input "root")))
  (time (println "part 2" (resolve-expr (rewrite-all input "humn") "humn"))))
