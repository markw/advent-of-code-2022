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
  (let [expr (get tree k)]
    (if (= 3 (count expr))
      (evaluate 
        (resolve-expr tree (first expr))
        (second expr)
        (resolve-expr tree (last expr)))
      (Integer. (first expr)))))
           
(let [input (parse-input "input.txt")]
  (println "part 1" (resolve-expr input "root")))
