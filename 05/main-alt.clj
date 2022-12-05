;;     [V] [G]             [H]        
;; [Z] [H] [Z]         [T] [S]        
;; [P] [D] [F]         [B] [V] [Q]    
;; [B] [M] [V] [N]     [F] [D] [N]    
;; [Q] [Q] [D] [F]     [Z] [Z] [P] [M]
;; [M] [Z] [R] [D] [Q] [V] [T] [F] [R]
;; [D] [L] [H] [G] [F] [Q] [M] [G] [W]
;; [N] [C] [Q] [H] [N] [D] [Q] [M] [B]
;;  1   2   3   4   5   6   7   8   9 
;;

(def stacks ['(\Z \P \B \Q \M \D \N)
             '(\V \H \D \M \Q \Z \L \C)
             '(\G \Z \F \V \D \R \H \Q)
             '(\N \F \D \G \H)
             '(\Q \F \N)
             '(\T \B \F \Z \V \Q \D)
             '(\H \S \V \D \Z \T \M \Q)
             '(\Q \N \P \F \G \M)
             '(\M \R \W \B)])

(defn parse-move
  [s]
  (let [tokens (.split s " ")]
    (map
      #(%1 (Integer. (get tokens %2)))
      [identity dec dec]
      [1 3 5])))

(def moves
  (map 
    parse-move
    (filter
      #(.startsWith % "move")
      (.split (slurp "input.txt") "\n"))))

(defn do-move
  [stacks [n from to] items]
  (reduce
    (fn [acc [k v]] (assoc acc k v))
    stacks
    [[from (drop n (get stacks from))]
     [to (reduce #(cons %2 %1) (get stacks to) items)]]))

(defn do-move-part-1
  [stacks [n from to]]
  (do-move stacks [n from to] (take n (get stacks from))))

(defn do-move-part-2
  [stacks [n from to]]
  (do-move stacks [n from to] (reverse (take n (get stacks from)))))

(defn get-all-firsts
  [stacks]
  (reduce str
    (map first stacks)))

(println "part 1"
  (get-all-firsts 
    (reduce
      (fn [acc move] (do-move-part-1 acc move))
      stacks
      moves)))

(println "part 2"
  (get-all-firsts 
    (reduce
      (fn [acc move] (do-move-part-2 acc move))
      stacks
      moves)))
