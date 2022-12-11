(def sample-monkeys [
                     {:items [79 98]
                      :op #(* 19 %)
                      :test #(zero? (mod % 23))
                      :true 2 :false 3
                      :inspected 0}
                     {:items [54 65 75 74]
                      :op #(+ 6 %)
                      :test #(zero? (mod % 19))
                      :true 2 :false 0
                      :inspected 0}
                     {:items [79 60 97]
                      :op #(* % %)
                      :test #(zero? (mod % 13))
                      :true 1 :false 3
                      :inspected 0}
                     {:items [74]
                      :op #(+ 3 %)
                      :test #(zero? (mod % 17))
                      :true 0 :false 1
                      :inspected 0}])

(def all-monkeys [
                    {:items [84 72 58 51]
                     :op #(* 3 %)
                     :test #(zero? (mod % 13))
                     :true 1 :false 7
                     :inspected 0}
                    {:items [88 58 58]
                     :op #(+ 8 %)
                     :test #(zero? (mod % 2))
                     :true 7 :false 5
                     :inspected 0}
                    {:items [93 82 71 77 83 53 71 89]
                     :op #(* % %)
                     :test #(zero? (mod % 7))
                     :true 3 :false 4
                     :inspected 0}
                    {:items [81 68 65 81 73 77 96]
                     :op #(+ % 2)
                     :test #(zero? (mod % 17))
                     :true 4 :false 6
                     :inspected 0}
                    {:items [75 80 50 73 88]
                     :op #(+ 3 %) 
                     :test #(zero? (mod % 5))
                     :true 6 :false 0
                     :inspected 0}
                    {:items [59 72 99 87 91 81] 
                     :op #(* % 17)
                     :test #(zero? (mod % 11)) 
                     :true 2 :false 3
                     :inspected 0}
                    {:items [86 69]
                     :op #(+ % 6) 
                     :test #(zero? (mod % 3))
                     :true 1 :false 0
                     :inspected 0}
                    {:items [91]
                     :op #(+ % 1)
                     :test #(zero? (mod % 19)) 
                     :true 2 :false 5
                     :inspected 0}])

(defn add-item
  [monkey item]
  (assoc monkey :items (conj (:items monkey) item)))

;; needed a big hint from Reddit for this
(def lcm (* 13 2 7 17 5 11 3 19))

(defn next-monkeys
  [part monkey]
  (let [{items :items test :test op :op t :true f :false}  monkey
        new-items-fn (condp = part
                       1 #(int (/ (op %) 3))
                       2 #(mod (op %) lcm)) ;; <-- big hint from Reddit for this
        new-items (map new-items-fn items)]
    (map
      #(if (test %) [% t] [% f])
      new-items)))

(defn inspect
  [part monkeys n]
  (let [monkey (get monkeys n)
        {items :items inspected :inspected} monkey
        to-throw (next-monkeys part monkey)
        new-inspected (+ (count items) inspected)]
    (reduce
      (fn [acc [item x]]
        (let [target (get acc x)]
          (assoc acc x (add-item target item))))
      (assoc monkeys n (merge monkey {:items [] :inspected new-inspected}))
      to-throw)))
  
(defn play-round
  [part monkeys n]
  (reduce 
    (partial inspect part)
    monkeys
    (range (count monkeys))))

(defn solve
  [part rounds]
  (println "part" part
    (reduce * 
      (take 2
        (reverse
          (sort
            (map :inspected
              (reduce
                (partial play-round part)
                all-monkeys
                (range rounds)))))))))

(solve 1 20)
(solve 2 10000)
