(def INFINITY Long/MAX_VALUE)

(defn dijkstra
  [graph source]
  (loop [seen #{} distances {source 0}]
    (let [vertex-not-seen? (comp not #(contains? seen %1) first)
          priorities (sort-by second (filter vertex-not-seen? distances))
          vertex (ffirst priorities)]
      (if (nil? vertex) 
        distances
        (let [relaxed (mapv
                        (fn [[k v]] [k (min (+ v (vertex distances)) 
                                            (distances k INFINITY))])
                        (vertex graph))]
          (recur
            (conj seen vertex)
            (reduce #(apply assoc %1 %2) distances relaxed)))))))
  
  
(def graph
  {:s {:u 10 :x 5}
   :x {:u 3 :v 9 :y 2}
   :u {:v 1 :x 1}
   :y {:v 6 :s 7}
   :v {:y 4}})

(println (dijkstra graph :s))
