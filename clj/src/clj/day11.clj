(ns clj.day11
  (:require [clojure.math.combinatorics :as combo ]
            [clojure.set :as s]))

;elements
;:t 1
;:pl 2
;:s 3
;:pr 4
;:r 5
;:e 6
;:d 7
; positive is generator, negative is chip

(def visited (atom #{}))

(def ip1
  {:e 0
   :floors [#{6 -6 7 -7 1 -1 2 3}
   ;:floors [#{1 -1 2 3}
            #{-2 -3}
            #{4 -4 5 -5}
            #{}]})

(defn negate [n]
  (* -1 n))

(defn generator? [n]
  (> n 0))

(defn chip? [n]
  (not (generator? n)))

(defn hasMatchingGenerator? [chip generators]
  (contains? generators (negate chip)))

(def floorValid? 
  (memoize 
    (fn [f]
      (let [s (count f)]
        (cond
          (< s 2) true
          :else 
          (let [generators (filter generator? f)]
            (if (empty? generators)
              true
              (reduce (fn [agg chip]
                        (and agg (hasMatchingGenerator? chip (set generators)))
                        ) true (filter chip? f)))))))))

(defn positive? [n]
  (>= n 0))

(defn chipCount [f]
  (count (filter chip? f)))

(defn generatorCount [f]
  (count (filter generator? f)))

(def pairs 
  (memoize 
    (fn [pos]
      (hash [(:e pos)
             (map 
               (fn [flr]
                 [(chipCount flr) (generatorCount flr)]
                 ) (:floors pos))]))))

(defn positionNotVisited [pos]
  (not (contains? @visited (pairs pos))))

(defn positionValid [pos]
  (and
   (positionNotVisited pos)
   (every? floorValid? (:floors pos))))

(defn applyMove [from [f to] things]
  (let [newPos (assoc from :e to)
        floors (:floors newPos)
        fromFloor (-> (nth floors f)
                      (s/difference ,,, things))
        toFloor (-> (nth floors to)
                    (s/union ,,, things))]
    (assoc newPos :floors 
           (-> floors
               (assoc-in ,,, [f] fromFloor)
               (assoc-in ,,, [to] toFloor)))))

(def subset
  (memoize
    (fn [things]
      (set
        (map set 
             (let [t (seq things)]
               (concat (map (fn [thing] [thing]) t)
                       (combo/combinations things 2))))))))

(defn possiblePositions [pos]
  (let [e (:e pos)
        things (nth (:floors pos) e)
        paths (cond
                (= 0 e) [1]
                (= 1 e) [0 2]
                (= 2 e) [1 3]
                (= 3 e) [2]
                :else [])
        subs (subset things) ]
    (mapcat (fn [p]
              (->> subs
                   (map (fn [s] (applyMove pos [e p] (set s))))
                   (filter (fn [c] (positionValid c)))) 
              ) paths)))

(defn complete? [pos]
  (let [f (:floors pos)]
    (and 
      (= 3 (:e pos))
      (empty? (nth f 0))
      (empty? (nth f 1))
      (empty? (nth f 2)))))

(defn evaluatePos [[foundSolution nextLevel] pos]
  (if foundSolution
    [foundSolution nextLevel]
    (if (complete? pos)
      [true nextLevel]
      (do
        (swap! visited conj (pairs pos))
        (let [pp (possiblePositions pos)
              [_ up] (reduce (fn [[hashes positions] p] 
                           (let [ser (pairs p)]
                             (if (contains? hashes ser)
                               [hashes positions]
                               [(conj hashes ser) (conj positions p)]))) [#{} #{}] pp)
              n (into nextLevel up)]
          [foundSolution n]) ))))

(defn solution []
  (reset! visited #{})
  (loop [depth 0
         positions #{ip1} ]
    (prn (str "(" depth "," (count positions) "," (count @visited) ")"))
    ;can only really get this far in at the moment
    (if (> depth 100)
      depth
     (let 
      [[f n] (reduce evaluatePos [false #{}] positions)]
      (if f
        depth
        (recur (+ 1 depth) n))))))
