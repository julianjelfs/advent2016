(ns clj.day11
  (:require [clojure.math.combinatorics :as combo ]
            [clojure.set :as s]))

; (def initialPosition
;   {:e 0
;    :floors [#{ {:m :h} {:m :l}} 
;             #{ {:g :h} }
;             #{ {:g :l} }
;             #{} ]})
;

;elements
;:t 1
;:pl 2
;:s 3
;:pr 4
;:r 5
;:e 6
;:d 7
; positive is generator, negative is chip


(def ip1 
  {:e 0
   :floors [#{1 -1 2 3}
            #{-2 -3}
            #{4 -4 5 -5}
            #{}]})

(defn negate [n]
  (* -1 n))

(defn generator? [n]
  (> n 0))

(defn chip? [n]
  (not (generator? n)))

(def ip2
  {:e 0
   :floors [#{ {:g :e} {:m :e} {:g :d} {:m :d} {:g :t} {:m :t} {:g :pl} {:g :s} } 
            #{ {:m :pl} {:m :s} }
            #{ {:g :pr} {:m :pr} {:g :r} {:m :r} }
            #{} ]})


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
      (let [f (:floors pos)]
        [(:e pos)
               (map (fn [flr]
                      [(chipCount flr) (generatorCount flr)]
                      ) f)]))))

(defn positionNotVisited [visited pos]
  (not (contains? visited (pairs pos))))

(defn positionValid [visited pos]
  (and
    (positionNotVisited visited pos)
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

(defn possiblePositions [visited pos]
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
                   ;something like this - but we need to reorganise a bit
                   ;(reduce (fn [v p]
                   ;          (if (positionNotVisited v p)
                   ;            v
                   ;            (conj v (pairs p)))) visited)
                   (filter (fn [c] (positionValid visited c)))) 
              ) paths)))

(defn complete? [pos]
  (let [f (:floors pos)]
    (and 
      (= 3 (:e pos))
      (empty? (nth f 0))
      (empty? (nth f 1))
      (empty? (nth f 2)))))

(defn evaluatePos [[foundSolution visited nextLevel] pos]
  (if foundSolution
    [foundSolution visited nextLevel]
    (if (complete? pos)
      [true visited nextLevel]
      (let [v (conj visited (pairs pos))
            n (into nextLevel 
                    (possiblePositions v pos))]
        [foundSolution v n]))))

(defn solution []
  (loop [depth 0
         positions #{ip1} 
         visited #{}]
    (prn (str "(" depth "," (count positions) "," (count visited) ")"))
    ;can only really get this far in at the moment
    (if (> depth 100)
      depth
     (let 
      [[f v n] (reduce evaluatePos [false visited #{}] positions)]
      (if f
        depth
        (recur (+ 1 depth) n v))))))
