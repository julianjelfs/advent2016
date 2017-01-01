(ns clj.day11
  (:require [clojure.math.combinatorics :as combo ]
            [clojure.set :as s]))

; (def initialPosition
;   {:e 0
;    :floors [#{ {:m :h} {:m :l}} 
;             #{ {:g :h} }
;             #{ {:g :l} }
;             #{} ]})

(def completeSet 
  (reduce s/union (:floors ip1)))

(def ip1
  {:e 0
   :floors [#{ {:g :t} {:m :t} {:g :pl} {:g :s} } 
            #{ {:m :pl} {:m :s} }
            #{ {:g :pr} {:m :pr} {:g :r} {:m :r} }
            #{} ]})

(def ip2
  {:e 0
   :floors [#{ {:g :e} {:m :e} {:g :d} {:m :d} {:g :t} {:m :t} {:g :pl} {:g :s} } 
            #{ {:m :pl} {:m :s} }
            #{ {:g :pr} {:m :pr} {:g :r} {:m :r} }
            #{} ]})

; (def elements [:h :l])

;(def elements [:e :d :t :pl :s :pr :r])
(def elements [:t :pl :s :pr :r])

(defn hasMatchingGenerator? [chip generators]
  (contains? generators {:g (:m chip)}))

(def floorValid? 
  (memoize 
    (fn [f]
      (let [s (count f)]
        (cond
          (< s 2) true
          :else 
          (let [generators (filter :g f)]
            (if (empty? generators)
              true
              (reduce (fn [agg chip]
                        (and agg (hasMatchingGenerator? chip (set generators)))
                        ) true (filter :m f)))))))))

(defn positive? [n]
  (>= n 0))

(defn floors [pos e t] 
  (->> 
    (map-indexed 
      (fn [i f]
        (if (contains? f {t e})
          i
          -1)) (:floors pos))
    (filter positive?)))

(def pairs 
  (memoize 
    (fn [pos]
      (let [e (:e pos)
            f (:floors pos)]
        [(:e pos)
         (map #(count (filter :g %1)) f)
         (map #(count (filter :m %1)) f)]))))


(defn positionNotVisited [visited pos]
  (not (contains? visited (pairs pos))))

(def positionValid
  (memoize 
       (fn [visited pos]
         (and
           (positionNotVisited visited pos)
           (every? floorValid? (:floors pos))))))


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
                       (for [a t
                             b t
                             :when (not (= a b))]
                         [a b]))))))))

(def subset2
  (memoize 
    (fn [things]
      (->> (combo/subsets (seq things))
           (filter (fn [s] 
                     (and 
                       (< (count s) 3) 
                       (> (count s) 0))))
           (map set)))))

(defn possiblePositions [visited pos]
  (let [e (:e pos)
        things (nth (:floors pos) e)
        paths (cond
                (= 0 e) [[0 1]]
                (= 1 e) [[1 0] [1 2]]
                (= 2 e) [[2 1] [2 3]]
                (= 3 e) [[3 2]]
                :else [])
        subs (subset things) ]
    (mapcat (fn [p]
              (->> subs
                   (map (fn [s] (applyMove pos p (set s))))
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
