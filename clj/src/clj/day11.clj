(ns clj.day11
  (:require [clojure.math.combinatorics :as combo ]
            [clojure.set :as s]))

; (def initialPosition
;   {:e 0
;    :floors [#{ {:m :h} {:m :l}} 
;             #{ {:g :h} }
;             #{ {:g :l} }
;             #{} ]})

(def initialPosition
  {:e 0
   :floors [#{ {:g :t} {:m :t} {:g :pl} {:g :s} } 
            #{ {:m :pl} {:m :s} }
            #{ {:g :pr} {:m :pr} {:g :r} {:m :r} }
            #{} ]})

; (def elements [:h :l])

(def elements [:t :pl :s :pr :r])

(defn matchingGenerators [generators chip]
  (filter (fn [g] (= (:g g) (:m chip))) generators))

(def floorValid? 
  (memoize 
    (fn [f]
      (let [s (count f)]
        (cond
          (= 0 s) true
          (= 1 s) true
          :else 
          (let [[chips generators] (split-with :m f)
                chipsWithoutGen (filter (fn [c]
                                          (empty? (matchingGenerators generators c))) chips)]
            (if (empty? chipsWithoutGen)
              true
              (empty? generators))))))))


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
      [(:e pos)
       (->> elements
            (mapcat (fn [e]
                      (let [chipFloors (floors pos e :m)
                            genFloors (floors pos e :g)
                            merged (map (fn [c g] [c g] ) chipFloors genFloors)]
                        (sort merged)))))])))


(def positionNotVisited 
  (memoize 
    (fn [visited pos]
      (not (contains? visited (pairs pos))))))

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
      (->> (combo/subsets (seq things))
           (filter (fn [s] 
                     (and 
                       (< (count s) 3) 
                       (> (count s) 0))))))))

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

(def complete? 
  (memoize
    (fn [pos]
      (let [f (:floors pos)]
        (and 
          (empty? (nth f 0))
          (empty? (nth f 1))
          (empty? (nth f 2)))))))

(defn evaluatePos [[foundSolution visited nextLevel] pos]
  (if foundSolution
    [foundSolution visited nextLevel]
    (if (complete? pos)
      [true visited nextLevel]
      (let [v (conj visited (pairs pos))
            n (into nextLevel (possiblePositions v pos))]
        [foundSolution v n]))))

(defn solution []
  (loop [depth 0
         positions #{initialPosition} 
         visited #{}]
    (prn depth)
    (prn (count positions))
    (prn (count visited))
    ;can only really get this far in at the moment
    (if (> depth 4)
      depth
     (let 
      [[f v n] (reduce evaluatePos [false visited []] positions)]
      (if f
        depth
        (recur (+ 1 depth) n v))))))
