(def E 2.718281828459)
(defn square [n] (Math/pow n 2))

(defn ! [n]
  (loop [cur n acc 1]
    (if (zero? cur) acc
        (recur (dec cur) (*' cur acc)))))

(defn combinations [n r] (/ (! n) (* (! (- n r)) (! r))))
(defn binomial-distribution [trial-count prob-success success-count]
  (* (combinations trial-count success-count)
     (Math/pow prob-success success-count)
     (Math/pow (- 1 prob-success) (- trial-count success-count))))

(defn expectation [trial-count prob-success]
  (Math/round (reduce + (map #(* % (binomial-distribution trial-count prob-success %)) (range trial-count)))))

(defn variance [trial-count prob-success]
  (- (reduce + (map #(* (square %) (binomial-distribution trial-count prob-success %)) (range trial-count)))
     (square (expectation trial-count prob-success))))

(defn standard-deviation [trial-count prob-success]
  (Math/sqrt (variance trial-count prob-success)))

(def coin-flip-pmf [0.5 0.5])
(def die-roll-pmf [0.1667 0.1667 0.1667 0.1667 0.1667 0.1667])

; 0.7
(def biased-die-roll-pmf [0.3 0.14 0.14 0.14 0.14 0.14])

; sum (p(x) * log( p(x) / q(x))
; Both p and q must have the same exact event space
(defn log2 [n] (/ (Math/log n) (Math/log 2)))
(defn map-both [p q fn]
  (loop [lst1 p lst2 q out-lst []]
    (if (empty? lst1) out-lst (recur (rest lst1) (rest lst2) (conj out-lst (fn (first lst1) (first lst2)))))))
(defn prob-fn [x y] (* x (log2 (/ x y))))
(defn relative-entropy [p q]
  (reduce + (map-both p q prob-fn)))
