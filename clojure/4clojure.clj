; https://www.4clojure.com/problem/30
(defn compress-seq [lst]
  (loop [last-token nil bld-lst [] l lst]
    (let [next-token (first l) rest-l (rest l)]
      (cond (empty? l) bld-lst (= last-token next-token) (recur last-token bld-lst rest-l) :else (recur next-token (conj bld-lst next-token) rest-l)))))

; https://www.4clojure.com/problem/56
(defn find-distinct-items [lst] (seq (set lst)))

; https://www.4clojure.com/problem/31
(defn partition-at [lst index]
  (loop [left-lst [] right-lst lst idx index]
    (let [next-tkn (first right-lst) remaining (rest right-lst)]
      (if (= idx 0) [left-lst right-lst]
        (recur (conj left-lst next-tkn) remaining (dec idx))))))
(defn partition-sequence [size lst]
  (loop [part-lst [] l lst]
    (let [[next-grp next-lst] (partition-at l size)]
      (if (< (count l) size) part-lst
        (recur (conj part-lst next-grp) next-lst)))))

; Rotate Sequence
; https://www.4clojure.com/problem/44
(defn rotate-seq [cnt s]
  (if (>= cnt 0) (apply concat (reverse (partition-at s cnt)))
    (recur (+ (count s) cnt) s)))

; Anagrams
; http://www.4clojure.com/problem/77
(defn sort-str [s] (apply str (sort s)))
(defn anagram-groups [lst]
  (filter #(> (count %) 1) (vals (group-by #(sort-str %) lst))))

(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
   iter-comb
   (fn iter-comb [c j]
     (if (> j n) nil
         (let [c (assoc c j (dec (c j)))]
     (if (< (c j) j) [c (inc j)]
         (loop [c c, j j]
           (if (= j 1) [c j]
         (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
   step
   (fn step [c j]
     (cons (rseq (subvec c 1 (inc n)))
     (lazy-seq (let [next-step (iter-comb c j)]
           (when next-step (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

(defn combinations
  "All the unique ways of taking n different elements from items"
  [items n]      
  (let [v-items (vec (reverse items))]
    (if (zero? n) (list ())
  (let [cnt (count items)]
    (cond (> n cnt) nil
    (= n cnt) (list (seq items))
    :else
    (map #(map v-items %) (index-combinations n cnt)))))))

(def divides #{[8 4] [9 3] [4 2] [27 9]})
(defn parent-of-relation [relation]
  (letfn [(grab-nth [lst n] (map #(nth % n) lst))]
    (let [x-values (grab-nth relation 0) y-values (grab-nth relation 1)]
      (clojure.set/difference (set x-values) (set y-values)))))
(defn child-of-relation [relation]
  (letfn [(grab-nth [lst n] (map #(nth % n) lst))]
    (let [x-values (grab-nth relation 0) y-values (grab-nth relation 1)]
      (clojure.set/difference (set y-values) (set x-values)))))

; Use a vector as a stack in Clojure
(defn inside-bracket [code]
  (loop [cd code closers []]
    (let [next-ch (str (first cd)) rem-code (rest cd) closer (peek closers)]
      (cond
        (= next-ch "") (empty? closers)
        (= next-ch "{") (recur rem-code (conj closers "}"))
        (= next-ch "(") (recur rem-code (conj closers ")"))
        (= next-ch "[") (recur rem-code (conj closers "]"))
        (= next-ch closer) (recur rem-code (pop closers))
        :else false))))
(defn only-brackets [s] (apply str (filter #(.contains "(){}[]" (str %)) (seq s))))
(defn balance-brackets [code]
  (let [only-bks (only-brackets code)]
    (inside-bracket only-bks)))

; Longest Increasing Sub-Seq
(defn longest-increasing-sub [lst]
  (let [slopes (filter #(> % -1) (map #(if (> (- %1 %2) 0) (inc %3) -1) lst (rest lst) (range (count lst))))
        full-slopes (conj (vec (conj slopes 0)) (count lst))
        ranges (filter #(> (count %) 1) (map #(subvec lst %1 %2) full-slopes (rest full-slopes)))]
    (first (sort-by #(- (count %)) ranges))))

; For Science!
; (maze-traversible? ["#######" "#     #" "#  #  #" "#M # C#" "#######"])
(defn find-mouse [maze] (filter (fn [[row col]] (> col -1)) (map-indexed #(identity [%1 (.indexOf %2 "M")]) maze)))
(defn get-row-col [mtx row col] (.charAt (nth mtx row) col))
(defn mark-maze [maze row col]
  (let [maze-row (nth maze row)]
    (into [] (concat (subvec maze 0 row)
            [(str (subs maze-row 0 col) "X" (subs maze-row (+ col 1)))]
            (subvec maze (+ row 1))))
))
(defn maze-traversible? [maze [row col]]
  (if (and (>= row 0) (>= col 0) (< row (count maze)) (< col (count (first maze))))
    (let [cell (get-row-col maze row col)]
      (cond
        (= cell \C) true
        (or (= cell \space) (= cell \M)) (or
          (maze-traversible? (mark-maze maze row col) [(+ row 1) col])
          (maze-traversible? (mark-maze maze row col) [(- row 1) col])
          (maze-traversible? (mark-maze maze row col) [row (+ col 1)])
          (maze-traversible? (mark-maze maze row col) [row (- col 1)])
        )
        :else false
      ))))
(def badmaze ["########"
              "#M  #  #"
              "#   #  #"
              "# # #  #"
              "#   #  #"
              "#  #   #"
              "#  # # #"
              "#  #   #"
              "#  #  C#"
              "########"])
(def badmaze2 ["M     "
              "      "
              "      "
              "      "
              "    ##"
              "    #C"])
(def goodmaze ["C######"
              " #     "
              " #   # "
              " #   #M"
              "     # "])
(def goodmaze2 ["C# # # #"
              "        "
              "# # # # "
              "        "
              " # # # #"
              "        "
              "# # # #M"])

;(maze-traversible? maze (find-mouse maze))
;(defn cheese-found? [curr-row curr-col maze]
  ;(if ()

; Palindromic Numbers
; (= (take 26 (__ 0))
;    [0 1 2 3 4 5 6 7 8 9 
;     11 22 33 44 55 66 77 88 99 
;     101 111 121 131 141 151 161])

