; http://en.wikipedia.org/wiki/Maximum_subarray_problem
(defn max-subarr [lst]
  (loop [l (rest lst) nxt (first lst) max-ending nxt max-thus nxt]
    (if (empty? l) max-thus
      (recur (rest l) (first l) (max nxt (+ max-ending nxt)) (max max-thus (max nxt (+ max-ending nxt)))))))
