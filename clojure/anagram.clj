(def words ('bad' 'coffee' 'loco' 'mary' 'army' 'cool'))

(defn anagram-groups [words] (group-by sort words))
(defn get-anagrams [lst] (filter (fn [l] (> (count l) 1)) (vals (anagram-groups lst))))

; "1010" => 10, "11" => 3
(defn binStrToInt [binStr] (reverse binStr))
