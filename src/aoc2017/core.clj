(ns aoc2017.core)

;;; Day 1 - Inverse Captcha

(defn inverse-captcha
  [input move]
  (let [moved (concat (drop move input) (take move input))]
    (first (reduce (fn [[sum moved] n]
                     [(cond-> sum (= n (first moved)) (+ n))
                      (rest moved)])
                   [0 moved]
                   input))))


;;; Day 2 - Corruption Checksum

(defn difference
  [input]
  (apply - (apply (juxt max min) input)))

(defn division
  [input]
  (first (for [a input b input
               :when (and (not= a b) (= (rem a b) 0))]
           (quot a b))))

(defn spreadsheet-checksum
  [input operation]
  (reduce + (map operation input)))
