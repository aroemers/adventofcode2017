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
