(ns aoc2017.core)

;;; Day 1 - Inverse Captcha

(defn rotate
  [n coll]
  (concat (drop n coll) (take n coll)))

(defn keep*
  [f coll & colls]
  (apply sequence (comp (map f) (filter some?)) coll colls))

(defn sum
  [coll]
  (reduce + coll))

(defn same
  [x & ys]
  (when (every? (partial = x) ys)
    x))

(defn inverse-captcha
  [coll move]
  (->> coll (rotate move) (keep* same coll) sum))


;;; Day 2 - Corruption Checksum

(defn difference
  [coll]
  (apply - (apply (juxt max min) coll)))

(defn division
  [coll]
  (first (for [a coll b coll
               :when (and (not= a b) (= (rem a b) 0))]
           (quot a b))))

(defn spreadsheet-checksum
  [colls operation]
  (->> colls (map operation) sum))


;;; Day 3 - Spiral Memory

(defn squares
  []
  (letfn [(squares* [n]
            (cons (* n n) (lazy-seq (squares* (inc n)))))]
    (squares* 1)))

(defn between
  [n coll]
  (reduce (fn [x y]
            (if (< x n (inc y))
              (reduced [x y])
              y))
          coll))

(defn spiral-memory
  [n]
  (let [[x y]  (between n (filter odd? (squares)))
        length (long (Math/sqrt y))
        ring   (quot length 2)
        pos    (rem (- n x) (dec length))
        diff   (Math/abs (- pos ring))]
    (+ ring diff)))

(defn lookups
  []
  (letfn [(lookups* [n maximus minus times]
            (condp = n
              0       (cons [1 (inc minus)]
                            (lazy-seq (if (< 0 times)
                                        (lookups* maximus maximus (+ minus 2) (dec times))
                                        (lookups* (inc maximus) (inc maximus) (+ minus 2) 1))))
              1       (cons [1 minus (inc minus)]
                            (lazy-seq (lookups* 0 maximus minus times)))
              maximus (cons [1 2 minus (dec minus)]
                            (lazy-seq (lookups* (dec n) maximus minus times)))
              (cons [1 minus (inc minus) (dec minus)]
                    (lazy-seq (lookups* (dec n) maximus minus times)))))]
    (lookups* 1 1 1 2)))


(defn stress-test
  [n]
  (loop [cell    14
         vals    [1 1 2 4 5 10 11 23 25 26 54 57 59]
         lookups (drop 12 (lookups))]
    (let [val (reduce (fn [sum minus]
                        (+ sum (nth vals (dec (- cell minus)))))
                      0
                      (first lookups))]
      (if (< val n)
        (recur (inc cell)
               (conj vals val)
               (rest lookups))
        vals))))
