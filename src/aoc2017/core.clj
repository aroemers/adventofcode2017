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

(def squares
  ((fn squares* [n]
     (cons (* n n) (lazy-seq (squares* (inc n)))))
   1))

(defn between
  [n coll]
  (reduce (fn [x y]
            (if (< x n (inc y))
              (reduced [x y])
              y))
          coll))

(defn spiral-memory
  [n]
  (let [[x y]  (between n (filter odd? squares))
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


;;; Day 4 - High-Entropy Passphrases

(defn no-doubles
  [phrases]
  (filter #(apply distinct? %) phrases))

(defn no-anagrams
  [phrases]
  (filter #(apply distinct? (map sort %)) phrases))

(defn high-entropy
  [phrases]
  (-> phrases no-doubles count))

(defn added-security
  [phrases]
  (-> phrases no-anagrams count))



;;; Day 5 - A Maze of Twisty Trampolines, All Alike

(defn jumps*
  [numbers update-fn]
  (let [array      (long-array numbers)
        last-index (dec (count array))]
    (reduce (fn [index jumps]
              (if (<= 0 index last-index)
                (let [jump (aget array index)]
                  (aset-long array index (update-fn jump))
                  (+ index jump))
                (reduced jumps)))
            0
            (range))))

(defn jumps-inc
  [numbers]
  (jumps* numbers inc))

(defn jumps-three
  [numbers]
  (jumps* numbers #(if (< % 3) (inc %) (dec %))))



;;; Day 6 - Memory Reallocation

(defn max-index
  [numbers]
  (reduce (fn [current i]
            (let [n (get numbers i)]
              (if (< (first current) n)
                [n i]
                current)))
          [-1 -1]
          (range 0 (count numbers))))

(defn reallocate
  [numbers]
  (let [[number index] (max-index numbers)]
    (loop [numbers (assoc numbers index 0)
           index   (mod (inc index) (count numbers))
           number  number]
      (if (< 0 number)
        (recur (update numbers index inc)
               (mod (inc index) (count numbers))
               (dec number))
        numbers))))

(defn reallocates
  [numbers]
  (loop [previous #{}
         current  numbers
         ordered  []]
    (if (get previous current)
      {:total-steps (count ordered)
       :cycle-steps (count (drop-while (partial not= current) ordered))}
      (recur (conj previous current) (reallocate current) (conj ordered current)))))
