(ns aoc2017.core
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

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


;;; Day 7

(defn parse-line
  [line]
  (let [[_ node weight _ children] (re-matches #"(.+) \((\d+)\)( -\> (.+))?" line)]
    {:node     node
     :weight   (Long/parseLong weight)
     :children (when children (set (re-seq #"\w+" children)))}))

(defn parse-lines
  [lines]
  (map parse-line lines))

(defn find-root
  [parsed]
  (let [children (set (mapcat :children parsed))
        nodes    (set (map :node parsed))]
    (first (set/difference nodes children))))

(defn make-graph
  [parsed]
  (let [root    (find-root parsed)
        grouped (group-by :node parsed)
        fill-children (fn fill-children [node]
                        (assoc node :children (map fill-children (map first (vals (select-keys grouped (:children node)))))))]
    (fill-children (first (get grouped root)))))

(defn weigher
  [node]
  (if (and (map? node) (:weight node))
    (let [child-weights (map :sum (:children node))]
      (merge node {:sum       (reduce + (:weight node) child-weights)
                   :balanced? (if (seq child-weights)
                                (apply = child-weights)
                                true)}))
    node))

(defn find-unbalance
  [parsed]
  (let [found  (promise)
        finder (fn [node]
                 (when (and (map? node) (not (:balanced? node)))
                   (deliver found (map #(dissoc % :children) (:children node))))
                 node)]
    (->> parsed make-graph (walk/postwalk weigher) (walk/postwalk finder))
    (let [nodes        @found
          grouped      (group-by :sum nodes)
          group-1      (val (first grouped))
          group-2      (val (second grouped))
          group-1-node (first group-1)
          group-2-node (first group-2)]
      (if (= (count group-1) 1)
        (- (:weight group-1-node)
           (- (:sum group-1-node)
              (:sum group-2-node)))
        (- (:weight group-2-node)
           (- (:sum group-2-node)
              (:sum group-1-node)))))))
