(ns aoc2017.core
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.string :as str]))

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


;;; Day 7 - Recursive Circus

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
  (let [root          (find-root parsed)
        grouped       (group-by :node parsed)
        fill-children (fn fill-children [node]
                        (let [child-nodes (->> node :children (select-keys grouped) vals (map first))]
                          (assoc node :children (map fill-children child-nodes))))]
    (fill-children (first (get grouped root)))))

(defn weight-walker
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
  (let [found       (promise)
        find-walker (fn [node]
                      (when (and (map? node) (not (:balanced? node)))
                        (deliver found (map #(dissoc % :children) (:children node))))
                      node)]
    (->> parsed make-graph (walk/postwalk weight-walker) (walk/postwalk find-walker))
    (let [groups    (->> found deref (group-by :sum) vals (sort-by count))
          odd-node  (ffirst groups)
          good-node (first (second groups))]
      (- (:weight odd-node)
         (- (:sum odd-node)
            (:sum good-node))))))


;;; Day 8 - I Heard You Like Registers

(defn instruction
  [registers line]
  (let [[_ register operation value test-register test-operation test-value]
        (re-matches #"(\w+) (\w+) (-?\d+) if (\w+) (\S+) (-?\d+)" line)]
    (if ((eval (symbol (case test-operation "==" "=" "!=" "not=" test-operation)))
         (registers test-register 0)
         (Long/parseLong test-value))
      (let [new-value ((case operation "inc" + "dec" -) (registers register 0) (Long/parseLong value))]
        (assoc registers
               register new-value
               :max (max (:max registers) new-value)))
      registers)))

(defn instructions
  [input]
  (reduce instruction {:max Long/MIN_VALUE} (str/split-lines input)))

(defn max-register
  [input]
  (-> input instructions (dissoc :max) vals (->> (apply max))))

(defn max-alltime
  [input]
  (->> input instructions :max))


;;; Day 9 - Stream Processing

(defn groups
  [input]
  (loop [groups  []
         depth   1
         input   input
         ignore  false
         garbage false]
    (if-let [c (first input)]
      (cond ignore
            (recur groups depth (rest input) false garbage)

            (= c \!)
            (recur groups depth (rest input) true garbage)

            garbage
            (if (= c \>)
              (recur groups depth (rest input) false false)
              (recur groups depth (rest input) false true))

            (= c \<)
            (recur groups depth (rest input) false true)

            (= c \{)
            (recur (conj groups depth) (inc depth) (rest input) false false)

            (= c \})
            (recur groups (dec depth) (rest input) false false)

            :otherwise
            (recur groups depth (rest input) false false))
      groups)))

(defn groups-score
  [input]
  (-> input groups sum))

(defn garbage-count
  [input]
  (->> (str/replace input #"!." "")
       (re-seq #"\<[^\>]*")
       (map (comp dec count))
       (sum)))


;;; Day 10 - Knot Hash

(defn knot-hash
  [size rounds lengths]
  (let [numbers (int-array (range size))]
    (reduce (fn [[index skip] length]
              (let [reversed (reverse (take length (drop index (cycle numbers))))]
                (doseq [i (range length)]
                  (aset-int numbers (mod (+ index i) size) (nth reversed i))))
              [(mod (+ index length skip) size) (inc skip)])
            [0 0]
            (take (* (count lengths) rounds) (cycle lengths)))
    (seq numbers)))

(defn knot-hash-score
  [size lengths]
  (let [[x y] (knot-hash size 1 lengths)]
    (* x y)))

(defn knot-hash-hex
  [input]
  (as-> input X
    (map byte X)
    (concat X [17 31 73 47 23])
    (knot-hash 256 64 X)
    (partition 16 X)
    (map #(reduce bit-xor %) X)
    (map #(format "%02x" %) X)
    (apply str X)))



;;; Day 11 - Hex Ed

(def directions
  ["n" "ne" "se" "s" "sw" "nw"])

(def optimizations
  (reduce (fn [acc i]
            (conj acc
                  [(directions i) (directions (mod (+ i 3) 6)) ""]
                  [(directions i) (directions (mod (+ i 2) 6)) (directions (mod (+ i 1) 6))]
                  [(directions i) (directions (mod (+ i 4) 6)) (directions (mod (+ i 5) 6))]))
          []
          (range 6)))

(defn index-of
  [coll elem]
  (first (keep-indexed (fn [i e] (when (= e elem) i)) coll)))

(defn optimize*
  [directions]
  (reduce (fn [directions [dir1 dir2 opti]]
            (let [index1 (index-of directions dir1)
                  index2 (index-of directions dir2)]
              (if (and index1 index2)
                (assoc directions index1 opti index2 "")
                directions)))
          directions
          optimizations))

(defn optimize
  [directions]
  (loop [directions directions]
    (let [optimized (optimize* directions)]
      (if (= directions optimized)
        (vec (remove str/blank? directions))
        (recur optimized)))))

(defn longest-distance
  [directions]
  (->> directions
       (reductions #(optimize (conj %1 %2)) [])
       (map count)
       (apply max)))


;;; Day 12 - Digital Plumber

(defn pipe-line
  [line]
  (let [[_ node nodes] (re-matches #"^(\d+) \<-\> (.*)$" line)]
    {node (set (re-seq #"\d+" nodes))}))

(defn pipe-lines
  [input]
  (reduce #(merge %1 (pipe-line %2)) {} (str/split-lines input)))

(defn pipe-group
  [pipes start-node]
  (loop [seen #{}
         see  #{start-node}]
    (if-let [node (first see)]
      (recur (conj seen node)
             (set/union (disj see node) (set/difference (get pipes node) seen)))
      seen)))

(defn pipe-groups
  [pipes]
  (loop [pipes  pipes
         groups #{}]
    (if-let [start-node (first (keys pipes))]
      (let [group (pipe-group pipes start-node)]
        (recur (apply dissoc pipes group)
               (conj groups group)))
      groups)))
