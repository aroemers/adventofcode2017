(ns aoc2017.core-test
  (:require [clojure.test :refer (deftest is are)]
            [aoc2017.core :as sut]
            [clojure.walk :as walk]))

;;; Day 1 - Inverse Captcha

(deftest rotate-test
  (is (= (sut/rotate 1 [1 1 2 2])
         [1 2 2 1])))

(deftest keep*-test
  (is (= (sut/keep* #(when (pos? %1) (+ %1 %2)) [-1 0 1] [2 2 2])
         [3])))

(deftest sum-test
  (is (= (sut/sum [1 2 3 4 5]) 15)))

(deftest same-test
  (are [args result] (= (apply sut/same args) result)
    [1 1]     1
    [0 1]     nil
    [2 2 2]   2
    [2 2 1]   nil
    [nil nil] nil))

(deftest inverse-captcha-test
  (are [captcha  move sum] (= (sut/inverse-captcha captcha move) sum)
    [1 1 2 2]         1 3
    [1 1 1 1]         1 4
    [1 2 3 4]         1 0
    [9 1 2 1 2 1 2 9] 1 9

    [1 2 1 2]         2 6
    [1 2 2 1]         2 0
    [1 2 3 4 2 5]     3 4
    [1 2 3 1 2 3]     3 12
    [1 2 1 3 1 4 1 5] 4 4))


;;; Day 2 - Corruption Checksum

(deftest difference-test
  (are [input result] (= (sut/difference input) result)
    [5 3 1 8] 7
    [1 1]     0))

(deftest division-test
  (are [input result] (= (sut/division input) result)
    [5 9 2 8] 4
    [9 4 7 3] 3
    [3 8 6 5] 2))

(deftest spreadsheet-checksum-test
  (is (= (sut/spreadsheet-checksum
          [[5 1 9 5]
           [7 5 3]
           [2 4 6 8]]
          sut/difference)
         18))

  (is (= (sut/spreadsheet-checksum
          [[5 9 2 8]
           [9 4 7 3]
           [3 8 6 5]]
          sut/division)
         9)))


;;; Day 4 - High-Entropy Passphrases

(deftest no-doubles-test
  (is (= (sut/no-doubles [["aa" "bb" "cc" "dd" "ee"]
                          ["aa" "bb" "cc" "dd" "aa"]
                          ["aa" "bb" "cc" "dd" "aaa"]])
         [["aa" "bb" "cc" "dd" "ee"]
          ["aa" "bb" "cc" "dd" "aaa"]])))

(deftest no-anagrams-test
  (is (= (sut/no-anagrams [["abcde" "fghij"]
                           ["abcde" "xyz" "ecdab"]
                           ["a" "ab" "abc" "abd" "abf" "abj"]
                           ["iiii" "oiii" "ooii" "oooi" "oooo"]
                           ["oiii" "ioii" "iioi" "iiio"]])
         [["abcde" "fghij"]
          ["a" "ab" "abc" "abd" "abf" "abj"]
          ["iiii" "oiii" "ooii" "oooi" "oooo"]])))


;;; Day 5 - A Maze of Twisty Trampolines, All Alike

(deftest jumps*-test
  (is (= (sut/jumps-inc   [0 3 0 1 -3])  5))
  (is (= (sut/jumps-three [0 3 0 1 -3]) 10)))


;;; Day 6 - Memory Reallocation

(deftest max-index-test
  (is (= (sut/max-index [0 2 7 0]) [7 2])))

(deftest reallocate-test
  (is (= (sut/reallocate [0 2 7 0]) [2 4 1 2])))

(deftest reallocates-test
  (is (= (sut/reallocates [0 2 7 0])
         {:total-steps 5
          :cycle-steps 4})))


;;; Day 7 - Recursive Circus

(def input-7 [{:node "pbga", :weight 66, :children nil}
              {:node "xhth", :weight 57, :children nil}
              {:node "ebii", :weight 61, :children nil}
              {:node "havc", :weight 66, :children nil}
              {:node "ktlj", :weight 57, :children nil}
              {:node "fwft", :weight 72, :children #{"cntj" "xhth" "ktlj"}}
              {:node "qoyq", :weight 66, :children nil}
              {:node "padx", :weight 45, :children #{"qoyq" "havc" "pbga"}}
              {:node "tknk", :weight 41, :children #{"ugml" "padx" "fwft"}}
              {:node "jptl", :weight 61, :children nil}
              {:node "ugml", :weight 68, :children #{"ebii" "jptl" "gyxo"}}
              {:node "gyxo", :weight 61, :children nil}
              {:node "cntj", :weight 57, :children nil}])

(deftest parse-line-test
  (is (= (sut/parse-line "foobar (38)")
         {:node     "foobar"
          :weight   38
          :children nil}))
  (is (= (sut/parse-line "foobar (38) -> alice, bob")
         {:node     "foobar"
          :weight   38
          :children #{"alice" "bob"}})))

(deftest find-root-test
  (is (= (sut/find-root input-7) "tknk")))

(deftest make-graph-test
  (is (= (sut/make-graph input-7)
         {:node "tknk", :weight 41,
          :children [{:node "ugml", :weight 68,
                      :children [{:node "ebii", :weight 61, :children ()}
                                 {:node "jptl", :weight 61, :children ()}
                                 {:node "gyxo", :weight 61, :children ()}]}
                     {:node "padx", :weight 45,
                      :children [{:node "qoyq", :weight 66, :children ()}
                                 {:node "havc", :weight 66, :children ()}
                                 {:node "pbga", :weight 66, :children ()}]}
                     {:node "fwft", :weight 72,
                      :children [{:node "cntj", :weight 57, :children ()}
                                 {:node "xhth", :weight 57, :children ()}
                                 {:node "ktlj", :weight 57, :children ()}]}]})))

(deftest weight-walker-test
  (is (= (->> input-7 sut/make-graph (walk/postwalk sut/weight-walker))
         {:node "tknk", :weight 41, :sum 778, :balanced? false
          :children [{:node "ugml", :weight 68, :sum 251, :balanced? true
                      :children [{:node "ebii", :weight 61, :children (), :sum 61, :balanced? true}
                                 {:node "jptl", :weight 61, :children (), :sum 61, :balanced? true}
                                 {:node "gyxo", :weight 61, :children (), :sum 61, :balanced? true}]}
                     {:node "padx", :weight 45, :sum 243, :balanced? true
                      :children [{:node "qoyq", :weight 66, :children (), :sum 66, :balanced? true}
                                 {:node "havc", :weight 66, :children (), :sum 66, :balanced? true}
                                 {:node "pbga", :weight 66, :children (), :sum 66, :balanced? true}]}
                     {:node "fwft", :weight 72, :sum 243, :balanced? true
                      :children [{:node "cntj", :weight 57, :children (), :sum 57, :balanced? true}
                                 {:node "xhth", :weight 57, :children (), :sum 57, :balanced? true}
                                 {:node "ktlj", :weight 57, :children (), :sum 57, :balanced? true}]}]})))

(deftest find-unbalance-test
  (is (= (sut/find-unbalance input-7) 60)))


;;; Day 8 - I Heard You Like Registers

(def input-8 "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(deftest max-register-test
  (is (= (sut/max-register input-8) 1)))

(deftest max-alltime-time
  (is (= (sut/max-alltime input-8) 10)))


;;; Day 9 - Stream Processing

(deftest groups-test
  (are [input result] (= (sut/groups input) result)
    "{}"                        [1]
    "{{{}}}"                    [1 2 3]
    "{{},{}}"                   [1 2 2]
    "{{{},{},{{}}}}"            [1 2 3 3 3 4]
    "{<{},{},{{}}>}"            [1]
    "{<a>,<a>,<a>,<a>}"         [1]
    "{{<a>},{<a>},{<a>},{<a>}}" [1 2 2 2 2]
    "{{<!>},{<!>},{<!>},{<a>}}" [1 2]))

(deftest groups-score-test
  (are [input result] (= (sut/groups-score input) result)
    "{}"                        1
    "{{{}}}"                    6
    "{{},{}}"                   5
    "{{{},{},{{}}}}"            16
    "{<{},{},{{}}>}"            1
    "{<a>,<a>,<a>,<a>}"         1
    "{{<a>},{<a>},{<a>},{<a>}}" 9
    "{{<!>},{<!>},{<!>},{<a>}}" 3))

(deftest garbage-count-test
  (are [input result] (= (sut/garbage-count input) result)
    "<>"                  0
    "<random characters>" 17
    "<<<<>"               3
    "<{!>}>"              2
    "<!!>"                0
    "<!!!>>"              0
    "<{o\"i!a,<{i<a>"     10))


;;; Day 10 - Knot Hash

(deftest knot-hash-test
  (is (= (sut/knot-hash 5 1 [3 4 1 5]) [3 4 2 1 0])))

(deftest knot-hash-score-test
  (is (= (sut/knot-hash-score 5 [3 4 1 5]) 12)))

(deftest knot-hash-hex-test
  (are [input result] (= (sut/knot-hash-hex input) result)
    ""         "a2582a3a0e66e6e86e3812dcb672a272"
    "AoC 2017" "33efeb34ea91902bb2f59c9920caa6cd"
    "1,2,3"    "3efbe78a8d82f29979031a4aa0b16a9d"
    "1,2,4"    "63960835bcdc130f0b66d7ff4f6a5a8e"))
