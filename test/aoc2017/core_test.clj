(ns aoc2017.core-test
  (:require [clojure.test :refer (deftest is are)]
            [aoc2017.core :as sut]))

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
