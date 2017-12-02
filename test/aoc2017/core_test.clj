(ns aoc2017.core-test
  (:require [clojure.test :refer (deftest is are)]
            [aoc2017.core :as sut]))

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
