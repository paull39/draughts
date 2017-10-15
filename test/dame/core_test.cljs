(ns dame.core-test
  (:require [cljs.test :refer-macros [deftest is]]
            [dame.core :as c]))

(deftest i-should-fail
  (is (= 0 1) "I should fail"))

(deftest i-should-succeed
  (is (= 1 1) "I should succeed"))

(deftest black-figure-test
  (is (= :black (:color c/black)) "Problem with the 'define of a Black Figure"))

(deftest inField?-test
  (is (= false (c/inField? [-1 -1])) "negative Values are not valid for inField")
  (is (= true (c/inField? [0 0] [0 0] [(dec c/board-size) (dec c/board-size)])))
  "two always true values have been broken")