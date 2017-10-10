(ns dame.core-test
  (:require [cljs.test :refer-macros [deftest is]]
            [dame.core :as c]))

(deftest i-should-fail
  (is (= 0 1) "I should fail"))

(deftest i-should-succeed
  (is (= 1 1) "I should succeed"))

(deftest black-figure-test
  (is (= :black (:color c/black)) "Problem with the 'define of a Black Figure"))

(deftest move-direction-tops
  (is (=
        [[-1 -1] [1 -1]]
        (c/move-direction [[{:dir :top}]] 0 0))))

(deftest move-direction-tests)

(prn "aaa")
