(ns dame.core
  (:require                                                 ;[doo.runner :refer-macros [doo-tests]]
            [reagent.core :as reagent :refer [atom]]
    ;[dame.core-test]
            ))

;(doo-tests 'dame.core-test)

(enable-console-print!)

(def board-size 8)
(defn black [i j]
  {:color :black, :dir :bot, :i i, :j j})
(defn white [i j]
  {:color :white, :dir :top, :i i, :j j})

(def clicked-state
  "i store clicked figure for moving in an atom, lazy!"
  (atom {:clicked false, :i nil, :j nil, :move []}))

(defn new-board [n]
  (vec (repeat n (vec (repeat n 0)))))

(defn is-black-field? [i j]
  "pred for coordinate i j
  according to chess board rules"
  (if (even? j)
    (if (even? i)
      false
      true)
    (if (even? i)
      true
      false)))


(defn black-start [board]
  "3 rows of queens at start for black MAYBE MERGE?"
  (let [black-start-fields (for [i (range board-size)
                                 j (range 3)
                                 :when (is-black-field? i j)]
                             [i j])]
    (reduce (fn [acc [i j]] (assoc-in acc [j i] (black i j))) board black-start-fields)))

(defn white-start [board]
  "3 rows of queens at start for white MAYBE MERGE?"
  (let [white-start-fields (for [i (range board-size)
                                 j (range (- board-size 3) board-size)
                                 :when (is-black-field? i j)]
                             [i j])]
    (reduce (fn [acc [i j]] (assoc-in acc [j i] (white i j))) board white-start-fields)))

(defn fill-board [board]
  "put em in"
  (white-start (black-start board)))

(defn inField?
  "this function with its arity is a synonym to technical debt"
  ([[i j]]
    (and
      (< i board-size)
      (< j board-size)
      (>= i 0)
      (>= j 0)))
  ([[i j] [x y]]
    (and (inField? [i j])
         (inField? [x y])))
  ([[i j] [x y] & args]
    (and
      (inField? [i j])
      (inField? [x y])
      (every? inField? args))))

(defn move-direction [figure]
  "Gives the first layer of move directions, means diagonal according to the state
  of your figure"
  (cond
    (= (:dir figure) :top)
    [[-1 -1] [1 -1]]
    (= (:dir figure) :bot)
    [[-1 1] [1 1]]
    (= (:dir figure) :all)
    [[-1 -1] [1 -1] [-1 1] [1 1]]
    :else
    []))

(defonce app-state (atom {:text          "Mein Damenspiel!"
                          :board         (fill-board (new-board board-size))
                          :game-status   :in-progress
                          :turn          :white
                          :clicked-state clicked-state
                          :toMove []
                          :possible-hits []}))

(defn get-all-figures-from [state player]
  (->> (flatten (:board state))
       (filter (fn [e] (= player (:color e))))))

(defn explode-dir [figure]
  "maps all directions for that figure
  the result is a vector [[[x y] [dir1] [[x y] [dir2]] ...]"
  (mapv (fn [e]
         [[(:i figure) (:j figure)]
          e])
       (move-direction figure)))

(defn explode-all-figures-from [state player]
  "explodes all figures from the given player in state"
  (into [] (apply concat (map (fn [e]
                                (explode-dir e))
                              (get-all-figures-from state player)))))

(defn expand-to-three-field [[[i j] [di dj]]]
  "one vector one direction but rome is everywhere"
  (let [one [i j]
        two [(+ i di) (+ j dj)]
        three [(+ (* 2 di) i) (+ (* 2 dj) j)]]
    [one two three]))

(defn explode-all-figures-dir [figure-dir]
  "takes [[[1 0] [-1 1]] ...]
  and wraps it into  [[[1 0] [0 1] [-1 2]] ...]
  if one value is not in inField, filterEDED!!!"
  (filter
    (fn [e] (inField? (first e) (second e) (last e)))
    (map #(expand-to-three-field %) figure-dir)))

(defn hit? [state [[i j] [ii jj] [iii jjj]]]
  "i really dont know how to make this more enjoyable to read for myself"
  (let [first (get-in (:board state) [j i])
        second (get-in (:board state) [jj ii])
        third (get-in (:board state) [jjj iii])
        fcolor (:color first)
        scolor (:color second)]
    (if (zero? third)
      (if (zero? second)
        false
        (if ((complement =) fcolor scolor)
          true
          false))
      false)))

(defn filter-for-valid-hits [state player]
  (filter (fn [e]
            (hit? state e))
          (explode-all-figures-dir (explode-all-figures-from state player))))

(defn check-for-hits [state]
  (let [computed-once (filter-for-valid-hits state (:turn state))
        clear-buffer (conj state [:possible-moves []])]
    (if ((complement empty?) computed-once)
      (conj clear-buffer computed-once)
      clear-buffer)))


(defn blank [i j]
  [:rect {:width  0.9
          :height 0.9
          :fill   (if (is-black-field? i j)
                    "black"
                    "ivory")
          :x      i
          :y      j}])

(defn to-move-field-click! [i j]
  (when (get @app-state :clicked-state)
    (let [accI (:i @(get @app-state :clicked-state))
          accJ (:j @(get @app-state :clicked-state))
          accFigure (get-in @app-state [:board accJ accI])]
      ;    (do
      (swap! app-state assoc-in [:board j i] accFigure)
      (swap! app-state assoc-in [:board accJ accI] 0)
      (swap! app-state assoc :clicked-state nil)
      (let [beatenFigure (reduce (fn [acc e]
                                   (when (= (get e 2) [i j])
                                     (when (= (get e 0) [accI accJ])
                                       (get e 1))))
                                 []
                                 (get @app-state :toMove))]
        (if ((complement empty?) beatenFigure)
          (swap! app-state assoc-in
                 [:board (second beatenFigure) (first beatenFigure)] 0)
          (swap! app-state assoc :turn (if (= (:turn @app-state) :black) :white :black)))))
    (swap! app-state assoc :toMove)))


(defn to-move-field [i j]
  [:rect {:width  0.9
          :height 0.9
          :x      i
          :fill   "red"
          :opacity 0.2
          :y      j
          :on-click
          (fn blank-click [e]
            (to-move-field-click! i j))}])

(defn click-on-figure! [i j]
  "Takes either a white or a black figure and marks it as clicked"
  (when (=
          (:color (get-in @app-state [:board j i]))
          (:turn @app-state))
    (swap! app-state assoc :clicked-state (atom {:clicked true
                                                 :i       i
                                                 :j       j
                                                 ;:move (calcute-direction @app-state i j)
                                                 }))
    ;(prn (get @app-state :clicked-state))
    ))
;  (and
;(get (get-clicked-Atom @app-state) :clicked)) )

(defn black-figure [i j]
  [:svg
   {:on-click
    (fn some-fn [e]
      (click-on-figure! i j))}
   (blank i j)
   [:circle
    {:r    0.45
     :fill "gray"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]
   [:circle
    {:r    0.40
     :fill "black"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]
   [:circle
    {:r    0.20
     :fill "grey"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]])

(defn white-figure [i j]
  [:svg
   {:on-click
    (fn some-fn [e]
      (click-on-figure! i j))}
   (blank i j)
   [:circle
    {:r    0.45
     :fill "yellow"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]
   [:circle
    {:r    0.40
     :fill "ivory"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]
   [:circle
    {:r    0.20
     :fill "yellow"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]])

(defn clicked-figure [i j]
  [:svg
   (blank i j)
   [:circle
    {:r    0.45
     :fill "red"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]
   [:circle
    {:r    0.40
     :fill "black"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]
   [:circle
    {:r    0.20
     :fill "red"
     :cx   (+ 0.45 i)
     :cy   (+ 0.45 j)}]])

(comment
(defn parse-for-matches [state i j]
  (filter (fn [e] (and
                    (= (first e) i)
                    (= (second e) j)))
          (when (get state :clicked-state)
            (:move @(:clicked-state state)))))

(defn matched? [state i j]
  (and
    (:clicked @(:clicked-state state))
    (= i (:i @(:clicked-state state)))
    (= j (:j @(:clicked-state state)))))
)

(defn draughts []
  [:center
   [:h1 (:text @app-state)]
   [:h2 (:turn @app-state)]
   [:button
    {:on-click
     (fn change-turn [e]
       (swap! app-state assoc :turn (if (= (:turn @app-state) :white)
                                      :black
                                      :white)))}
    "change turn"]
   (into
     [:svg {:view-box (str "0 0 " board-size " " board-size)
            :width    500
            :height   500}
      [:rect
       {:width  1000
        :height 1000
        :fill   "brown"
        :x      -200
        :y      -200}]]
     (for [i (range board-size)
           j (range board-size)]
       (cond
         ;         ((complement empty?) (parse-for-matches @app-state i j))
         ;         (to-move-field i j)
         ;
         ; (when (:clicked-state @app-state)
         ;           (matched? @app-state i j)
         ;)

;         (clicked-figure i j)
         :else
         (case (:color (get-in @app-state [:board j i]))
           nil [blank i j]
           :black [black-figure i j]
           :white [white-figure i j]))))]
  )

(reagent/render-component [draughts]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  (prn "check for hits, black:")
  (prn (check-for-hits @app-state)))
