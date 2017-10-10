(ns dame.core
  (:require                                                 ;[doo.runner :refer-macros [doo-tests]]
            [reagent.core :as reagent :refer [atom]]
    ;[dame.core-test]
            ))

;(doo-tests 'dame.core-test)

(enable-console-print!)

(def board-size 8)
(def black {:color :black, :dir :bot})
(def white {:color :white, :dir :top})
(def clicked-state (atom {:clicked false,
                          :i       nil,
                          :j       nil,
                          :move    []}))

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
  "3 rows of queens at start for black"
  (let [black-start-fields (for [i (range board-size)
                                 j (range 3)
                                 :when (is-black-field? i j)]
                             [i j])]
    (reduce (fn [acc [i j]] (assoc-in acc [j i] black)) board black-start-fields)))

(defn white-start [board]
  "3 rows of queens at start for white"
  (let [white-start-fields (for [i (range board-size)
                                 j (range (- board-size 3) board-size)
                                 :when (is-black-field? i j)]
                             [i j])]
    (reduce (fn [acc [i j]] (assoc-in acc [j i] white)) board white-start-fields)))

(defn fill-board [board]
  "put em in"
  (white-start (black-start board)))



(defonce app-state (atom {:text          "Mein Damenspiel!"
                          :board         (fill-board (new-board board-size))
                          :game-status   :in-progress
                          :turn          :white
                          :clicked-state clicked-state
                          :toMove []}))

(defn move-direction [state i j]
  "Gives the first layer of move directions, means diagonal according to the state
  of your figure"
  (cond
    (=
      (:dir (get-in state [:board j i]))
      :top)
    [[-1 -1] [1 -1]]
    (=
      (:dir (get-in state [:board j i]))
      :bot)
    [[-1 1] [1 1]]
    :else
    [[-1 -1] [1 -1] [-1 1] [1 1]]))

(defn vector-calc! [state i j dx dy]
  "if nothings is in the way just returns the first layer of moves,
  if sth is in the way checks if its your own figure or sb elses
  and returns a coordinate according to that"
  (let [x (+ i dx)
        y (+ j dy)]
    (if ((complement zero?) (get-in state [:board y x]))
      (when ((complement =)
              (:turn state)
              (:color (get-in state [:board y x])))
          (let [x2 (+ i (* 2 dx))
                y2 (+ j (* 2 dy))]
            (when (zero? (get-in state [:board y2 x2]))
              (swap! app-state assoc :toMove (conj (:toMove @app-state) [[i j] [x y] [x2 y2]]))
              [x2 y2])))
      [x y])))

(defn calcute-direction [state i j]
  "2 parted the calculation to make it easier for figures who are able
  to go to all directions"
  (for [dir (move-direction state i j)]
    (vector-calc! state i j (first dir) (second dir))))

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

(defn click-on-figure [i j]
  "Takes either a white or a black figure and marks it as clicked"
  (when (=
          (:color (get-in @app-state [:board j i]))
          (:turn @app-state))
    (swap! app-state assoc :clicked-state (atom {:clicked true
                                                 :i i
                                                 :j j
                                                 :move (calcute-direction @app-state i j)}))
    ;(prn (get @app-state :clicked-state))
    ))
;  (and
;(get (get-clicked-Atom @app-state) :clicked)) )

(defn black-figure [i j]
  [:svg
   {:on-click
    (fn some-fn [e]
      (click-on-figure i j))}
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
      (click-on-figure i j))}
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
         ((complement empty?) (parse-for-matches @app-state i j))
         (to-move-field i j)
         (when (:clicked-state @app-state)
           (matched? @app-state i j))
         (clicked-figure i j)
         :else
         (case (:color (get-in @app-state [:board j i]))
           nil [blank i j]
           :black [black-figure i j]
           :white [white-figure i j]))))])

(reagent/render-component [draughts]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  (prn (:toMove @app-state))
  (prn "eyyw")
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  ;(prn (:move @(get @app-state :clicked-state)))
  )