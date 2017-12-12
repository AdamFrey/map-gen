(ns raycaster.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(enable-console-print!)

(println "This text is printed from src/raycaster/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(defn- camera
  [[width height] resolution focal-length]
  {:focal-length focal-length
   :width width
   :height height
   :resolution resolution
   :spacing (/ width resolution)
   :range 14
   :light-range 10
   })

(defn setup [camera]
                                        ; Set frame rate to 30 frames per second.
  (q/frame-rate 60)
                                        ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
                                        ; setup function returns initial state. It contains
                                        ; circle color and position.
  {:color 0
   :angle 0
   :map [[0 0 0 0 0 0 1]
         [0 0 0 0 0 0 0]
         [0 1 0 0 0 0 0]
         [0 0 0 0 0 1 0]
         [0 0 0 0 0 0 0]
         [0 0 0 0 0 0 0]
         [0 0 0 1 1 1 0]]
   :camera camera
   :player {:coordinates [3.5 3.5]
            :direction 0
            :range 10}})

(defn update-state [state] ; Update sketch state by changing circle color and position.
  (-> state
      (update-in [:player :direction] #(+ % 0.005))
      #_(update :color #(mod (+ % 10.7) 255))))

(defn inspect
  [m sin cos step shift-x shift-y distance offset]
  (let [dx (if (neg? cos) shift-x 0)
        dy (if (neg? sin) shift-y 0)
        coordinates [(int (- (:x step) dx)) (int (- (:y step) dy))]
        height (get-in m coordinates)
        distance' (+ distance (Math/sqrt (:length2 step)))
        shading (if (not (zero? shift-x))
                  (if (neg? cos) 2 0)
                  (if (neg? sin) 2 1))
        offset' (- offset (Math/floor offset))]
    (assoc step
           :height height
           :distance distance'
           :shading shading
           :offset offset')))

(defn step
  [rise run x y inverted?]
  (if (zero? run)
    {:length2 10}
    (let [dx (if (pos? run)
               (- (Math/floor (inc x)) x)
               (- (Math/ceil (dec x)) x))
          dy (* dx (/ rise run))
          [x' y'] (cond-> [(+ x dx) (+ y dy)]
                    inverted? ((comp vec reverse)))]
      {:x x' :y y' :length2 (+ (* dx dx) (* dy dy))})))

(defn ray
  [m rng sin cos origin]
  (if (> rng (:distance origin))
    (let [step-x (step sin cos (origin :x) (origin :y) false)
          step-y (step cos sin (origin :y) (origin :x) true)
          next-step (if (< (:length2 step-x) (:length2 step-y))
                      (inspect m sin cos step-x 1 0 (:distance origin) (:y step-x))
                      (inspect m sin cos step-y 0 1 (:distance origin) (:x step-y)))]
      (if (pos? (:height next-step))
        next-step
        (recur m rng sin cos next-step)))
    {:height 2 :distance 1000}))

(defn cast
  [m [x y] angle rng]
  (let [sin (Math/sin angle)
        cos (Math/cos angle)]
    (ray m rng sin cos {:x x :y y})))

(defn project
  [camera height angle distance]
  (if (zero? distance)
    {:top 0
     :height (camera :height)}
    (let [z (* distance (Math/cos angle))
          wall-height (/ (* (camera :height) height) z)
          bottom (* (/ (camera :height) 2) (+ 1 (/ 1 z)))]
      {:top (- bottom wall-height) :height wall-height})))

(defn draw-column
  [state camera m column ray angle]
  (let [left (* column (:spacing camera))
        wall (project camera (ray :height) angle (ray :distance))]
    (q/rect left (:top wall) (:spacing camera) (:height wall))))

(defn draw-state [state]
  (q/background 220) ; Clear the sketch by filling it with light-grey color.

  (q/fill (:color state) 100 100)
  (q/no-stroke)

  (let [spacing (get-in state [:camera :spacing])
        resolution (get-in state [:camera :resolution])]
    (doseq [column (range resolution)]
      (let [x (- (/ column resolution) 0.5)
            angle (Math/atan2 x (get-in state [:camera :focal-length]))
            ray (cast (state :map) (get-in state [:player :coordinates])
                      (+ angle (get-in state [:player :direction]))
                      (get-in state [:camera :range]))]
        (draw-column state (state :camera) (state :map) column ray angle)))))

(let [canvas [640 480]
      c (camera canvas 320 0.8)]
  (q/defsketch my-sketch
    :title "You spin my circle right round"
    :host "display"
    :size [(c :width) (c :height)]
                                        ; setup function called only once, during sketch initialization.
    :setup (partial setup c)
                                        ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
                                        ; This sketch uses functional-mode middleware.
                                        ; Check quil wiki for more info about middlewares and particularly
                                        ; fun-mode.
    :middleware [m/fun-mode]))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
