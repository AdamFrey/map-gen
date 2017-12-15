(ns raycaster.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(enable-console-print!)

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
  (q/frame-rate 24)
  (q/color-mode :hsb)
  {:map [[1 1 1 1 1 1 1 1]
         [1 0 0 0 0 0 0 1]
         [1 1 1 0 0 1 1 1]
         [1 0 1 0 0 1 0 1]
         [1 0 1 0 0 1 0 1]
         [1 0 0 0 0 0 0 1]
         [1 1 1 1 1 1 1 1]]
   :camera camera
   :player {:coordinates [4 4]
            :direction 0.45}})

(defn update-state [state]
  state)

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
    {:height 0 :distance 1000}))

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
        wall (project camera (ray :height) angle (ray :distance))
        brightness (:height wall)]
    (q/fill 90 80 brightness)
    (q/rect left (:top wall) (:spacing camera) (:height wall))))

(defn draw-state [state]
  ; Draw background
  (q/no-stroke)
  (q/background 140 100 255)

  ; Draw Floor
  (q/fill 40 100 60)
  (q/rect 0 120 320 240)

  ; Draw walls
  (let [spacing (get-in state [:camera :spacing])
        resolution (get-in state [:camera :resolution])]
    (doseq [column (range resolution)]
      (let [x (- (/ column resolution) 0.5)
            angle (Math/atan2 x (get-in state [:camera :focal-length]))
            ray (cast (state :map) (get-in state [:player :coordinates])
                      (+ angle (get-in state [:player :direction]))
                      (get-in state [:camera :range]))]
        (draw-column state (state :camera) (state :map) column ray angle)))))

(defn walk
  [state distance]
  (let [coordinates (get-in state [:player :coordinates])
        direction (get-in state [:player :direction])
        map (get-in state [:map])
        curr-x (first coordinates)
        curr-y (nth coordinates 1)
        dx (* distance (Math/cos direction))
        dy (* distance (Math/sin direction))
        new-x (if (<= (get-in map [(+ curr-x dx) curr-y]) 0)
                 (+ curr-x dx)
                 curr-x)
        new-y (if (<= (get-in map [curr-x (+ curr-y dy)]) 0)
                 (+ curr-y dy)
                 curr-y)]

        (assoc-in state [:player :coordinates] [new-x new-y])))

(defn key-pressed [state event]
  (case (:key event)
    (:w :up) (walk state 0.05)
    (:a :left) (update-in state [:player :direction] #(- % 0.05))
    (:s :down) (walk state -0.05)
    (:d :right) (update-in state [:player :direction] #(+ % 0.05))
    state))

(let [canvas [320 240]
      c (camera canvas 64 0.8)]

  (q/defsketch my-sketch
    :host "display"
    :size [(c :width) (c :height)]
    :setup (partial setup c)
    :update update-state
    :key-pressed key-pressed
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
