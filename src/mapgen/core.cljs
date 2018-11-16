(ns mapgen.core
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
   :light-range 10})

(defn make-board [size]
  (to-array-2d
   (mapv (fn [y x-column]
           (mapv (fn [x] {:x x :y y}) x-column))
         (range size) (repeat size (range size)))))

(defn setup [camera]
  (q/frame-rate 24)
  (q/color-mode :hsb)
  {:map (make-board 15)
   :images (q/load-image "assets/water.gif")
   :camera camera
   :player {:coordinates [4 4]
            :direction 0.45}})

(def tile-size 50)

(defn draw-state [state]
  (q/image (:img state) 0 0)
  #_(doseq [row (first (:map state))]
      (let [{:keys [x y]} row]
        (q/fill 90 80 70)
        #_(q/image (:img state) 0 0)
        (q/rect (* x tile-size) (* y tile-size) tile-size tile-size))))

(let [canvas [800 800]
      c (camera canvas 64 0.8)]
  (q/defsketch my-sketch
    :host "display"
    :size [(c :width) (c :height)]
    :setup (partial setup c)
    ;;:update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
