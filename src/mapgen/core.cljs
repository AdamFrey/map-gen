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

(defn setup [camera]
  (q/frame-rate 24)
  (q/color-mode :hsb)
  {:map    [[1 1 1 1 1 1 1 1]
            [1 0 0 0 0 0 0 1]
            [1 1 1 0 0 1 1 1]
            [1 0 1 0 0 1 0 1]
            [1 0 1 0 0 1 0 1]
            [1 0 0 0 0 0 0 1]
            [1 1 1 1 1 1 1 1]]
   :camera camera
   :player {:coordinates [4 4]
            :direction   0.45}})

(let [canvas [1320 1240]
      c      (camera canvas 64 0.8)]

  (q/defsketch my-sketch
    :host "display"
    :size [(c :width) (c :height)]
    :setup (partial setup c)
    ;;:update update-state
    ;;:draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
