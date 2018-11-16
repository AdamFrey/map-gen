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
   :range 14})

(defn make-board [size]
  (to-array-2d
   (mapv (fn [y x-column]
           (mapv (fn [x] {:x x :y y}) x-column))
         (range size) (repeat size (range size)))))

(defn setup [camera]
  (q/frame-rate 1)
  (q/color-mode :hsb)
  {:map (make-board 15)
   :images {:grass (q/load-image "assets/grass.gif")
            :water (q/load-image "assets/water.gif")
            :water-land-corner (q/load-image "assets/water-land-corner.gif")
            :land-water-corner (q/load-image "assets/land-water-corner.gif")
            :land-water (q/load-image "assets/land-water.gif")}
   :camera camera})

(def tile-size 15)

(defn draw-state [state]
  (doseq [column (:map state)
          row column]
    (let [{:keys [x y]} row
          tile (rand-nth (keys (:images state)))]
      (q/image (-> state :images tile) (* x tile-size) (* y tile-size)))))

(let [canvas [300 225]
      c (camera canvas 64 0.8)]
  (q/defsketch my-sketch
    :host "display"
    :size [(c :width) (c :height)]
    :setup (partial setup c)
    ;;:update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
