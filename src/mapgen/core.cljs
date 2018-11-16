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


(def board-size 15)
(def tile-size 15)
(def global-scale 2)

(defn setup [camera]
  (q/frame-rate 1)
  (q/color-mode :hsb)
  {:map (make-board board-size)
   :images {:grass (q/load-image "assets/grass.gif")
            :water (q/load-image "assets/water.gif")
            :water-land-corner (q/load-image "assets/water-land-corner.gif")
            :land-water-corner (q/load-image "assets/land-water-corner.gif")
            :land-water (q/load-image "assets/land-water.gif")}
   :camera camera})

(defn draw-state [state]
  (q/scale global-scale)
  (q/image-mode :center)
  (doseq [column (:map state)
          row column]
    (let [{:keys [x y]} row
          tile (rand-nth (keys (:images state)))
          rot (rand)]
      (q/with-translation [(+ (/ tile-size 2) (* x tile-size))
                           (+ (/ tile-size 2) (* y tile-size))]
        (q/rotate rot)
        (q/image (-> state :images tile) 0 0)
        (q/rotate (- rot)))
      )))

(let [c-size (* 2 board-size tile-size)
      canvas [c-size c-size]
      c (camera canvas 64 0.6)]
  (q/defsketch my-sketch
    :host "display"
    :size [(c :width) (c :height)]
    :setup (partial setup c)
    ;;:update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
