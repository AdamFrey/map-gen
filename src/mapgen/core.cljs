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
   :camera camera
   :player {:coordinates [4 4]
            :direction 0.45}})

(def tile-size 50)

(defn draw-state [state]
  (doseq [row (first (:map state))]
    (let [{:keys [x y]} row]
      (q/fill 90 80 70)
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

(defrecord Tile
    [tile-type orientation])

(def tile-types {:grass [:g :g :g :g :g :g :g :g]
                 :grass-water [:g :g :g :b :w :w :w :b]
                 :grass-water-corner [:g :g :g :b :w :b :g :g]
                 :water-grass-corner [:w :w :w :b :g :b :w :w]
                 :water [:w :w :w :w :w :w :w :w]})

(defn rotated-tiles [t]
  (map #(vector (keyword (str (name t) "-" %))
                (->> (cycle (get tile-types t))
                     (drop (* 2 %))
                     (take 8)))
       (range 4)))

(def boundary-map
  (into {} (apply concat (map rotated-tiles (keys tile-types)))))

;;; positions from T
;;;    0
;;;  3 T 1
;;;    2

(defn can-be-adjacent?
  [t1 t2 position]
  (= (->> (cycle (get boundary-map t1))
          (drop (* 2 position))
          (take 3))
     (->> (cycle (get boundary-map t2))
          (drop 4)
          (drop (* 2 position))
          (take 3)
          reverse)))

#_(defn adjacency-map [t]
    (for [t (keys boundary-map)
          pos ()])
    (filter #(fn [[t1 t2]] (can-be-adjacent? t1 t2) )))

