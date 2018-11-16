(ns mapgen.core
  (:require [clojure.string :as str]
            [quil.core :as q :include-macros true]
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

(def sphere-size 2)

(defn placed-tile-probability-updates [board {origin-x :x origin-y :y}]
  (for [x (range (- origin-x sphere-size) (+ origin-x (inc sphere-size)))
        y (range (- origin-y sphere-size) (+ origin-y (inc sphere-size)))]
    (when-not (= [x y] [origin-x origin-y])
      (when-let [valid-tile (some-> board (aget y) (aget x))]
        valid-tile))))

(defn create-asset-map [images]
  (into {}
        (map (fn [sym]
               [(keyword (first (str/split (str sym) #"\."))) `(q/load-image ~(str "assets/" sym))]))
        images))

(comment
  (create-asset-map
   [])
  )

(defn setup [camera]
  (q/frame-rate 1)
  (q/color-mode :hsb)
  {:map (make-board board-size)
   :images {:grass-water (quil.core/load-image "assets/grass-water.gif"),
            :grass-sand-corner (quil.core/load-image "assets/grass-sand-corner.gif"),
            :sand (quil.core/load-image "assets/sand.gif"),
            :lava-sand-corner (quil.core/load-image "assets/lava-sand-corner.gif"),
            :grass-water-corner (quil.core/load-image "assets/grass-water-corner.gif"),
            :lava (quil.core/load-image "assets/lava.gif"),
            :palette (quil.core/load-image "assets/palette.gif"),
            :sand-grass-corner (quil.core/load-image "assets/sand-grass-corner.gif"),
            :water-grass-corner (quil.core/load-image "assets/water-grass-corner.gif"),
            :sand-grass (quil.core/load-image "assets/sand-grass.gif"),
            :sand-lava-corner (quil.core/load-image "assets/sand-lava-corner.gif"),
            :grass (quil.core/load-image "assets/grass.gif"),
            :lava-sand (quil.core/load-image "assets/lava-sand.gif"),
            :water (quil.core/load-image "assets/water.gif")}
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

