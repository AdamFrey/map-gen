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

(def init-prob
  {:grass (/ 1 3)
   :grass-water (/ 1 3)
   :water (/ 1 3)})


(defn make-board [size]
  (to-array-2d
   (mapv (fn [y x-column]
           (mapv (fn [x] {:x x :y y :prob init-prob}) x-column))
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

(def tile-paths
  '[grass-water.gif,
    grass-sand-corner.gif,
    sand.gif,
    lava-sand-corner.gif,
    grass-water-corner.gif,
    lava.gif,
    sand-grass-corner.gif,
    water-grass-corner.gif,
    sand-grass.gif,
    sand-lava-corner.gif,
    grass.gif,
    lava-sand.gif,
    water.gif])

(def tile->asset-path
  (into {}
        (map (fn [path]
               [(keyword (first (str/split (str path) #"\.")))
                (str "assets/" path)]))
        tile-paths))

(defn setup [camera]
  (q/frame-rate 1)
  (q/color-mode :hsb)
  {:map (make-board board-size)
   :images (reduce-kv (fn [acc k v]
                        (assoc acc k (q/load-image v)))
                      {} tile->asset-path)
   :camera camera})

;; (into {}
;;       (for [x [-1 0 1]
;;             y [-1 0 1]]
;;         [[x y] (case y
;;                  1 [:grass-water]
;;                  0 [:water]
;;                  -1 [:water])]))

 (def tiles
  {:grass {:type :grass
           :rot 0
           :neighbors
           {[0 1] [:grass],
            [0 0] [:grass],
            [-1 1] [:grass],
            [1 1] [:grass],
            [1 -1] [:grass-water],
            [1 0] [:grass],
            [-1 0] [:grass],
            [-1 -1] [:grass-water],
            [0 -1] [:grass-water]}}

   :grass-water {:type :grass-water
                :rot 0
                :neighbors
                {[0 1] [:grass],
                 [0 0] [:grass-water],
                 [-1 1] [:grass],
                 [1 1] [:grass],
                 [1 -1] [:water],
                 [1 0] [:grass-water],
                 [-1 0] [:grass-water],
                 [-1 -1] [:water],
                 [0 -1] [:water]}}

   :water {:type :water
           :rot 0
           :neighbors
           {[0 1] [:grass-water],
            [0 0] [:water],
            [-1 1] [:grass-water],
            [1 1] [:grass-water],
            [1 -1] [:water],
            [1 0] [:water],
            [-1 0] [:water],
            [-1 -1] [:water],
            [0 -1] [:water]}}})

(defn draw-state [state]
  (q/scale global-scale)
  (q/image-mode :center)
  (doseq [column (:map state)
          row column]
    (let [{:keys [x y prob]} row
          tile (rand-nth (keys (:images state)))
          rot (rand)]
      (q/with-translation [(+ (/ tile-size 2) (* x tile-size))
                           (+ (/ tile-size 2) (* y tile-size))]
        (q/rotate rot)
        (doseq [[t-type t-prob] prob]
          (q/tint-float 255 (* 255 t-prob))
          (q/image (-> state :images t-type) 0 0))
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

(defn adjacency-map-for-tile [t]
  (for [t2 (keys boundary-map)
        pos (range 4)]
    [t t2 pos(can-be-adjacent? t t2 pos)]))

(def adjacency-map
  (filter last (mapcat adjacency-map-for-tile (keys boundary-map))))

(def position-coords {0 [0 -1]
                      1 [1 0]
                      2 [0 1]
                      3 [-1 0]})

(def neighbors
  (reduce (fn [acc [t1 t2 pos _]] (update-in acc [t1 (get position-coords pos)] #((fnil conj []) % t2))) {} adjacency-map))
