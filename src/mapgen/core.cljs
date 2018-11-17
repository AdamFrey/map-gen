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

(def origin-tile-types
  {:grass [:g :g :g :g :g :g :g :g]
   :grass-water [:g :g :g :b :w :w :w :b]
   :grass-water-corner [:g :g :g :b :w :b :g :g]
   :water-grass-corner [:w :w :w :b :g :b :w :w]
   :water [:w :w :w :w :w :w :w :w]
   :sand [:s :s :s :s :s :s :s :s]
   :sand-grass [:g :g :g :s :s :s :s :s]
   :grass-sand-corner [:g :s :s :s :s :s :s :s]
   :sand-grass-corner [:s :s :g :g :g :g :g :s]
   :lava [:l :l :l :l :l :l :l :l]
   :lava-sand [:l :l :l :l :s :s :s :l]
   :lava-sand-corner [:l :l :l :l :s :l :l :l]
   :sand-lava-corner [:s :s :s :l :l :l :s :s]})

(def init-weights
  {:grass 10
   :grass-water 5
   :water 10
   :grass-water-corner 3
   :water-grass-corner 3})

(def init-prob
  {:grass-0 (/ 1 3)
   :grass-water-0 (/ 1 3)
   :water-0 (/ 1 3)})

(defn make-board [size]
  (into {}
        (for [x (range size)
              y (range size)]
          [[x y] {:x x
                  :y y
                  :prob init-prob}])))

(def board-size 15)
(def tile-size 15)
(def global-scale 2)
(def disk-size 1)

(defn disk-around [[xx yy]]
  (for [x (range (- xx disk-size) (+ xx (inc disk-size)))
        y (range (- yy disk-size) (+ yy (inc disk-size)))
        :when (not= [x y] [xx yy])]
    [x y]))

#_(disk-around [1 2])
;; => ([0 1] [0 2] [0 3] [1 1] [1 3] [2 1] [2 2] [2 3])

(defn normalize [prob-map]
  (let [prob-sum (apply + (vals prob-map))]
    (into {}
          (for [[type prob] prob-map]
            [type (/ prob prob-sum)]))))

#_(normalize {:a 0.3 :b 0.3})
;; => {:a 0.5, :b 0.5}

(defn joint-probability [prob-map-a prob-map-b]
  (normalize
   (into {}
         (for [[type prob-b] prob-map-b
               :let [prob-a (type prob-map-a)]]
           [type (* prob-b prob-a)]))))

#_(joint-probability {:a 0.5 :b 0.5} {:a 0 :b 1})
;; => {:a 0, :b 1}

#_(joint-probability {:a 0 :b 1} {:a 0.3 :b 0.6})
;; => {:a 0, :b 1}

#_(joint-probability {:a .6 :b .3} {:a 0.3 :b 0.6})
;; => {:a 0.5, :b 0.5}

(defn propagate-probability [board position]
  (let [element (get board position)
        neighbors-pos (disk-around position)
        updated-neighbors (doall
                           (for [n-pos neighbors-pos]
                             (when-let [n-el (get board n-pos)]
                               (let [new-prob (joint-probability (:prob element) (:prob n-el))]
                                 [n-pos (assoc n-el :prob new-prob)]))))]
    (into board updated-neighbors)))

#_(propagate-probability fake-board [1 1])

#_(into {}
        (for [x [0 1 2]
              y [0 1 2]]
          [[x y] {:x x :y y :prob {:a (/ 1 3)
                                   :b (/ 2 3)}}]))
(def fake-board
  {[2 2] {:x 2, :y 2, :prob {:a 0.3333333333333333, :b 0.6666666666666666}},
   [0 0] {:x 0, :y 0, :prob {:a 0.3333333333333333, :b 0.6666666666666666}},
   [1 0] {:x 1, :y 0, :prob {:a 0.3333333333333333, :b 0.6666666666666666}},
   [1 1] {:x 1, :y 1, :prob {:a 0.3333333333333333, :b 0.6666666666666666}},
   [0 2] {:x 0, :y 2, :prob {:a 0.3333333333333333, :b 0.6666666666666666}},
   [2 0] {:x 2, :y 0, :prob {:a 0.3333333333333333, :b 0.6666666666666666}},
   [2 1] {:x 2, :y 1, :prob {:a 0.3333333333333333, :b 0.6666666666666666}},
   [1 2] {:x 1, :y 2, :prob {:a 0.3333333333333333, :b 0.6666666666666666}},
   [0 1] {:x 0, :y 1, :prob {:a 0.3333333333333333, :b 0.6666666666666666}}})

(def tile-paths
  '[grass.gif
    grass-sand-corner.gif
    grass-water-corner.gif
    grass-water.gif
    lava.gif
    lava-sand-corner.gif
    lava-sand.gif
    sand.gif
    sand-grass-corner.gif
    sand-grass.gif
    sand-lava-corner.gif
    water.gif
    water-grass-corner.gif])

(def tile->asset-path
  (into {}
        (map (fn [path]
               [(keyword (first (str/split (str path) #"\.")))
                (str "assets/" path)]))
        tile-paths))





(defrecord Tile
    [tile-type orientation])

(defn rotated-tiles [t]
  (map #(vector (keyword (str (name t) "-" %))
                (->> (cycle (get origin-tile-types t))
                     (drop (* 2 %))
                     (take 8)))
       (range 4)))

(def boundary-map
  (into {} (apply concat (map rotated-tiles (keys origin-tile-types)))))

#_(keys boundary-map)

;;; positions from T
;;; 0
;;; 3 T 1
;;; 2

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

(defn adjacency-map
  []
  (filter last (mapcat adjacency-map-for-tile (keys boundary-map))))

(def position-coords
  {0 [0 -1]
   1 [1 0]
   2 [0 1]
   3 [-1 0]})

(defn neighbors
  []
  (reduce (fn [acc [t1 t2 pos _]]
            (update-in acc [t1 (get position-coords pos)]
                       #((fnil conj #{}) % t2)))
          {}
          (adjacency-map)))

(defn parse-tile-key
  [k]
  (drop 1 (re-find #"(.*)-(\d+)$" (name k))))

(def tiles
  (reduce (fn [acc [k v]]
            (let [[base-tile rot] (parse-tile-key k)]
              (assoc acc
                     k
                     {:type (keyword base-tile)
                      :rot (js/parseInt rot)
                      :neighbors v})))
          {}
          (neighbors)))

(defn make-board-weights [size]
  (into {}
        (for [x (range size)
              y (range size)]
          [[x y] {:x x
                  :y y
                  :allowed-tiles (keys tiles)}])))

(defn asset+rotation [state tile]
  (let [base-tile (keyword (second (re-find #"(.+)\-\d$" (name tile))))
        img (-> state :images base-tile)]
    (case base-tile
      (:water :lava :sand :grass) [img 0]
      [img (* (/ js/Math.PI 2) (js/parseInt (re-find #"\d$" (name tile))))])))

(defn draw-image [state prob-map]
  (doseq [[tile prob] prob-map]
    (let [[asset rotation-rad] (asset+rotation state tile)]
      (q/rotate rotation-rad)
      (q/tint-float 255 (* 255 prob))
      (q/image asset 0 0)
      (q/rotate (- rotation-rad)))))

(defn asset+rotation-weight [state tile]
  (let [base-tile (get-in tiles [tile :type])
        rot (get-in tiles [tile :rot])
        img (-> state :images base-tile)]
    (case base-tile
      (:water :lava :sand :grass) [img 0]
      [img (- (* (/ js/Math.PI 2) rot))])))

(defn grouped-tiles-and-total-weight [allowed-tiles]
  (let [grouped-tiles (group-by #(get-in tiles [% :type]) allowed-tiles)]
    [grouped-tiles (apply + (map #(init-weights % 0) (keys grouped-tiles)))]))

(defn draw-image-weights [state allowed-tiles]
  (let [[grouped-tiles total-weight] (grouped-tiles-and-total-weight allowed-tiles)]
    #_(println "Drawing " grouped-tiles)
    (doseq [[tile-group tile-subset] grouped-tiles]
      (let [weight (/ (get init-weights tile-group 0) (count tile-subset))]
        (doseq [tile tile-subset]
          (let [[asset rotation-rad] (asset+rotation-weight state tile)]
            (q/rotate rotation-rad)
            (q/tint-float 255 (* 255 (/ weight total-weight)))
            (q/image asset 0 0)
            (q/rotate (- rotation-rad))))))))

(defn draw-state-weights [state]
  (q/scale global-scale)
  (q/image-mode :center)
  (doseq [column (:map state)
          row column]
    (let [{:keys [x y allowed-tiles tile-type]} row
          tile (rand-nth (keys (:images state)))]
      (q/with-translation [(+ (/ tile-size 2) (* x tile-size))
                           (+ (/ tile-size 2) (* y tile-size))]
        (if tile-type
          (draw-image-weights state #{tile-type})
          (draw-image-weights state allowed-tiles ))))))

(defn setup-weights [camera]
(q/frame-rate 1)
(q/color-mode :hsb)
{:map (make-board-weights board-size)
 :images (reduce-kv (fn [acc k v]
                      (assoc acc k (q/load-image v)))
                    {} tile->asset-path)
 :camera camera})

(defn pick-tile [board]
  (let [tiles-grouped-by-allowed (dissoc (group-by (fn [[k v]] (count (:allowed-tiles v))) board)
                                         0)
        min-count (first (sort (keys tiles-grouped-by-allowed)))
        ;; note that since board is a map, each entry is a key-value pair.
        tile (rand-nth (get tiles-grouped-by-allowed min-count))]
    tile))

(defn pick-tile-type [allowed-tiles]
  (let [[tile-groups total-weight] (grouped-tiles-and-total-weight allowed-tiles)
        rand-value (rand total-weight)
        chosen-group (reduce (fn [acc v]
                               (let [new-total (+ acc (init-weights v 0))]
                                 (if (> new-total rand-value)
                                   (reduced v)
                                   new-total)))
                             0
                             (keys tile-groups))]
    (rand-nth (tile-groups chosen-group))))

(defn update-state-weights [state]
  (let [board (:map state)
        [coords {:keys [allowed-tiles]}] (pick-tile board)
        new-type (pick-tile-type allowed-tiles)
        new-board (update board coords #(-> %
                                            (assoc :tile-type new-type)
                                            (dissoc :allowed-tiles)))]
    (println "updating" coords "to" new-type)
    (assoc state :map new-board)))

(defn draw-state [state]
  (q/scale global-scale)
  (q/image-mode :center)
  (doseq [column (:map state)
          row column]
    (let [{:keys [x y prob]} row
          tile (rand-nth (keys (:images state)))]
      (q/with-translation [(+ (/ tile-size 2) (* x tile-size))
                           (+ (/ tile-size 2) (* y tile-size))]
        (draw-image state prob)))))

(defn setup [camera]
  (q/frame-rate 1)
  (q/color-mode :hsb)
  {:map (make-board board-size)
   :images (reduce-kv (fn [acc k v]
                        (assoc acc k (q/load-image v)))
                      {} tile->asset-path)
   :camera camera})

(let [c-size (* 2 board-size tile-size)
      canvas [c-size c-size]
      c (camera canvas 64 0.6)]
  (q/defsketch my-sketch
    :host "display"
    :size [(c :width) (c :height)]
    :setup (partial setup-weights c)
    :update update-state-weights
    :draw draw-state-weights
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
