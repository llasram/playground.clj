(ns playground.heart
  (:use [rosado.processing]
        [rosado.processing.applet]
        [playground.util :only [ignore-errors]])
  (:import [processing.core PApplet]))

(defn setup []
  (size 200 200)
  (smooth)
  (no-stroke)
  (framerate 30)
  (color-mode PApplet/RGB 255)
  (background 255))

(defn bezier-path [path t]
  (let [[x1 y1 cx1 cy1 cx2 cy2 x2 y2] path]
    [(bezier-point x1 cx1 cx2 x2 t) (bezier-point y1 cy1 cy2 y2 t)]))

(def paths
  [[100 50 10 -20 -10 100 100 190]
   [100 190 210 100 190 -20 100 50]])

(defn next-state [[r t p]]
  (let [r (+ r 0.015)
        t (+ t 0.01),
        [t p] (if (> t 1)
                [(dec t) (- 1 p)]
                [t p])]
    [r t p]))

(defn draw [[r t p]]
  (color-mode PApplet/RGB 255)
  (fill 255 8)
  (rect 0 0 (width) (height))
  (color-mode PApplet/HSB 2.0)
  (fill (inc (sin r)) 2.0 2.0)
  (let [[x y] (bezier-path (paths p) t)]
    (translate (/ (- (width) (height)) 2) 0)
    (scale (/ (height) 200.0))
    (translate 0 -10)
    (ellipse x y 30 30)))

(defonce state (atom [0 0 0]))
(def fresh (atom true))

(defn draw-wrapper []
  (ignore-errors
    (when @fresh
      (setup)
      (reset! fresh false))
    (draw @state)
    (swap! state next-state)))

(defn -main [& args]
  (defapplet heart
    :setup setup
    :draw draw-wrapper
    :size [200 200])
  (run heart))

(comment
  (-main)
  (stop heart)
  )
