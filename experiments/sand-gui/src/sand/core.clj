(ns sand.core
  (:require
    [sand.tower :as tower]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as humble]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.ui :as ui]
    [io.github.humbleui.window :as window]
    [clojure.set :as set]
    [nrepl.cmdline :as nrepl]))

(defonce *window (atom nil))

(defonce *tower (atom 
                  {:walls #{}
                   :sand #{}
                   :grain nil
                   :floor false
                   :running false
                   }))

(defn get-height [walls]
  (first (sort > (map second walls))))

(def paints 
  {:bg (paint/fill 0x40e9b96e)
   :wall (paint/fill 0xff888a85)
   :sand (paint/fill 0xff8f5902)
   :grain (paint/fill 0xfff57900)

   })

(def step-1 (partial iterate tower/iterate-1))
(defn step-2 [t]
  (let [next-t (tower/iterate-until-settled-1 t)
        new-sand (set/difference (:sand next-t) (:sand t))
        ]
        (if-let [[sx sy] (first new-sand)]
          (if (and (= 1 (count new-sand)) (or ((:sand t) [(dec sx) (inc sy)])
            ((:sand t) [(inc sx) (inc sy)])))
            (recur next-t)
            next-t)
          next-t)))


(defn paint-canvas [ctx canvas size]

  (canvas/draw-rect canvas (humble/rect-xywh 0 0 (:width size) (:height size)) (:bg paints))

  (when-let [height (get-height (:walls @*tower))]

    (when (:running @*tower)
      (binding [tower/*floating-pos* (if (:floor @*tower) (+ 3 height) height)
                tower/*floor-pos* (when (:floor @*tower) (+ 2 height))]
        ;(swap! *tower tower/iterate-until-settled-1)
        (swap! *tower step-2)
        ;(swap! *tower #(nth (step-1 %) 9))
        ))


    (canvas/translate canvas (/ (:width size) 2) 0)

    (let [scale ( / (:height size) (+ 4 height))]
      (canvas/scale canvas scale scale))

    (canvas/translate canvas -500 0)   ; [0 500] in the center
    (canvas/translate canvas -0.5 0.5) ; adjust to the centers of the boxen

    (doseq [x (:walls @*tower)]
      (canvas/draw-rect canvas (humble/rect-xywh (first x) (second x) 1 1) (:wall paints)))

    (doseq [x (:sand @*tower)]
      (canvas/draw-rect canvas (humble/rect-xywh (first x) (second x) 1 1) (:sand paints)))

    (when-let [x (:grain @*tower)]
      (canvas/draw-rect canvas (humble/rect-xywh (first x) (second x) 1 1) (:grain paints)))

    (when (:floor @*tower)
      (canvas/draw-rect canvas (humble/rect-xywh 0 (+ 2 height) 1000 1) (:wall paints)))

    (window/request-frame (:window ctx))))

(def app
  (ui/column
    (ui/canvas
      {:on-paint paint-canvas})))

(defn load-tower
  [f]
  (swap! *tower (fn [_] 
                  {:walls (if f (tower/parse-file f) #{})
                   :sand #{}
                   :grain nil
                   :running false
                   })))

(defn toggle-run []
  (swap! *tower #(assoc % :running (not (:running %)))) )

(defn toggle-floor []
  (swap! *tower #(assoc % :floor (not (:floor %)))) )

(defn -main
  [& args]

  (when-let [file (first args)] 
    (load-tower file)
    (toggle-run))

  (ui/start-app!
    (reset! *window (ui/window 
                      {:title "Sandtower"}
                      #'app)))
  (apply nrepl/-main args))

(comment
  (load-tower nil)
  (load-tower "/proj/aoc/aoc-2022/resources/2022/day14.test.txt")
  (load-tower "/proj/aoc/aoc-2022/resources/2022/day14.txt")


  (toggle-floor)
  (toggle-run)

  (:walls @*tower)
  (:sand @*tower)
  (-main))


