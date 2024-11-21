(ns examples.hoplon
  (:require
    [hoplon.core :refer [defelem] :as h]
    [hoplon.goog]
    [javelin.core :as j]))

(defonce counter (j/cell 1))
(defonce counter-by-42 (j/cell= (* counter 42)))

(defelem buttons
  [{monitored-counter ::counter
    monitored-counter-by-42 ::counter-by-42 :as attrs} _]
  (h/div
    (h/div :css {:width "50%"}
           (h/button :click #(swap! monitored-counter inc)
                     :css {:color "cyan"
                           :display "block"
                           :width "100%"
                           :background-color "black"}
                     "Click me to increase!")
           (h/button :mouseover #(swap! monitored-counter dec)
                     :css {:color "orange"
                           :display "block"
                           :width "100%"
                           :background-color "gray"}
                     "Go over me to decrease!"))
    (h/br)
    (h/text "counter=~{monitored-counter}")
    (h/br)
    (h/text "counter-by-42=~{monitored-counter-by-42}")))

(defonce TICK 1000)

(defelem timer [attrs _]
  (let [start   (or (:start attrs) 0)
        seconds (j/cell start)]
    (.setInterval js/window #(swap! seconds inc) TICK)
    (h/div attrs (j/cell= (str "Seconds Elapsed: " seconds)))))

(defelem timers
  [_ _]
  ;; our first timer will start at 0 and count up
  (h/p (timer :style "color: green;" :start 0))
  ;; start 3 more timers, each with different starting values
  (apply h/ol (map h/li (for [r (range 1 4)] (timer :start r)))))

(defelem notification
  [{monitored-counter ::counter :as attrs} _]
  (let [flag (j/cell false)
        timer (js/window.setInterval #(swap! flag not) (* 3 TICK))]
    (h/dialog :attr (j/cell= {:open flag})
              (h/text "The counter is ~{monitored-counter}"))))

(defelem home []
  (h/div
    :id "app"
    (h/h3 "Welcome to Hoplon!")
    (buttons ::counter counter
             ::counter-by-42 counter-by-42)
    (timers)
    (notification ::counter counter)))

(defn ^:dev/after-load init! []
  (let [app (.getElementById js/document "app")
        parent (.-parentElement app)]
    (.replaceChild parent (home) app)))

(defn ^{:export true} main []
  (init!))

(comment

  (h/dialog "42")

  )
