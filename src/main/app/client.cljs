(ns app.client
  (:require [reagent.core :as r]
            [reagent.dom :as rd]))

(defn some-component []
  [:div
   [:h3 "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red"]
        " text."]])

(defn ^:export run
  []
  (js/console.log "Run starttt")
  (rd/render [some-component] (js/document.getElementById "app")))
