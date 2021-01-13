(ns app.client
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

(defn atom-input [value]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn search [val results]
  (go (let [_        (reset! results {:searching true})
            response (<! (http/get "https://en.wikipedia.org/w/api.php?format=json&origin=*&action=query&prop=linkshere"
                                   {:with-credentials? false
                                    :query-params      {"titles" (str val)}}))]
        (prn (:body response))
        (reset! results response))))

(comment
  (def a (r/atom "linux"))
  (def result (r/atom {}))
  (search a result)
  @result
  )

(defn title-lists [result]
  (when (== 200 (:status @result))
    (let [pages  (-> @result :body :query :pages)
          titles (->> (vals pages)
                      (mapcat :linkshere)
                      (map :title))]
      [:ul
       (for [title titles]
         ^{:key title} [:li title])])))

(defn loading-bar [result]
  (when (:searching @result)
    [:p "Searching, please wait....."]))

(defn search-button [val response]
  [:input {:type "button" :value "Search!"
           :on-click #(search val response)}])

(defn shared-state []
  (let [val (r/atom "")
        result (r/atom {})]
    (fn []
      [:div
       [:p [atom-input val] [search-button @val result]]
       [loading-bar result]
       [title-lists result]])))

(defn ^:export run
  []
  (js/console.log "Run starttt")
  (rd/render [shared-state] (js/document.getElementById "app")))
