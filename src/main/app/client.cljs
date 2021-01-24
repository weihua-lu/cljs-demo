(ns app.client
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [clojure.string :as str]
            [datascript.core :as d]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! timeout]]))

(def api-endpoint "https://en.wikipedia.org/w/api.php?format=json&origin=*&action=query&prop=linkshere|links")

(def schema
  {:wiki/title     {:db/unique :db.unique/identity}
   :wiki/link      {:db/cardinality :db.cardinality/many}
   :wiki/linkshere {:db/cardinality :db.cardinality/many}})

(def conn (d/create-conn schema))

(defn extract [type response]
  "extract either :links or :linkshere from api body"
  (let [pages (-> response :body :query :pages)]
    (->> (vals pages)
         (mapcat type)
         (map :title))))

(defn handle-success [title response status]
  (let [[links linkshere] (map #(extract % response) [:links :linkshere])]
    (swap! status assoc :searching false)
    (if (and (empty? linkshere) (empty? links))
      (swap! status assoc
             :error :error/empty-response)
      (d/transact! conn [{:wiki/title     title
                          :wiki/link      links
                          :wiki/linkshere linkshere}]))))

(defn handle-failure [response status]
  (swap! status assoc
         :searching false
         :error :error/bad-request)
  (prn response))

(defn search [status]
  (go (let [title    (str (:search-val @status))
            _        (swap! status assoc :searching true :error nil)
            response (<! (http/get api-endpoint
                                   {:with-credentials? false
                                    :query-params      {:titles title}}))]
        (if (== 200 (:status response))
          (handle-success title response status)
          (handle-failure response status)))))

(defn atom-input [status]
  [:div.control
   [:input.input {:type        "text"
                  :value       (:search-val @status)
                  :on-change   #(swap! status assoc :search-val (-> % .-target .-value))
                  :on-focus    #(swap! status assoc :focus true)

                  :on-blur
                  (fn [_]
                    (go (<! (timeout 200))
                        (swap! status assoc :focus false)))

                  :on-key-down #(case (.-which %)
                                  13 (search status) nil)}]])

(defn lower-includes? [s substr]
  (let [s-low (str/lower-case s)
        substr-low (str/lower-case substr)]
    (str/includes? s-low substr-low)))

(defn search-hint [input]
  ;; hint all possibles from link or linkshere, exclude already searched title
  (when-not (empty? input)
    (d/q '[:find ?term
           :in $ ?include-fn ?input
           :where
           (or [_ :wiki/linkshere ?term]
               [_ :wiki/link ?term])
           [(?include-fn ?term ?input)]

           (not-join [?term]
                     [_ :wiki/title ?term])] @conn lower-includes? input)))

(defn dropdown [status]
  (let [possible-terms (search-hint (:search-val @status))]
    [:div.dropdown
     {:class (if (and (not-empty possible-terms) (:focus @status)) "is-active")}
     [atom-input status]
     [:div.dropdown-menu
      [:div.dropdown-content
       (for [[hint] possible-terms]
         ^{:key hint}
         [:a.dropdown-item
          {:on-click (fn [_]
                       (go
                         (prn "click hint")
                         (swap! status assoc :search-val hint)
                         (<! (search status))
                         (swap! status assoc :search-val "")))}
          hint])]]]))

(defn result-table []
  (let [all-data (d/q '[:find (pull ?e [*])
                        :where [?e :wiki/title ]] @conn)]
    [:div.table-container
     [:table.table.is-striped.is-fullwidth
      [:thead
       [:tr
        (for [col (->> schema keys (map str))]
          ^{:key col} [:th col])]]
      [:tbody
       (for [[{:wiki/keys [title link linkshere]}] all-data]
         ^{:key title}
         [:tr [:td title] [:td (str link)] [:td (str linkshere)]])]]]))

(defn build-relation-map [[title-a title-b relation :as v]]
  {(hash (str title-a title-b))
   {:source title-a
    :target title-b
    :relations [relation]}})

(defn concat-relations [m1 m2]
  (let [r1 (:relations m1)
        r2 (:relations m2)]
    (assoc m1 :relations (concat r1 r2))))

(defn merge-relations [relations]
  (->> relations
       (map build-relation-map)
       (apply merge-with concat-relations)
       vals))

(defn query-relations []
  (let [query  '[:find ?title1 ?title2 ?relation
                 :in $ ?include-fn ?type
                 :where
                 [?e1 :wiki/title ?title1]
                 [?e2 :wiki/title ?title2]
                 [?e2 ?type  ?relation]
                 [(not= ?title1 ?title2)]
                 [(?include-fn ?relation ?title1)]]]

    ;; Query all relations with one title,
    ;; by fuzzy search from all other titles' linkshere(outbound link) and link(inbound link)
    (->> [:wiki/linkshere :wiki/link]
         (mapcat #(d/q query @conn lower-includes? %)))))

(defn relation-table []
  (let [rs      (-> (query-relations) merge-relations)]
    [:ul (for [{:keys [source target relations]} rs]
           ^{:key (str source target)}
           [:div.box
            [:li
             [:p [:strong source]
              " is very likely to be related with "
              [:strong target] " due to terms we found on " [:strong target]
              " ---- "
              (str (into [] relations))]]])]))

(defn searching-bar [status]
  (when (:searching @status)
    [:div.container
     [:p.notification.is-primary "Searching, please wait....."]
     [:progress.progress.is-large.is-info]]))

(defn error-bar [status]
  (when (:error @status)
    [:p.notification.is-danger "Opps, no results found, please retry!"]))

(defn tables [status]
  (when-not (:searching @status)
    [:div
     [:p.title.is-3 "search list.."]
     [result-table]
     [:p.title.is-3 "possible relations..search more to explore!"]
     [relation-table]]))

(defn search-button [status]
  [:div.control
   [:input.button.is-info
    {:type     "button" :value "Search!"
     :on-click #(search status)}]])

(defn input-bar [status]
  [:div.field.has-addons.has-addons-centered
   [dropdown status]
   [search-button status]])

(defn main-body [status]
  [:div
   [searching-bar status]
   [error-bar status]
   [tables status]])

(defn shared-state []
  (let [status     (r/atom {:search-val "" :searching false})]

    ;; init [db] with some sample searches
    (go
      (swap! status assoc :searching true)
      (<! (search (r/atom {:search-val "java"})))
      (<! (search (r/atom {:search-val "clojure"})))
      (<! (search (r/atom {:search-val "lisp"})))
      (<! (search (r/atom {:search-val "programming language"})))
      (swap! status assoc :searching false))

    (fn []
      [:div
       [input-bar status]
       [main-body status]])))


;;;;;;;;;;;;;;;;;
;; entry-point ;;
;;;;;;;;;;;;;;;;;

(defn ^:export run
  []
  (js/console.log "Run start")
  (rd/render [shared-state] (js/document.getElementById "app")))







(comment
  ;; TODO put this in a proper test ns some day..

  ;; init
  (def conn (d/create-conn schema))

  ;; search three terms
  (go
    (<! (search (r/atom {:search-val "java"})))
    (<! (search (r/atom {:search-val "clojure"})))
    (<! (search (r/atom {:search-val "lisp"}))))

  ;; check db
  conn

  ;; query relations of java and clojure
  (query-relations)
;; => (["java" "clojure" "Java (programming language)"] ["java" "clojure" "Java applet"] ["java" "clojure" "Java virtual machine"])

  ;; merge into a map by source target title
  (merge-relations (query-relations))
;; => ({:source "java", :target "clojure", :relations ("Java (programming language)" "Java applet" "Java virtual machine")} {:source "lisp", :target "clojure", :relations ("Common Lisp" "Emacs Lisp" "Lisp (programming language)" "*Lisp" "Allegro Common Lisp")})

  ;; search a new term
  (go
    (<! (search (r/atom {:search-val "Java applet"}))))

  ;; search hint shall give all java hints except "Java applet"
  (search-hint "java")
;; => #{["Talk:Java applet"] ["Java (programming language)"] ["Java virtual machine"] ["2019 Java blackout"]}

  ;; search with empty response alter state to error code
  (def s (r/atom {:search-val "weihua"}))
  (go (<! (search s)))
  (:error @s)
;; => :error/empty-response
  )
