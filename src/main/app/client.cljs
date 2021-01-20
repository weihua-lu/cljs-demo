(ns app.client
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [clojure.string :as str :refer [includes?]]
            [datascript.core :as d]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

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
    (reset! status {:searching false})
    (d/transact! conn [{:wiki/title     title
                        :wiki/link      links
                        :wiki/linkshere linkshere}])))

(defn handle-failure [response status]
  (reset! status {:error (:status response)})
  (prn response))

(defn search [title status]
  (go (let [_        (reset! status {:searching true})
            response (<! (http/get api-endpoint
                                   {:with-credentials? false
                                    :query-params      {:titles (str title)}}))]
        (if (== 200 (:status response))
          (handle-success title response status)
          (handle-failure response status)))))

(defn atom-input [value status]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))
           :on-key-down #(case (.-which %) 13 (search @value status) nil)}])

(defn title-lists [result]
  (let [pages  (-> @result :body :query :pages)
        titles [:a :b :c]]
    [:ul
     (for [title titles]
       ^{:key title} [:li title])]))

(defn result-table []
  (let [all-data (d/q '[:find (pull ?e [*])
                        :where [?e :wiki/title ]] @conn)]
    [:table
     [:tr
      (for [col (->> schema keys (map str))] [:td col])]
     (for [[{:wiki/keys [title link linkshere]}] all-data]
       [:tr [:td (str title)] [:td (str link)] [:td (str linkshere)]])]))

(defn build-relation-map [[title-a title-b relation :as v]]
  (assoc {} (hash (str title-a title-b))
         {:source title-a
          :target title-b
          :relations [relation]}))

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
                 :in $ ?type
                 :where
                 [?e1 :wiki/title ?title1]
                 [?e2 :wiki/title ?title2]
                 [?e2 ?type  ?relation]
                 [(not= ?title1 ?title2)]

                 ;; case insensitive regex match
                 [(str "(?i)" ?title1) ?patternstr]
                 [(re-pattern ?patternstr) ?pattern]
                 [(re-find ?pattern ?relation)]]]

    ;; Query all relations with one title,
    ;; by fuzzy search from all other titles' linkshere(outbound link) and link(inbound link)
    (->> [:wiki/linkshere :wiki/link]
         (mapcat #(d/q query @conn %)))))

(defn relation-table []
  (let [relations (-> (query-relations)
                      merge-relations)
        message   "~s is very likely to be related with ~s due to terms we found on ~s --- ~s"]
    [:ul (for [{:keys [source target relations]} relations]
           [:li (cljs.pprint/cl-format nil message source target target relations)])]))

(defn status-bar [status]
  (cond
    (:searching @status)
    [:p "Searching, please wait....."]

    (:error @status)
    [:p "Opps, something went wrong, please retry!"]

    :else
    [:div
     [:p "search list.."]
     [result-table]
     [relation-table]]))

(defn search-button [val response]
  [:input {:type "button" :value "Search!"
           :on-click #(search val response)}])

(defn shared-state []
  (let [search-val (r/atom "")
        status     (r/atom {})]

    ;; init with some sample searches
    (search "java" status)
    (search "clojure" status)
    (search "lisp" status)

    (fn []
      [:div
       [:p [atom-input search-val status] [search-button @search-val status]]
       [status-bar status]])))

(defn ^:export run
  []
  (js/console.log "Run starttt")
  (rd/render [shared-state] (js/document.getElementById "app")))

(comment
  ;; TODO put this in a proper test ns some day..

  ;; init
  (def conn (d/create-conn schema))
  (def status (r/atom {}))

  ;; search two terms
  (search "java" status)
  (search "clojure" status)
  (search "lisp" status)

  ;; query relations of java and clojure
  (query-relations)
;; => (["java" "clojure" "Java (programming language)"] ["java" "clojure" "Java applet"] ["java" "clojure" "Java virtual machine"])

  ;; merge into a map by source target title
  (merge-relations (query-relations))
;; => ({:source "java", :target "clojure", :relations ("Java (programming language)" "Java applet" "Java virtual machine")} {:source "lisp", :target "clojure", :relations ("Common Lisp" "Emacs Lisp" "Lisp (programming language)" "*Lisp" "Allegro Common Lisp")})
  )
