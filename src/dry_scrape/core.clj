(ns dry-scrape.core
  (:require [hickory.core :refer :all]
            [hickory.select :as s]
            [clojure.string :as string]
            [clj-http.client :as cl]
            )
  (:gen-class))

(def site-htree )



(defn get-game-events
  [game-id]
  (let [site-htree (-> (cl/get (str "http://espn.go.com/nhl/boxscore?gameId=" game-id)) :body parse as-hickory)
        elems (s/select (s/descendant (s/id "my-players-table")
                                      (s/and (s/tag :div) (s/nth-child 4))
                                      (s/and (s/tag :div) (s/class "mod-content"))
                                      (s/and (s/tag :td)
                                             (s/nth-child 3))) site-htree)]
    (->> elems
         (map :content)
         ;; (map #([(first %) (first (:content (nth % 2)))]))
         (flatten)
         (filter #(not (and (coll? %) (= (:tag %) :br))))
         (map #(if (coll? %) (first (:content %)) (string/trim %)))
         (vec)
         )))

(defn parse-by-date
  [date]
  (let [url (str "http://espn.go.com/nhl/scoreboard?date=" date)
        site-htree (-> (cl/get url) :body parse as-hickory)
        elems (s/select (s/descendant (s/class "span-4")
                                      (s/or (s/id :gamesLeft)
                                            (s/id :gamesRight))
                                      (s/class "expand-gameLinks")
                                      (s/tag :a)) site-htree) ]
    (->> elems
         (filter #(= (:content %) ["Play‑By‑Play"]))
         (map #(:href (:attrs %)))
         (map #(str "http://espn.go.com" %))
         ))
    
    )

(defn parse-play-by-play
  [url]
  (let [site-htree (-> (cl/get url) :body parse as-hickory)
        elems (s/select (s/descendant (s/class "story-container")
                                      (s/and (s/tag :div) (s/nth-child 4))
                                      (s/and (s/tag :div) (s/class "mod-content"))
                                      (s/and (s/tag :td)
                                             (s/nth-child 3))) site-htree)]
    (->> elems
         (map :content)
         (map #(if (coll? (first %))
                 (first (:content (first %)))
                 (first %)))
         (map string/trim))))

(defn point? [s] (re-find #"goal scored" (string/lower-case s)))
(defn assist? [s])


(defn get-points
  [coll]
  (filter point? coll))

(defn format-game-events
  [events]
  (let [point? #(re-find #"([0-9]+)" %)
        assists? #(re-find #"Assists" %)]
    nil
    ))


    


(defn -main
  [& args]
  (let [urls (parse-by-date (first args))]
    (clojure.pprint/pprint (map parse-play-by-play urls))))


  ;; (clojure.pprint/pprint (get-points (parse-play-by-play (first args)))))




