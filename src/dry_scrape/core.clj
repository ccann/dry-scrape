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

(defn get-pbp-urls
  "Return a coll of play-by-play URLs for this date."
  [date]
  (let [url (str "http://espn.go.com/nhl/scoreboard?date=" date)
        site-htree (-> (cl/get url) :body parse as-hickory)
        elems (s/select (s/descendant (s/class "span-4")
                                      (s/or (s/id :gamesLeft)
                                            (s/id :gamesRight))
                                      (s/class "expand-gameLinks")
                                      (s/tag :a)) site-htree)]
    (->> elems
         (filter #(= (:content %) ["Play‑By‑Play"]))
         (map #(:href (:attrs %)))
         (map #(str "http://espn.go.com" % "&period=0"))
         ))
    
    )

(defn parse-play-by-play
  [url]
  (let [htree (-> (cl/get url) :body parse as-hickory)
        nodes (s/select (s/descendant (s/class "story-container")
                                      (s/and (s/tag :div) (s/nth-child 4))
                                      (s/and (s/tag :div) (s/class "mod-content"))
                                      (s/and (s/tag :td)
                                             (s/nth-child 3))) htree)]
    (->> nodes
         (map :content)
         (map #(if (coll? (first %))
                 (first (:content (first %)))
                 (first %)))
         (map string/trim))))

(defn point? [s] (re-find #"goal scored" (string/lower-case s)))
(defn assist? [s])
(defn sog? [s] (re-find #"shot on goal" (string/lower-case s)))

(declare parse-sogs-and-saves)

{"shot-on-goal" parse-sogs-and-saves
 "goal scored" nil
 } 
    






    


(defn -main
  [& args]
  (let [urls (get-pbp-urls (first args))]
    (clojure.pprint/pprint (map parse-play-by-play urls))))


  ;; (clojure.pprint/pprint (get-points (parse-play-by-play (first args)))))




