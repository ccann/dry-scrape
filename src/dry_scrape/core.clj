(ns dry-scrape.core
  (:require [hickory.core :refer :all]
            [hickory.select :as s]
            [clojure.string :as string]
            [clj-http.client :as cl]
            )
  (:gen-class))



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

(defn format-game-events
  [events]
  (let [goal? #(re-find #"([0-9]+)" %)
        assists? #(re-find #"Assists" %)]
    nil
    ))
    


(defn -main
  [& args]
  (clojure.pprint/pprint (get-game-events (first args))))



