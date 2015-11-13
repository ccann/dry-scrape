(ns dry-scrape.core
  (:require [hickory.core :refer :all]
            [hickory.select :as s]
            [clojure.string :as str]
            [clj-http.client :as cl])
  (:gen-class))

(defn- line-break? [m] (and (coll? m) (= (:tag m) :br)))

(defn- get-htree [url] (-> url cl/get :body parse as-hickory))

(defn- collect-content
  "Take a vec of event-frags and return a vec of 1 or 2 strings."
  [coll]
  (if (= 1 (count coll))
    coll
    [(first coll) (first (:content (second coll)))]))
                 

(defn parse-game-event-fragments
  "Return a list of event-vector fragments for this game."
  [game-id]
  (let [url (str "http://espn.go.com/nhl/boxscore?gameId=" game-id)
        site-htree (get-htree url)
        elems (s/select (s/descendant (s/id "my-players-table")
                                      (s/and (s/tag :div) (s/nth-child 4))
                                      (s/and (s/tag :div) (s/class "mod-content"))
                                      (s/and (s/tag :td)
                                             (s/nth-child 3))) site-htree)]
    (->> elems
         (map :content)
         (map #(remove line-break? %))
         (map collect-content)
         (map #(map str/trim %)))))


(defn get-pbp-urls
  "Return a list of play-by-play URLs for this date string: yyyymmdd."
  [date]
  (let [url (str "http://espn.go.com/nhl/scoreboard?date=" date)
        site-htree (-> (cl/get url) :body parse as-hickory)
        elems (s/select (s/descendant (s/class "span-4")
                                      (s/or (s/id :gamesLeft)
                                            (s/id :gamesRight))
                                      (s/class "expand-gameLinks")
                                      (s/tag :a))
                        site-htree)]
    (->> elems
         (filter #(= (:content %) ["Play‑By‑Play"]))
         (map #(-> % :attrs :href))
         (map #(str "http://espn.go.com" % "&period=0")))))

(defn get-play-by-play
  [url]
  (let [htree (get-htree url)
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
         (map str/trim)
         (map str/lower-case))))

;; (clojure.pprint/pprint (map get-play-by-play (get-pbp-urls "20151109")))

(defn- goal
  "Parse input and return a goal event-map."
  [s]
  (let [assists (re-find #"(?<=assisted by )[a-z- ]+" s)
        [a1 a2] (when assists (str/split assists #" and "))
        goal-scorer (re-find #"(?<=goal scored\s{1,2}by )[a-z- ]+(?=\()" s)
        strength (cond
                   (.contains s "power play goal") :power-play
                   (.contains s "shorthanded goal") :shorthanded
                   :else :even)
        shot-type (re-find #"[a-z0-9- ]+(?=\))" s)]
    {:event :goal
     :goal goal-scorer
     :strength strength
     :shot shot-type
     :primary-assist a1
     :secondary-assist a2}))

;; (goal "goal scored by ryan kesler(tip-in 18 ft) assisted by sami vatanen and clayton stoner")
;; (goal "power play goal scored  by anthony duclair(slapshot 34 ft) assisted by oliver ekman-larsson and martin hanzal")

(defn- pop-strength-prefix
  [s]
  (let [[pp sh] ["power play - " "shorthanded - "]
        strength (cond
                   (.contains s pp) :power-play
                   (.contains s sh) :shorthanded
                   :else :even)
        s (case strength
            :power-play (str/replace s pp "")
            :shorthanded (str/replace s sh "")
            s)]
    [strength (str/trim s)]))

;; (pop-strength-prefix "shot on goal by mike santorelli saved by anders lindback(snap 42 ft)")
;; (pop-strength-prefix "power play - shot on goal by mikkel boedker saved by frederik andersen(slapshot 42 ft)")

(defn- sog
  "Parse input and return a shot-on-goal event-map."
  [s]
  (let [[strength s] (pop-strength-prefix s)
        shooter (re-find #"(?<=shot on goal by )[a-z- ]+(?= saved)" s)
        goalie (re-find #"(?<=saved by )[a-z- ]+(?=\()" s)]
    {:event :sog
     :strength strength
     :shot shooter
     :save goalie}))

;; (sog "shot on goal by mike santorelli saved by anders lindback(snap 42 ft)")
;; (sog "power play - shot on goal by mikkel boedker saved by frederik andersen(slapshot 42 ft)")

(defn- hit
  "Parse input and return a hit event-map"
  [s]
  (let [[strength s] (pop-strength-prefix s)
        hitter (re-find #"[a-z- ]+(?= credited with hit)" s)
        hittee (re-find #"(?<=with hit on  )[a-z- ]+(?= in)" s)]
    {:event :hit
     :strength strength
     :hit hitter
     :target hittee}))


;; (hit "josh manson credited with hit on  jordan martinook in defensive zone")
;; (hit "shorthanded - josh manson credited with hit on  tobias rieder in defensive zone")

(defn- faceoff
  "Parse input and return a faceoff event-map"
  [s]
  (let [[strength s] (pop-strength-prefix s)
        winner (re-find #"[a-z- ]+(?= won faceoff against)" s)
        loser (re-find #"(?<= won faceoff against )[a-z- ]+(?= in )" s)]
    {:event :faceoff
     :strength strength
     :win winner
     :loss loser}))

;; (faceoff "boyd gordon won faceoff against ryan getzlaf in neutral zone")
;; (faceoff "power play - martin hanzal won faceoff against ryan kesler in offensive zone")

(defn- turnover
  "Parse input and return a turnover event-map."
  [kind s]
  (let [pattern (re-pattern (str "(?<=" (name kind) "  by )[a-z- ]+(?= in )" ))
        [strength s] (pop-strength-prefix s)
        player (re-find pattern s)]
    {:event :turnover
     :strength strength
     kind player}))

;; (turnover :giveaway "giveaway  by patrick maroon in defensive zone")
;; (turnover :takeaway "takeaway  by shawn horcoff in defensive zone")



(defn parse-event
  "Parse input with all available parsers and return a vector of event-maps."
  [s]
  (let [mapping {"goal scored" goal
                 "shot on goal" sog
                 "credited with hit" hit
                 "won faceoff against" faceoff
                 "takeaway  by" (partial turnover :takeaway)
                 "giveaway  by" (partial turnover :giveaway)}
        apropos-parse (fn [[substring parser]]
                          (try
                            (when (.contains s substring)
                              (parser s))
                            (catch Exception e nil)))]
    (->> mapping
         (map apropos-parse)
         (remove nil?))))

;; (parse-event "goal scored by ryan kesler(tip-in 18 ft) assisted by sami vatanen and clayton stoner")

(defn parse-events
  "Take a date string yyyymmdd and return a vector of event-maps aggregated over all that date's
  games."
  [date]
  (->> date
       (get-pbp-urls)
       (map get-play-by-play)
       (mapcat #(map parse-event %))
       (map not-empty)
       (remove nil?)))

    
         

;; (clojure.pprint/pprint (parse-events "20151110"))





    


(defn -main
  [& args]
  (let [urls (get-pbp-urls (first args))]
    (clojure.pprint/pprint (map parse-play-by-play urls))))


  ;; (clojure.pprint/pprint (get-points (parse-play-by-play (first args)))))




