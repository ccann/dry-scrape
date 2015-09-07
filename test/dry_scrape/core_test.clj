(ns dry-scrape.core-test
  (:require [clojure.test :refer :all]
            [dry-scrape.core :refer :all]
            [hickory.core :refer :all]
            [clj-http.client :as cl]))


(deftest test-get-pbp-urls
  (let [urls (get-pbp-urls "20150204")
        bruins (parse-play-by-play (first urls))]
    (clojure.pprint/pprint urls)
    (is (= (count urls) 3))
    (clojure.pprint/pprint bruins)
    (is bruins)
             ))

