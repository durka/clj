(ns org.durka.nytimes.campaign
  (:use org.durka.nytimes.common)
  (:use clojure.contrib.seq-utils)
  (:import [java.io FileNotFoundException]))

(def *version* "v2")

(def *api-key* (:campaign api-key))

(defn mapmap
  "Map with separate fns for the keys vals of a map(s). Colls must be maps."
  [kf vf & colls]
  (zipmap (apply map kf (map keys colls))
          (apply map vf (map vals colls))))

(defn prelude
  [type year]
  ["svc" "elections" "us" *version* type year "finances"])

(defn summaries
  "Get financial summaries for all candidates in a particular election."
  [type year]
  (request (concat (prelude type year) ["candidates"])
           {}
           *api-key*))

(defn details
  "Get details for a specific candidate in a given election. candidate can be a string (last name) or ID."
  [type year candidate]
  (request (concat (prelude type year) ["candidates" candidate])
           {}
           *api-key*))

(defn by-state
  "Total donations to each candidate from a state (identified by postal code)."
  [type year state]
  (request (concat (prelude type year) ["states" state])
           {}
           *api-key*))

(defn by-zip
  "Total donations to each candidate from a ZIP code (5-digit)."
  [type year zip]
  (request (concat (prelude type year) ["zips" zip])
           {}
           *api-key*))

(defn donor-search
  "Lazy seq of the results of searching for donors -- the server returns results in pages of 100 (but a flattened seq is returned by this function), so evaluation will occur when the 100n+1'th entry is accessed. Restrict by at least one of zip code, last name, first name (in a map like {:lname \"smith\" :fname \"john\" :zip \"11111\"}). If no results, returns nil. If you don't care about the first 100n entries, then pass n as the start-page. Then the 100n+1'th entry will be the first element in the returned seq."
  ([type year params] (donor-search type year params 0))
  ([type year params start-page]
   (let [at (fn at [offset]
              (try
                (let [page (request (concat (prelude type year) ["contributions" "donorsearch"])
                                    (mapmap #(.sym %) identity ;; strip the leading : from map keys
                                            (assoc params :offset (* 100 offset)))
                                    *api-key*)]
                  (lazy-cat page (at (inc offset)))) ;;when there are no more pages, the call to request within this call to at will die with an FNFE, preventing lazy-cat from being called any more
                (catch FileNotFoundException fnfe
                  nil)))]
     (at start-page))))

