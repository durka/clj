(ns org.durka.nytimes.campaign
  (:use org.durka.nytimes.common))

(def *version* "v2")

(def *api-key* (:campaign api-key))

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
  "Lazy seq of the results of searching for donors -- the server returns results in pages of 100, so evluation will occur when the 100n+1'th entry is accessed. Restrict by zip code, last name, and/or first name."
  [type year params]
  (request (concat (prelude type year) ["contributions" "donorsearch"])
           (mapkeys #(.sym %) params)
           *api-key*))
