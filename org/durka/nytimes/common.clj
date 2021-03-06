(ns org.durka.nytimes.common
;  (:use clj-http-client.core)
  (:use clojure.contrib.seq-utils)
  (:use clojure.contrib.duck-streams)
  (:use clojure.contrib.str-utils)
  (:require (org.danlarkin [json :as json])))

(def api-key {:campaign   "e19c3c51ad52a90dcb3da6ae991e318e:18:57691504"
              :movies     "8e8ed7dc4e55e1fa79a05aa458f04962:10:57691504"
              :community  "c2ztv3hpxc6d4eyxe6tbruqz"
              :congress   "2c7c0acde7e0ceac62793c728cb2aa89:9:57691504"
              :tags       "89dde161728357343a36ff5f42785b1e:17:57691504"})

(defn request
  [dirs params kee]
  (with-open [nyt (reader
                    (apply str (flatten
                                 ["http://api.nytimes.com/"
                                  (interpose "/" dirs)
                                  ".json?"
                                  (interpose "&"
                                             (map #(str (key %) "=" (val %))
                                                  (conj {'api-key kee} params)))])))]
    (let [data (json/decode-from-reader nyt)]
      (if (= "OK" (:status data))
        (with-meta (:results data) {:copyright (:copyright data)})
        ;#^{:copyright (:copyright data)} (:results data)
        (throw (Exception. (str "New York Times returned status " (:status data)
                                ": " (str-join " " (:errors data)))))))))
