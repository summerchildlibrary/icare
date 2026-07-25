(ns vd
  (:require [aztree :as aztree] [negentropy :as negentropy] [clojure.edn :as edn])
  (:import (java.net URI) (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)))
(def ^HttpClient http (HttpClient/newHttpClient))
(defn post-edn [path body]
  (let [req (-> (HttpRequest/newBuilder (URI/create (str "http://167.172.19.91:8080" path)))
                (.POST (HttpRequest$BodyPublishers/ofString (pr-str body))) (.build))]
    (edn/read-string (.body (.send http req (HttpResponse$BodyHandlers/ofString))))))
(defn -main [& [ns-hex]]
  (binding [*print-namespace-maps* false]
    (let [tree (aztree/atree)
          resp (post-edn "/sync" {:namespace ns-hex :ranges (negentropy/initial-ranges tree)})
          {:keys [only-theirs]} (negentropy/process-response tree (:sub-ranges resp))]
      (println "droplet holds" (count only-theirs) "records for" (subs ns-hex 0 12) "...")
      (doseq [[k v] (take 8 only-theirs)] (println "  " k "->" (pr-str v))))))
