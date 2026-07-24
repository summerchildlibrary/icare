(ns verify
  (:require [aztree :as aztree] [negentropy :as negentropy] [clojure.edn :as edn])
  (:import (java.net URI) (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)))
(def ^HttpClient http (HttpClient/newHttpClient))
(defn post-edn [path body]
  (let [req (-> (HttpRequest/newBuilder (URI/create (str "http://localhost:8080" path)))
                (.POST (HttpRequest$BodyPublishers/ofString (pr-str body))) (.build))]
    (edn/read-string (.body (.send http req (HttpResponse$BodyHandlers/ofString))))))
(defn -main [& [ns-hex]]
  (binding [*print-namespace-maps* false]
    ;; Empty tree as initiator: every sub-range the server returns is data it has that we lack.
    (let [tree (aztree/atree)
          resp (post-edn "/sync" {:namespace ns-hex :ranges (negentropy/initial-ranges tree)})
          {:keys [only-theirs]} (negentropy/process-response tree (:sub-ranges resp))]
      (println "server holds" (count only-theirs) "records for namespace" ns-hex)
      (doseq [[k v] only-theirs] (println "  " k "->" v)))))
