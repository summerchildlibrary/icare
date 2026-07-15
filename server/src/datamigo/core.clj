(ns datamigo.core
  "Entry point for the datamigo sync server. The data model, persistence and REST
   protocol live in datamigo.storage and datamigo.http; this just boots the HTTP
   server. (The previous WebSocket protocol, per-triple LMDB schema and permission
   system have been retired in favour of open reads + signed commits over REST.)"
  (:require [datamigo.http :as http]))

(defn -main [& _args]
  (println "Starting datamigo...")
  (http/start-server!)
  ;; keep the main thread alive; the HttpServer runs on its own executor
  @(promise))
