(ns datamigo.core
  "Entry point for the datamigo server: sync plus push notifications, one
   backend. The data model, persistence and REST protocol live in
   datamigo.storage and datamigo.http, FCM delivery in datamigo.push; this just
   boots the HTTP server. (The previous WebSocket protocol, per-triple LMDB
   schema, permission system and separate Nostr-relay push service have been
   retired in favour of open reads + signed commits over REST.)"
  (:require [datamigo.http :as http]
            [datamigo.push :as push])
  (:import (java.net InetSocketAddress)
           (java.util.concurrent Executors)
           (com.sun.net.httpserver HttpServer)))

(def port 8080)
(def thread-pool-size 16)

(defn -main [& _]
  (println "Starting datamigo...")
  (let [server (HttpServer/create (InetSocketAddress. port) 0)]
    (.setExecutor server (Executors/newFixedThreadPool thread-pool-size))
    (.createContext server "/ping" (http/handler http/handle-ping))
    (.createContext server "/sync" (http/handler http/handle-sync))
    (.createContext server "/commit" (http/handler http/handle-commit))
    (.createContext server "/watch" (http/handler http/handle-watch))
    (.start server)
    (println (str "datamigo listening on :" port))
    (println (if (push/available?)
               "push notifications enabled"
               (str "push notifications disabled (no " push/service-account-path ")"))))

  @(promise))
