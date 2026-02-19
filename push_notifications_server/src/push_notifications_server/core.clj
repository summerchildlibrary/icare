(ns push-notifications-server.core
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clj-http.client :as http])
  (:import (fr.acinq.secp256k1 Secp256k1)
           (com.google.auth.oauth2 GoogleCredentials)
           (org.java_websocket.client WebSocketClient)
           (org.java_websocket.handshake ServerHandshake)
           (java.net URI URL HttpURLConnection)
           (java.io FileInputStream)
           (java.security MessageDigest)
           (java.util HexFormat)))


;; --- Crypto utilities ---
(defonce ^Secp256k1 secp256k1 (Secp256k1/get))
(defonce digester (MessageDigest/getInstance "SHA-256"))
(defonce hex-format (HexFormat/of))

(defn hex-decode
  "Decode a hex string to bytes"
  ^bytes [^String data]
  (.parseHex hex-format data))

(defn hex-encode
  "Encode bytes to hex string"
  ^String [^bytes data]
  (.formatHex hex-format data))

(defn sha256
  "Compute SHA256 hash of a string, returns bytes"
  ^bytes [^String data]
  (.digest digester (.getBytes data "UTF-8")))

(defn verify-schnorr-signature
  "Verify a Schnorr signature. Returns true if valid."
  [^bytes public-key ^bytes message ^bytes signature]
  (try
    (.verifySchnorr secp256k1 signature message public-key)
    (catch Exception _
      false)))

(defn compute-event-id
  [{:keys [pubkey created_at kind tags content]}]
  (try
    (let [canonical [0
                     (str/lower-case pubkey)      ;; lowercase hex string
                     (long created_at)             ;; number
                     (long kind)                   ;; number
                     (vec (map vec tags))          ;; array of arrays of strings
                     (str content)]                ;; string
          serialized (json/write-str canonical)]
      (hex-encode (sha256 serialized)))
    (catch Exception e
      nil)))

(defn verify-nostr-event
  [{:keys [id pubkey sig] :as event}]
  (when-let [computed-id (compute-event-id event)]
    (when (= computed-id id)
      (let [id-bytes (hex-decode id)
            pubkey-bytes (hex-decode pubkey)
            sig-bytes (hex-decode sig)]
        (when (verify-schnorr-signature pubkey-bytes id-bytes sig-bytes)
          event)))))

;; --- Push notification logic ---

;;TODO - define a clear schema for the content of fulfill-notify events and receipts and keep it in a shared place
(def fulfill-notify-kind 29508)
(def receipt-kind 39507)

(def project-id "do-do-online")
(def fcm-url (str "https://fcm.googleapis.com/v1/projects/" project-id "/messages:send"))
(def service-account-path "do-do-online-firebase-adminsdk-fbsvc-809d342389.json")
(def fcm-scope "https://www.googleapis.com/auth/firebase.messaging")

(defn get-access-token []
  (-> (GoogleCredentials/fromStream (FileInputStream. service-account-path))
      (.createScoped [fcm-scope])
      (doto .refresh)
      (.getAccessToken)
      (.getTokenValue)))

(defn send-push-notification! [{:keys [title body device-token]}]
  (http/post fcm-url
             {:headers {"Authorization" (str "Bearer " (get-access-token))}
              :content-type :json
              :body (json/write-str
                     {:message
                      {:token device-token
                       :notification
                       {:title title
                        :body body}}})}))

(comment
 (send-push-notification!
  {:title "Test Notification"
   :body "This is a test push notification from the server."
   :device-token "dc_FPzorRRK4m505ixKImx:APA91bECDruMHcHZzaw4xAmEVWRaivnjugQRTozIQMbN2uCIJ6nulHIUMM3vbObaXTNkjzejkYmpONQAs59zM-mcMvTDjLju19eFW2NpD53T8BIDLrDr43o"}))

(defonce ws-client (atom nil))

(defn create-websocket-client
  "Create a WebSocket client that connects to the relay and handles events"
  [url on-message on-open on-close on-error]
  (proxy [WebSocketClient] [(URI. url)]
    (onOpen [^ServerHandshake handshake]
      (println "WebSocket connected to" url)
      (when on-open (on-open this)))
    (onMessage [^String message]
      (when on-message (on-message this message)))
    (onClose [code reason remote]
      (println "WebSocket closed:" reason)
      (when on-close (on-close this code reason remote)))
    (onError [^Exception ex]
      (println "WebSocket error:" (.getMessage ex))
      (when on-error (on-error this ex)))))

(defn subscribe-to-fulfill-notify-events!
  "Send a REQ to subscribe to fulfill-notify events (kind 29508)"
  [^WebSocketClient client]
  (let [subscription-id "fulfill-notify-sub"
        req-message (json/write-str ["REQ" subscription-id {"kinds" [fulfill-notify-kind]}])]
    (println "Subscribing to fulfill-notify events...")
    (.send client req-message)))

(defn verify-and-read-fulfill
  [event expected-kind]
  (when (= expected-kind (:kind event))
    (when-let [verified (verify-nostr-event event)]
      (when-let [content (try
                           (clojure.edn/read-string (:content verified))
                           (catch Exception _
                             nil))]
        {:fulfill (assoc event :content content)}))))

(defn verify-and-read-receipt
  [{:keys [fulfill] :as ctx} expected-kind]
  (let [{:keys [notify-receipt]} (:content fulfill)]
    (when-let [parsed-receipt (try
                                (json/read-str notify-receipt :key-fn keyword)
                                (catch Exception _
                                  nil))]
      (when (= expected-kind (:kind parsed-receipt))
        (when-let [verified-receipt (verify-nostr-event parsed-receipt)]
          (when-let [receipt-content (try
                                       (clojure.edn/read-string (:content verified-receipt))
                                       (catch Exception _
                                         nil))]
            (assoc ctx
                   :receipt (assoc verified-receipt :content receipt-content))))))))

(defn verify-pubkeys-match
  [{:keys [fulfill receipt] :as ctx}]
  (let [fulfill-pubkey (:pubkey fulfill)
        fulfill-to-pubkey (get-in fulfill [:content :to-pubkey])
        receipt-pubkey (:pubkey receipt)
        receipt-to-pubkey (get-in receipt [:content :to-pubkey])]
    (when (and (= fulfill-pubkey receipt-to-pubkey)
               (= receipt-pubkey fulfill-to-pubkey))
      ctx)))

(defn handle-fulfill-notify-event
  "Process a fulfill-notify event through verification pipeline"
  [event]
  (some-> event
          (verify-and-read-fulfill fulfill-notify-kind)
          (verify-and-read-receipt receipt-kind)
          verify-pubkeys-match
          :fulfill
          (get-in [:content :notification-content])
          send-push-notification!))

(defn handle-websocket-message
  "Handle incoming WebSocket message from relay"
  [client message]
  (try
    (let [parsed (json/read-str message :key-fn keyword)
          msg-type (first parsed)]
      (case msg-type
        "EVENT" (handle-fulfill-notify-event (nth parsed 2))
        "OK" (println "Relay OK:" (nth parsed 1) (nth parsed 2))
        "EOSE" (println "End of stored events for subscription:" (nth parsed 1))
        "NOTICE" (println "Relay notice:" (nth parsed 1))
        (println "Unknown message type:" msg-type)))
    (catch Exception e
      (println "Error handling message:" (.getMessage e)))))

;; --- Main entry point ---

(def relay-url "ws://localhost:8080")

(defn start-server!
  "Start the push notification server - connects to relay and listens for fulfill-notify events"
  []
  (let [client (create-websocket-client
                relay-url
                handle-websocket-message
                (fn [c] (subscribe-to-fulfill-notify-events! c))
                nil
                nil)]
    (reset! ws-client client)
    (.connect client)
    (println "Push notification server started, connecting to" relay-url)
    client))

(defn -main
  [& args]
  (println "Starting push notification server...")
  (start-server!))
