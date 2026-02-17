(ns push-notifications-server.core
  (:require [clojure.data.json :as json]
            [clojure.string :as str])
  (:import (fr.acinq.secp256k1 Secp256k1)
           (org.java_websocket.client WebSocketClient)
           (org.java_websocket.handshake ServerHandshake)
           (java.net URI)
           (java.security MessageDigest)
           (java.util HexFormat)))

(def fulfill-notify-kind 29508)
(def relay-url "ws://localhost:8080")

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

(defn send-push-notification!
  "Send a push notification to a device"
  [device-token title body data])
  ;; TODO: send via FCM/APNs

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

(defn verify-event-kind
  "Verify event is of expected kind. Returns event or nil."
  [event expected-kind]
  (when (= expected-kind (:kind event))
    event))

(defn verify-and-extract-content
  "Verify event and extract parsed content. Returns {:event ... :content ...} or nil."
  [event]
  (when-let [verified (verify-nostr-event event)]
    (when-let [content (try
                         (clojure.edn/read-string (:content verified))
                         (catch Exception _
                           nil))]
      {:event verified :content content})))

(defn verify-and-extract-receipt
  [{:keys [event content] :as ctx}]
  (let [{:keys [notify-receipt notification-content]} content]
    (prn notify-receipt)
    (when-let [verified-receipt (verify-nostr-event notify-receipt)]
      (when-let [receipt-content (try
                                   (clojure.edn/read-string (:content verified-receipt))
                                   (catch Exception _
                                     nil))]
        (assoc ctx
               :verified-receipt verified-receipt
               :verified-receipt-content receipt-content
               :notification-content notification-content)))))

(defn extract-notification-data
  [{:keys [verified-receipt notification-content verified-receipt-content] :as ctx}]
  (let [recipient-pubkey (get verified-receipt "pubkey")
        device-token (:device-token verified-receipt-content)
        {:keys [task-title task-uuid]} notification-content]
    (when (and device-token task-title)
      {:device-token device-token
       :recipient-pubkey recipient-pubkey
       :task-title task-title
       :task-uuid task-uuid})))

(defn handle-fulfill-notify-event
  "Process a fulfill-notify event through verification pipeline"
  [event]
  (some-> event
          (verify-event-kind fulfill-notify-kind)
          verify-and-extract-content
          verify-and-extract-receipt
          extract-notification-data
          (as-> data
                (do (println "Sending notification to" (:recipient-pubkey data) "for task:" (:task-title data))
                    (send-push-notification! (:device-token data)
                                             "Task Completed!"
                                             (:task-title data)
                                             {:task-uuid (:task-uuid data)})))))

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
