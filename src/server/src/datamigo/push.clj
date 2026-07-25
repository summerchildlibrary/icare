(ns datamigo.push
  "Push notifications over FCM, folded into the sync server.

   The server stays deliberately dumb: it knows exactly one app-level rule —
   when a commit flips a task's :task/completed from falsy to true, whoever
   registered interest in that task entity gets a push.

   Interest is registered on /watch, which already carries the caller's friend
   list, and is held per namespace. The commit path costs a single hash lookup
   when nobody is subscribed to that namespace, which is the common case.

   Finding the previous version of an entity needs no scanning: a transact moves
   an entity to a fresh timestamp, so the same commit that puts the new version
   deletes the old one. The diff's :deletes are therefore direct pointers to the
   prior state, and a bulk first sync (which has no deletes) stays quiet on its
   own."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clj-http.client :as http])
  (:import (com.google.auth.oauth2 GoogleCredentials)
           (java.io File FileInputStream)
           (java.util.concurrent Executors ExecutorService)))

;; ── FCM ───────────────────────────────────────────────────────────────────────

(def project-id "do-do-online")
(def fcm-url (str "https://fcm.googleapis.com/v1/projects/" project-id "/messages:send"))
(def service-account-path "do-do-online-firebase-adminsdk-fbsvc-809d342389.json")
(def fcm-scope "https://www.googleapis.com/auth/firebase.messaging")

(defn available?
  "Push is optional: without the service account file the sync server still runs,
   it just never sends anything."
  []
  (.exists (File. service-account-path)))

(defonce ^:private credentials
  (delay (-> (GoogleCredentials/fromStream (FileInputStream. service-account-path))
             (.createScoped [fcm-scope]))))

(defn- access-token []
  (let [^GoogleCredentials creds @credentials]
    (.refreshIfExpired creds)
    (.getTokenValue (.getAccessToken creds))))

(defn send-push-notification! [{:keys [title body device-token]}]
  ;; throw-exceptions false so a rejection comes back as data: FCM puts the real
  ;; reason in the body (UNREGISTERED, SENDER_ID_MISMATCH, INVALID_ARGUMENT,
  ;; third-party auth errors), which an exception would reduce to a status code.
  (let [token (try (access-token)
                   (catch Exception e
                     (println "  push: could not obtain an FCM access token —"
                              "the service account is probably wrong or unreadable:" e)
                     nil))]
    (if-not token
      {:status :no-access-token}
      (let [resp (http/post fcm-url
                            {:headers {"Authorization" (str "Bearer " token)}
                             :content-type :json
                             :throw-exceptions false
                             :body (json/write-str
                                    {:message {:token device-token
                                               :notification (cond-> {:title title}
                                                               body (assoc :body body))}})})]
        ;; a rejection is the only way to learn a token went stale (UNREGISTERED)
        ;; or belongs to another project (SENDER_ID_MISMATCH), so keep it visible
        (when-not (<= 200 (:status resp) 299)
          (println "push rejected by FCM:" (:status resp) (:body resp)))
        resp))))

;; ── Interest registry ─────────────────────────────────────────────────────────
;;
;; {namespace-hex {device-token #{entity ...}}}
;;
;; Keyed by token rather than entity so a client re-registering simply replaces
;; its own set; subscriber counts per namespace are small (your friends), so the
;; per-put check stays trivial once the namespace gate has been passed.
;;
;; In memory only. A restart just means clients re-register on their next watch
;; poll, which is at most a minute away.

(defonce ^:private interests (atom {}))

(defn register-interests!
  "Record which entities `device-token` wants pushed, per namespace.
   `interests-map` is {namespace-hex [entity ...]}.

   Clients send their whole interest map on every watch poll, so this drops the
   token everywhere first and then re-adds it — otherwise unfriending someone
   would leave a stale subscription behind."
  [device-token interests-map]
  (when device-token
    (swap! interests
           (fn [registry]
             (let [cleared (reduce-kv (fn [m namespace tokens]
                                        (let [tokens' (dissoc tokens device-token)]
                                          (if (empty? tokens') m (assoc m namespace tokens'))))
                                      {}
                                      registry)]
               (reduce-kv (fn [m namespace v]
                            ;; {:name n :entities [...]}, or a bare vector from a
                            ;; client that predates names
                            (let [{:keys [name entities]} (if (map? v) v {:entities v})]
                              (if (seq entities)
                                (assoc-in m [namespace device-token]
                                          {:name name :entities (set entities)})
                                m)))
                          cleared
                          interests-map))))
    nil))

;; ── Completion detection ──────────────────────────────────────────────────────

(defn- capitalize-first
  "Upper-case the first character, leaving the rest alone — names are user-entered
   and often lower-case, and clojure.string/capitalize would also lower-case the
   remainder, turning \"JB\" into \"Jb\"."
  [s]
  (if (seq s)
    (str (str/upper-case (subs s 0 1)) (subs s 1))
    s))

(defn completion-pushes
  "Pushes owed for a commit, or nil. `tree` must be the namespace's tree as it
   stands BEFORE the diff is applied, since the prior versions live under the
   keys the diff is about to delete.

   A push is owed when a put marks a task complete, the same commit deletes a
   prior version of that entity, that prior version was not complete, and some
   subscriber named the entity."
  [namespace tree {:keys [puts deletes]}]
  (let [subscribers (not-empty (get @interests namespace))
        ;; previous versions by entity, via direct lookups on the deleted keys
        previous (into {}
                       (keep (fn [timestamp]
                               (when-let [entry (get tree timestamp)]
                                 (first entry))))
                       deletes)
        ;; every task in this commit that just went false -> true
        completed (into []
                        (keep (fn [[_timestamp entity-map]]
                                (let [[entity attmap] (first entity-map)
                                      prior (get previous entity)]
                                  (when (and (:task/completed attmap)
                                             (some? prior)
                                             (not (:task/completed prior)))
                                    [entity attmap]))))
                        puts)]
    (when (and subscribers (seq completed))
      (not-empty
       (into []
             (mapcat (fn [[entity attmap]]
                       ;; the name is whatever the *subscriber* calls this person,
                       ;; which is what they want to read — the server never has to
                       ;; know anything about who owns a namespace
                       (for [[token {:keys [name entities]}] subscribers
                             :when (contains? entities entity)]
                         {:device-token token
                          :title (str (capitalize-first (or name "Someone"))
                                      " finished \"" (:task/title attmap) "\"")})))
             completed)))))

(defn debug-registry
  "The live interest registry, for poking at from a REPL."
  []
  @interests)

(defonce ^ExecutorService sender (Executors/newSingleThreadExecutor))

(defn dispatch!
  "Fire and forget, so a commit never blocks on FCM."
  [pushes]
  (when (and (seq pushes) (not (available?)))
    (println "push skipped:" (count pushes) "notification(s) owed but no FCM credentials"))
  (when (and (seq pushes) (available?))
    (.submit sender ^Runnable
             (fn []
               (doseq [push pushes]
                 (try (send-push-notification! push)
                      (catch Exception e
                        (println (str "push failed: " e))))))))
  nil)
