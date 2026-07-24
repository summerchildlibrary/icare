(ns testclient
  "Standalone test client: behaves like the ClojureDart app's icare.network,
   pushing a signed friend-namespace commit to the datamigo server over HTTP so
   we can verify the real app pulls it. Reuses the server's own aztree/negentropy
   and secp256k1 so the crypto and reconciliation match exactly.

   Run under the :datamigo alias classpath (has aztree, negentropy, secp256k1)."
  (:require [aztree :as aztree]
            [negentropy :as negentropy]
            [clojure.edn :as edn])
  (:import (fr.acinq.secp256k1 Secp256k1)
           (java.security MessageDigest SecureRandom)
           (java.util HexFormat)
           (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)))

(def ^HexFormat hexf (HexFormat/of))
(defn hex [^bytes b] (.formatHex hexf b))
(defn unhex [^String s] (.parseHex hexf s))

(def ^Secp256k1 secp (Secp256k1/get))
(def ^HttpClient http (HttpClient/newHttpClient))
(def server "http://localhost:8080")

(defn sha256 ^bytes [^String s]
  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))

;; ── Keypair (BIP340 x-only, nostr-compatible) ─────────────────────────────────

(defn gen-keypair []
  (let [rng (SecureRandom.)]
    (loop []
      (let [sk (byte-array 32)]
        (.nextBytes rng sk)
        (if (.secKeyVerify secp sk)
          (let [compressed (.pubKeyCompress secp (.pubkeyCreate secp sk)) ; 33 bytes, 02/03 prefix
                xonly (byte-array 32)]
            (System/arraycopy compressed 1 xonly 0 32) ; drop parity prefix -> x-only
            {:sk sk :sk-hex (hex sk) :pub-hex (hex xonly)})
          (recur))))))

(defn sign-schnorr [sk-hex ^String msg-hash-hex]
  ;; Sign the 32-byte message (the sha256 hash) with BIP340 Schnorr, no aux rand.
  (hex (.signSchnorr secp (unhex msg-hash-hex) (unhex sk-hex) nil)))

;; ── HTTP (EDN in/out) ─────────────────────────────────────────────────────────

(defn post-edn [path body]
  (let [req (-> (HttpRequest/newBuilder (URI/create (str server path)))
                (.POST (HttpRequest$BodyPublishers/ofString (pr-str body)))
                (.build))
        resp (.send http req (HttpResponse$BodyHandlers/ofString))]
    (edn/read-string (.body resp))))

;; ── Sync rounds (initiator), mirrors icare.network/run-sync-rounds ────────────

(defn run-sync-rounds [namespace-hex tree]
  (loop [ranges (negentropy/initial-ranges tree)
         only-ours [] only-theirs [] commit-token nil rounds 0]
    (if (or (empty? ranges) (> rounds 64))
      {:only-ours only-ours :only-theirs only-theirs :commit-token commit-token}
      (let [resp (post-edn "/sync" {:namespace namespace-hex :ranges ranges})
            sub-ranges (:sub-ranges resp)
            token (:commit-token resp)
            {next-ranges :next-ranges new-ours :only-ours new-theirs :only-theirs}
            (negentropy/process-response tree sub-ranges)]
        (recur next-ranges
               (into only-ours new-ours)
               (into only-theirs new-theirs)
               token (inc rounds))))))

;; ── Push our namespace (sign + commit), mirrors push-sync! ────────────────────

(defn push! [{:keys [pub-hex sk-hex]} tree]
  (let [{:keys [only-ours only-theirs commit-token]} (run-sync-rounds pub-hex tree)
        diff {:puts only-ours :deletes (mapv first only-theirs)}]
    (if (and (empty? (:puts diff)) (empty? (:deletes diff)))
      (do (println "already in sync, nothing to commit") {:ok true :noop true})
      (let [signed-payload (pr-str [pub-hex diff commit-token])
            msg-hash-hex (hex (sha256 signed-payload))
            signature (sign-schnorr sk-hex msg-hash-hex)
            self-ok (.verifySchnorr secp (unhex signature) (sha256 signed-payload) (unhex pub-hex))]
        (println "committing" (count (:puts diff)) "puts," (count (:deletes diff)) "deletes")
        (println "signed payload:" signed-payload)
        (println "msg-hash-hex:" msg-hash-hex)
        (println "self verifySchnorr:" self-ok)
        (post-edn "/commit" {:namespace pub-hex
                             :diff diff
                             :commit-token commit-token
                             :signature signature})))))

;; ── Build a friend-namespace tree of state to send ────────────────────────────
;; :ordered is keyed by an integer timestamp -> {entity {attribute value}}.
;; We craft a friend whose db holds a querido with a couple of tasks so it is
;; recognizable when the app pulls it.

(defn demo-tree []
  (let [q     "friend-querido-1"
        t1    "friend-task-1"
        t2    "friend-task-2"
        records
        [[0 {q  {:querido/name "Test Client"
                 :querido/asset "assets/animated/heart.gif"
                 :querido/task-order [t1 t2]
                 :position/canvas :home
                 :position/x 200.0
                 :position/y 400.0}}]
         [1 {t1 {:task/text "hello from the test client"
                 :task/done false}}]
         [2 {t2 {:task/text "pushed over negentropy + schnorr"
                 :task/done true}}]]]
    (reduce (fn [tree [ts entity-map]] (assoc tree ts entity-map))
            (aztree/atree) records)))

(defn -main [& _]
  ;; ClojureDart's pr-str has no namespace-map shorthand — it always prints maps
  ;; expanded ({:task/text ...}). Match that so our signed payload's bytes are
  ;; identical to what the server produces when it re-serializes the parsed diff.
  (binding [*print-namespace-maps* false]
    (let [kp (gen-keypair)
          tree (demo-tree)]
      (println "=== TEST CLIENT ===")
      (println "namespace pub-hex (paste this as a friend in the app):")
      (println (:pub-hex kp))
      (println "seckey-hex (test only):" (:sk-hex kp))
      (println "tree entries:" (count tree))
      (let [result (push! kp tree)]
        (println "commit result:" result))
      (shutdown-agents))))
