(ns signtest
  "End-to-end check of the new commit signing: sign a diff containing a >8-entry
   map (the case that broke the old scheme) and verify the server-side path."
  (:require [clojure.edn :as edn])
  (:import (fr.acinq.secp256k1 Secp256k1)
           (java.security MessageDigest SecureRandom)
           (java.util HexFormat)))
(def ^HexFormat hexf (HexFormat/of))
(defn hex [b] (.formatHex hexf b))
(defn unhex [s] (.parseHex hexf s))
(def ^Secp256k1 secp (Secp256k1/get))
(defn sha256 [s] (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String s "UTF-8")))

(defn -main [& _]
  (binding [*print-namespace-maps* false]
    (let [rng (SecureRandom.) sk (byte-array 32)]
      (.nextBytes rng sk)
      (let [compressed (.pubKeyCompress secp (.pubkeyCreate secp sk))
            xonly (byte-array 32)]
        (System/arraycopy compressed 1 xonly 0 32)
        (let [ns-hex (hex xonly)
              token "tok-123"
              ;; the 10-entry physics map that broke the old scheme
              diff {:puts [[1 {:global/physics {:physics/repulsion-strength 2200.0 :physics/repulsion-radius 150.0
                                                :physics/damping 4.0 :physics/static-friction 300.0
                                                :physics/wall-stiffness 200.0 :physics/wall-damping 15.0
                                                :physics/obstacle-radius 85.0 :physics/sleep-speed 4.0
                                                :physics/sleep-frames 12 :physics/max-speed 3000.0}}]]
                    :deletes []}
              ;; CLIENT: serialize once, sign that exact string
              diff-edn (pr-str diff)
              sig (hex (.signSchnorr secp (sha256 (str ns-hex "|" diff-edn "|" token)) sk nil))
              ;; SERVER: verify over the received string, then parse
              ok (.verifySchnorr secp (unhex sig) (sha256 (str ns-hex "|" diff-edn "|" token)) (unhex ns-hex))
              parsed (edn/read-string diff-edn)
              ;; and confirm the OLD scheme would have failed on this same diff
              old-ok (.verifySchnorr secp (unhex sig) (sha256 (pr-str [ns-hex parsed token])) (unhex ns-hex))]
          (println "map entries:" (count (get-in parsed [:puts 0 1 :global/physics])))
          (println "NEW scheme (sign the transmitted string) verifies:" ok)
          (println "OLD scheme (re-serialize parsed diff) verifies: " old-ok)
          (println "parsed diff usable? puts:" (count (:puts parsed)) "deletes:" (count (:deletes parsed)))
          (println)
          (println (if (and ok (not old-ok)) "FIX CONFIRMED" "UNEXPECTED")))))))
