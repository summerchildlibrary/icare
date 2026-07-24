(ns sigtest
  (:import (fr.acinq.secp256k1 Secp256k1)
           (java.security MessageDigest SecureRandom)
           (java.util HexFormat)))
(def ^HexFormat hexf (HexFormat/of))
(defn hex [b] (.formatHex hexf b))
(defn unhex [s] (.parseHex hexf s))
(def ^Secp256k1 secp (Secp256k1/get))
(defn sha256 [s] (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String s "UTF-8")))
(defn -main [& _]
  (let [rng (SecureRandom.)
        sk (byte-array 32)]
    (.nextBytes rng sk)
    (let [compressed (.pubKeyCompress secp (.pubkeyCreate secp sk))
          prefix (bit-and (aget compressed 0) 0xff)
          xonly (byte-array 32)]
      (System/arraycopy compressed 1 xonly 0 32)
      (println "compressed prefix:" (format "%02x" prefix) "(02=even,03=odd)")
      (let [msg (sha256 "hello")
            sig (.signSchnorr secp msg sk nil)
            ok  (.verifySchnorr secp sig msg xonly)]
        (println "sign with raw sk -> verify against x-only:" ok)))))
