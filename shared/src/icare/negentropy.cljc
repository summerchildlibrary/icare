(ns icare.negentropy
  (:require [icare.aztree :as aztree]))

;; Negentropy set reconciliation between two aztrees keyed [timestamp entity] ->
;; {entity {attribute value}}. One side is authoritative (memory); the other
;; (disk) is made to match it. The two endpoints exchange only fingerprints, and
;; at the leaves the differing entries, so the bytes transferred scale with the
;; number of differences rather than the size of either set.
;;
;; A round trip:
;;   initiator (authoritative)         responder (to be reconciled)
;;   --------------------------        ----------------------------
;;   send ranges [[low high fp]...] -> respond: for each range whose fp differs
;;                                     from ours, split into buckets; small ones
;;                                     come back as [:items ...], larger as [:fp ...]
;;   process-response <-------------- send sub-ranges
;;     :items leaves -> diff into puts/deletes
;;     :fp ranges    -> loop with those still differing
;;   ... until no :fp ranges remain, then apply puts/deletes.
;;
;; Because a key carries the entity's update timestamp and every touch bumps it,
;; two matching keys are guaranteed to be the same unchanged entry. So a leaf diff
;; is a pure key-set difference: keys the initiator has that the responder lacks
;; are puts; keys the responder has that the initiator lacks are deletes.

(def ^:private bucket-count 16)
(def ^:private item-threshold 4)

(defn initial-ranges
  "The opening message: a single range covering the whole tree, tagged with the
   initiator's fingerprint for it."
  [tree]
  [[nil nil (aztree/range-fingerprint tree nil nil)]])

(defn respond
  "Responder side. Given the responder's `tree` and the initiator's `ranges`
   (each [low high their-fp]), return a vector of sub-ranges describing where the
   responder differs. Each sub-range is either
     [low high :fp   our-fp]     -- fingerprints still differ, recurse here
     [low high :items our-items] -- range is small; here are our actual [key value]s
   Ranges whose fingerprint already matches contribute nothing."
  [tree ranges]
  (into []
        (mapcat
         (fn [[low high their-fp]]
           (let [our-fp (aztree/range-fingerprint tree low high)]
             (if (= our-fp their-fp)
               []
               (mapcat
                (fn [[bucket-low bucket-high]]
                  (if (<= (aztree/range-size tree bucket-low bucket-high) item-threshold)
                    [[bucket-low bucket-high :items (aztree/range-items tree bucket-low bucket-high)]]
                    [[bucket-low bucket-high :fp (aztree/range-fingerprint tree bucket-low bucket-high)]]))
                (aztree/split-range tree low high bucket-count))))))
        ranges))

(defn- diff-leaf
  "Diff the initiator's authoritative items for [low, high) against the
   responder's `their-items` for the same range. Returns [puts deletes]: puts are
   [key value] the initiator has but the responder lacks; deletes are keys the
   responder has but the initiator lacks. Matching keys are identical entries and
   need no action."
  [tree low high their-items]
  (let [our-items  (aztree/range-items tree low high)
        our-keys   (set (map first our-items))
        their-keys (set (map first their-items))
        puts    (filterv (fn [[k _]] (not (contains? their-keys k))) our-items)
        deletes (filterv (fn [k] (not (contains? our-keys k))) (map first their-items))]
    [puts deletes]))

(defn process-response
  "Initiator side. Given the initiator's `tree` and the responder's `sub-ranges`,
   return {:next-ranges :puts :deletes}. :fp sub-ranges that still differ from the
   initiator become next-round ranges [low high our-fp]; :items leaves are diffed
   into puts and deletes. When :next-ranges is empty the sync is complete."
  [tree sub-ranges]
  (reduce
   (fn [acc [low high tag payload]]
     (case tag
       :fp
       (let [our-fp (aztree/range-fingerprint tree low high)]
         (if (= our-fp payload)
           acc
           (update acc :next-ranges conj [low high our-fp])))

       :items
       (let [[puts deletes] (diff-leaf tree low high payload)]
         (-> acc
             (update :puts into puts)
             (update :deletes into deletes)))))
   {:next-ranges [] :puts [] :deletes []}
   sub-ranges))
