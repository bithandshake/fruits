
(ns fruits.map.get
    (:require [fruits.mixed.api   :as mixed]
              [fruits.seqable.api :as seqable]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn get-by
  ; @description
  ; Returns a specific value from the given 'n' map at the given 'path' dynamic path.
  ;
  ; @param (map) n
  ; @param (vector) path
  ;
  ; @usage
  ; (letfn [(last-dex [%] (-> % count dec))]
  ;        (get-by {:a [{:b "B"} {:c "C"}]} [:a last-dex]))
  ; =>
  ; {:c "C"}
  ;
  ; @usage
  ; (get-by {:a [{:b "B"} {:c "C"}]} [:a #(-> % count dec)])
  ; =>
  ; {:c "C"}
  ;
  ; @return (*)
  [n path]
  (let [n (mixed/to-map n)]
       (get-in n (seqable/dynamic-path n path))))
