
(ns fruits.format.version
    (:require [fruits.format.number :as number]
              [fruits.mixed.api     :as mixed]
              [fruits.regex.api     :as regex]
              [fruits.string.api    :as string]
              [fruits.vector.api    :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn inc-version
  ; @description
  ; Increments the given version number.
  ;
  ; @param (string) n
  ;
  ; @usage
  ; (inc-version "1.2.19")
  ; =>
  ; "1.2.20"
  ;
  ; @usage
  ; (inc-version "0.0.99")
  ; =>
  ; "0.1.00"
  ;
  ; @usage
  ; (inc-version "9.9")
  ; =>
  ; "10.0"
  ;
  ; @return (string)
  [n]
  (letfn [(f0 [n delimiter-positions]
              (if (vector/not-empty? delimiter-positions)
                  (f0 (string/insert-part n "." (last delimiter-positions))
                      (vector/remove-last-item delimiter-positions))
                  (-> n)))]
         ; ...
         (if-let [version (-> n (regex/re-first #"\d(\.\d)*") first)]
                 (loop [version version delimiter-positions []]
                       (if-let [delimiter-position (string/first-dex-of version ".")]
                               (recur (string/remove-first-occurence version ".")
                                      (conj delimiter-positions delimiter-position))
                               (f0 (-> version (mixed/to-integer)
                                               (inc)
                                               (number/fill-leading-zeros (count version)))
                                   ; If the 'version' string contained only "9" digits before the increasing,
                                   ; an offset must be applied on the delimiter positions.
                                   ; Otherwise, "9.9" would be updated to "1.00" instead of "10.0".
                                   (if (-> version             (regex/re-match? #"^[9]+$"))
                                       (-> delimiter-positions (vector/->items inc))
                                       (-> delimiter-positions))))))))
