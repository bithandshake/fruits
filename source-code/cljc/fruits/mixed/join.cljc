
(ns fruits.mixed.join
    (:require [clojure.string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn join-digits
  ; @description
  ; Returns the decimal digits of the given 'n' value joined into a single string.
  ;
  ; @param (number or string) n
  ;
  ; @usage
  ; (join-digits "abc123def456")
  ; =>
  ; "123456"
  ;
  ; @usage
  ; (join-digits "abc-123.456def789")
  ; =>
  ; "123456789"
  ;
  ; @return (string)
  [n]
  (->> n str (re-seq #"\d") clojure.string/join))
 
