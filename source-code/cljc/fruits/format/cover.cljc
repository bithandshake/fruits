
(ns fruits.format.cover
    (:require [fruits.string.api :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn cover-email-address
  ; @description
  ; Covers the local part with '*' characters within the given email address.
  ;
  ; @param (string) n
  ;
  ; @usage
  ; (cover-email-address "user@provider.com")
  ; =>
  ; "u**r@provider.com"
  ;
  ; @return (string)
  [n]
  (if-let [user (string/before-first-occurence n "@" {:return? false})]
          (case (count user) 0 (str n)
                             1 (string/cover n "*")
                             2 (string/cover n "*" 1)
                               (string/cover n (string/repeat "*" (-> user count dec dec)) 1))))
