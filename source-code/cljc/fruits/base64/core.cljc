
(ns fruits.base64.core
    (:require [fruits.string.api :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn wrap
  ; @param (string) n
  ; @param (string) mime-type
  ;
  ; @usage
  ; (wrap "..." "application/pdf")
  ;
  ; @example
  ; (wrap "..." "application/pdf")
  ; =>
  ; "data:application/pdf;base64,..."
  ;
  ; @example
  ; (wrap "" "application/pdf")
  ; =>
  ; nil
  ;
  ; @example
  ; (wrap nil "application/pdf")
  ; =>
  ; nil
  ;
  ; @return (string)
  [n mime-type]
  (if (string/not-empty? n)
      (str "data:"mime-type";base64,"n)))
