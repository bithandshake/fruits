
(ns regex.check)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn pattern?
  ; @description
  ; Returns TRUE if the given 'n' value is a regex pattern.
  ;
  ; @param (*) n
  ;
  ; @return (boolean)
  [n]
  (= (-> n     type)
     (-> #"\d" type)))
