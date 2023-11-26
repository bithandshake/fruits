
(ns string.filter)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn filter-characters
  ; @param (*) n
  ; @param (vector) allowed-characters
  ;
  ; @example
  ; (filter-characters "+3630 / 123 - 4567"
  ;                    ["+" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"])
  ; =>
  ; "+36301234567"
  ;
  ; @example
  ; (filter-characters [:a :b] [":" "a" "b"])
  ; =>
  ; ":a:b"
  ;
  ; @return (string)
  [n allowed-characters]
  (let [n (str n)]
       (letfn [(f0 [result x]
                   (if (some #(= x %) allowed-characters)
                       (str result x) result))]
              (reduce f0 "" n))))
