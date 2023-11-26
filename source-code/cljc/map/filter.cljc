
(ns map.filter)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn filter-values
  ; @param (map) n
  ; @param (function) filter-f
  ;
  ; @usage
  ; (filter-values {:a "A"} string?)
  ;
  ; @example
  ; (filter-values {:a 0 :b 1 :c 2} even?)
  ; =>
  ; {:a 0 :c 2}
  ;
  ; @return (map)
  [n filter-f]
  (letfn [(f0 [%1 %2 %3] (if (-> %3 filter-f) (assoc %1 %2 %3) %1))]
         (reduce-kv f0 {} n)))
