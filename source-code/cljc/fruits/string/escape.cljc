
(ns fruits.string.escape)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn escape-characters
  ; @description
  ; Escapes each character in the given 'n' string.
  ;
  ; @param (string) n
  ;
  ; @usage
  ; (escape-characters "abc")
  ; =>
  ; "\\a\\b\\c"
  ;
  ; @return (string)
  [n]
  (let [n (str n)]
       (letfn [(f0 [result x] (str result "\\" x))]
              (reduce f0 nil n))))
