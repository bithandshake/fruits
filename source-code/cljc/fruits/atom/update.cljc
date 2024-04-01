
(ns fruits.atom.update)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn not!
  ; @description
  ; Performs a negation on the value of the given atom.
  ;
  ; @param (atom) n
  ;
  ; @usage
  ; (def my-atom (atom true))
  ; (not! my-atom)
  ;
  ; @return (boolean)
  [n]
  (swap! n not))

(defn inc!
  ; @description
  ; Increments the value of the given atom.
  ;
  ; @param (atom) n
  ;
  ; @usage
  ; (def my-atom (atom 42))
  ; (inc! my-atom)
  ;
  ; @return (number)
  [n]
  (swap! n inc))

(defn dec!
  ; @description
  ; Decrements the value of the given atom.
  ;
  ; @param (atom) n
  ;
  ; @usage
  ; (def my-atom (atom 42))
  ; (dec! my-atom)
  ;
  ; @return (number)
  [n]
  (swap! n dec))
