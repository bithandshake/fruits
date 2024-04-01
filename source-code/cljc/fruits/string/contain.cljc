
(ns fruits.string.contain
    (:require [clojure.string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn contains-part?
  ; @description
  ; Returns TRUE if the given 'n' string contains the given 'x' string.
  ;
  ; @param (string) n
  ; @param (string) x
  ;
  ; @usage
  ; (contains-part? "abc" "ab")
  ; =>
  ; true
  ;
  ; @usage
  ; (contains-part? "abc" "cd")
  ; =>
  ; false
  ;
  ; @usage
  ; (contains-part? "abc" "")
  ; =>
  ; true
  ;
  ; @usage
  ; (contains-part? "abc" nil)
  ; =>
  ; true
  ;
  ; @return (boolean)
  [n x]
  (let [n (str n)
        x (str x)]
       (clojure.string/includes? n x)))

(defn if-contains-part
  ; @description
  ; Returns the given 'n' string if it contains the given 'x' string.
  ;
  ; @param (string) n
  ; @param (string) x
  ;
  ; @usage
  ; (if-contains-part "abc" "ab")
  ; =>
  ; "abc"
  ;
  ; @usage
  ; (if-contains-part "abc" "cd")
  ; =>
  ; nil
  ;
  ; @usage
  ; (if-contains-part "abc" "")
  ; =>
  ; "abc"
  ;
  ; @usage
  ; (if-contains-part "abc" nil)
  ; =>
  ; "abc"
  ;
  ; @usage
  ; (if-contains-part [:a] "[:")
  ; =>
  ; "[:a]"
  ;
  ; @return (string)
  [n x]
  (let [n (str n)
        x (str x)]
       (if (-> n (clojure.string/includes? x))
           (-> n))))
