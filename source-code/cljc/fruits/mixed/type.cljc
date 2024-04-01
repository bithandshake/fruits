
(ns fruits.mixed.type
    (:refer-clojure :exclude [number?]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn number?
  ; @description
  ; Returns TRUE if the given 'n' value is a number or a string that contains only a number.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (number? 123)
  ; =>
  ; true
  ;
  ; @usage
  ; (number? 123.456)
  ; =>
  ; true
  ;
  ; @usage
  ; (number? "123")
  ; =>
  ; true
  ;
  ; @usage
  ; (number? "123.456")
  ; =>
  ; true
  ;
  ; @usage
  ; (number? "+123.456")
  ; =>
  ; true
  ;
  ; @usage
  ; (number? "-123.456")
  ; =>
  ; true
  ;
  ; @return (boolean)
  [n]
  (or (-> n number?)
      (-> (re-find #"^[\+\-]?[\d]+$"          (str n)) some?)
      (-> (re-find #"^[\+\-]?[\d]+[\.][\d]+$" (str n)) some?)))

(defn rational-number?
  ; @description
  ; Returns TRUE if the given 'n' value is a rational number or a string that contains only a rational number.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (rational-number? 123)
  ; =>
  ; true
  ;
  ; @usage
  ; (rational-number? 123.456)
  ; =>
  ; true
  ;
  ; @usage
  ; (rational-number? "123")
  ; =>
  ; true
  ;
  ; @usage
  ; (rational-number? "123.456")
  ; =>
  ; true
  ;
  ; @usage
  ; (rational-number? "+123.456")
  ; =>
  ; true
  ;
  ; @usage
  ; (rational-number? "-123.456")
  ; =>
  ; true
  ;
  ; @return (boolean)
  [n]
  (-> n number?))

(defn whole-number?
  ; @description
  ; Returns TRUE if the given 'n' value is a whole number or a string that contains only a whole number.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (whole-number? 123)
  ; =>
  ; true
  ;
  ; @usage
  ; (whole-number? "123")
  ; =>
  ; true
  ;
  ; @usage
  ; (whole-number? "+123")
  ; =>
  ; true
  ;
  ; @usage
  ; (whole-number? "-123")
  ; =>
  ; true
  ;
  ; @usage
  ; (whole-number? "123.456")
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n]
  (or (-> n integer?)
      (-> (re-find #"^[\+\-]?[\d]+$" (str n)) some?)))

(defn natural-whole-number?
  ; @description
  ; Returns TRUE if the given 'n' value is a natural whole number or a string that contains only a natural whole number.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (natural-whole-number? 123)
  ; =>
  ; true
  ;
  ; @usage
  ; (natural-whole-number? "123")
  ; =>
  ; true
  ;
  ; @usage
  ; (natural-whole-number? "+123")
  ; =>
  ; true
  ;
  ; @usage
  ; (natural-whole-number? "-123")
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n]
  (or (-> n nat-int?)
      (-> (re-find #"^[\+]?[\d]+$" (str n)) some?)))

(defn positive-whole-number?
  ; @description
  ; Returns TRUE if the given 'n' value is a positive whole number or a string that contains only a positive whole number.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (positive-whole-number? 123)
  ; =>
  ; true
  ;
  ; @usage
  ; (positive-whole-number? "123")
  ; =>
  ; true
  ;
  ; @usage
  ; (positive-whole-number? "+123")
  ; =>
  ; true
  ;
  ; @usage
  ; (positive-whole-number? "0")
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n]
  (or (-> n pos-int?)
      (and (-> (re-find #"^[\+]?[0]+$"  (str n)) nil?)
           (-> (re-find #"^[\+]?[\d]+$" (str n)) some?))))

(defn negative-whole-number?
  ; @description
  ; Returns TRUE if the given 'n' value is a negative whole number or a string that contains only a negative whole number.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (negative-whole-number? -123)
  ; =>
  ; true
  ;
  ; @usage
  ; (negative-whole-number? "-123")
  ; =>
  ; true
  ;
  ; @usage
  ; (negative-whole-number? "123")
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n]
  (or (-> n neg-int?)
      (and (-> (re-find #"^[-][0]+$"  (str n)) nil?)
           (-> (re-find #"^[-][\d]+$" (str n)) some?))))
