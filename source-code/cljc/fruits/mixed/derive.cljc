
(ns fruits.mixed.derive
    (:require [fruits.reader.api  :as reader]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn derive-integer
  ; @description
  ; Returns the given 'n' value if provided as an integer, otherwise ...
  ; 1. converts it into a string,
  ; 2. reads decimal digits and unary operators,
  ; 3. converts the result into an integer.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (derive-integer 3)
  ; =>
  ; 3
  ;
  ; @usage
  ; (derive-integer "-123")
  ; =>
  ; -123
  ;
  ; @usage
  ; (derive-integer "123.456")
  ; =>
  ; 123
  ;
  ; @usage
  ; (derive-integer "abc-123.456def789")
  ; =>
  ; -123
  ;
  ; @usage
  ; (derive-integer "abc-008")
  ; =>
  ; 8
  ;
  ; @return (integer)
  [n]
  (if (-> n integer?)
      (-> n)
      ; Matches digits with optional preceding negative sign (cannot be start with zero).
      ; Prevents the parser misreading values as non-decimal numbers e.g., "008" (in Java, leading zeros denote octal literals).
      (if-let [x (->> n str (re-find #"[\-]?[1-9][0-9]*"))]
              (reader/parse-edn x))))

(defn derive-number
  ; @description
  ; Returns the given 'n' value if provided as a number, otherwise ...
  ; 1. converts it into a string,
  ; 2. reads decimal digits, decimal separators and unary operators,
  ; 3. converts the result into a number.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (derive-number 3)
  ; =>
  ; 3
  ;
  ; @usage
  ; (derive-number "-123")
  ; =>
  ; -123
  ;
  ; @usage
  ; (derive-number "123.456")
  ; =>
  ; 123.456
  ;
  ; @usage
  ; (derive-number "abc-123.456def789")
  ; =>
  ; -123.456
  ;
  ; @usage
  ; (derive-number "abc-008")
  ; =>
  ; 8
  ;
  ; @return (number)
  [n]
  (if (-> n number?)
      (-> n)
      ; 1. Matches a decimal point followed by digits
      ;    Cannot be preceded by digits.
      ;    Matches abbreviated fragments e.g., ".123".
      ;    Extends the match with a leading zero.
      ; 2. Matches a single zero with optional following decimals and optional preceding negative sign.
      ;    Cannot be preceded by digits except other zeros.
      ;    Cannot be followed by digits.
      ;    Matches zero and zero with decimals that cannot be catched by the 3rd pattern.
      ; 3. Matches digits with optional decimals and optional negative sign (cannot be start with zero).
      ;    Prevents the parser misreading values as non-decimal numbers e.g., "008" (in Java, leading zeros denote octal literals).
      ;
      ; 1.    "abc.123def" ->   "0.123"
      ; 2. "abc000.123def" ->   "0.123"
      ; 3. "abc100.123def" -> "100.123"
      (letfn [(f0 [%] (if % (str "0" %)))
              (xx [c %] (str c %))]
             (if-let [x (or (->> n (re-find #"(?<![0-9])\.[0-9]+")                      f0)
                            (->> n (re-find #"[\-]?(?<![1-9][0-9]*)0(\.[0-9]+)?(?!\d)") first)
                            (->> n (re-find #"[\-]?[1-9][0-9]*(\.[0-9]+)?")             first))]
                     (reader/parse-edn x)))))

(defn derive-keyword
  ; @description
  ; Returns the given 'n' value if provided as a keyword, otherwise ...
  ; 1. converts it into a string,
  ; 2. reads characters that are allowed in keywords,
  ; 3. converts the result into a keyword.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (derive-keyword :a)
  ; =>
  ; :a
  ;
  ; @usage
  ; (derive-keyword "abc")
  ; =>
  ; :abc
  ;
  ; @usage
  ; (derive-keyword 123)
  ; =>
  ; :123
  ;
  ; @usage
  ; (derive-keyword [:a :b :c])
  ; =>
  ; ::a
  ;
  ; @usage
  ; (derive-keyword {:a "A" :b "B"})
  ; =>
  ; ::a
  ;
  ; @return (*)
  [n]
  (if (-> n keyword?)
      (-> n)
      (if-let [x (->> n str (re-find #"[a-zA-Z\d\+\-\_\<\>\=\*\!\?\%\&\/\#\:\.\']+"))]
              (-> x keyword))))
