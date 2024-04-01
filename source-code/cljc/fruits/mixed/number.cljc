
(ns fruits.mixed.number
    (:require [fruits.mixed.convert :as convert]
              [fruits.mixed.join :as join]
              [fruits.mixed.derive :as derive]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn add-numbers
  ; @description
  ; Adds the given values converted into numbers (non-numeric values are converted into 0).
  ;
  ; @param (list of *) abc
  ;
  ; @usage
  ; (add-numbers 1 2 "3")
  ; =>
  ; 6
  ;
  ; @usage
  ; (add-numbers 1 2 "3" "a")
  ; =>
  ; 6
  ;
  ; @return (number)
  [& abc]
  (letfn [(f0 [result x] (+ result (convert/to-number x)))]
         (reduce f0 0 abc)))

(defn subtract-numbers
  ; @description
  ; Subtracts the given values converted into numbers (non-numeric values are converted into 0).
  ;
  ; @param (list of *) abc
  ;
  ; @usage
  ; (subtract-numbers 1 2 "3")
  ; =>
  ; -4
  ;
  ; @usage
  ; (subtract-numbers 1 2 "3" "a")
  ; =>
  ; -4
  ;
  ; @return (number)
  [& abc]
  (letfn [(f0 [result x] (- result (convert/to-number x)))]
         (reduce f0 0 abc)))

(defn multiply-numbers
  ; @description
  ; Multiplies the given values converted into numbers (non-numeric values are converted into 0).
  ;
  ; @param (list of *) abc
  ;
  ; @usage
  ; (multiply-numbers 1 2 "3")
  ; =>
  ; 6
  ;
  ; @usage
  ; (multiply-numbers 1 2 "3" "a")
  ; =>
  ; 0
  ;
  ; @return (number)
  [& abc]
  (letfn [(f0 [result x] (* result (convert/to-number x)))]
         (reduce f0 1 abc)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn update-as-number
  ; @description
  ; - Converts the first numeric part of the given 'n' value into a number and applies the given 'f' function on it.
  ; - If no numeric part found, it provides 0 to the given 'f' function.
  ; - Returns the output as it returned from the given 'f' function.
  ;
  ; @param (*) n
  ; @param (function) f
  ; @param (list of *)(opt) params
  ;
  ; @usage
  ; (update-as-number "123" inc)
  ; =>
  ; 124
  ;
  ; @usage
  ; (update-as-number "123" + 1)
  ; =>
  ; 124
  ;
  ; @usage
  ; (update-as-number 123 + 1)
  ; =>
  ; 124
  ;
  ; @usage
  ; (update-as-number "abc-123.456def789" - 10)
  ; =>
  ; -133.456
  ;
  ; @usage
  ; (update-as-number "abc" inc)
  ; =>
  ; 1
  ;
  ; @usage
  ; (update-as-number [] inc)
  ; =>
  ; 1
  ;
  ; @return (*)
  [n f & params]
  (letfn [(f0 [%] (apply f % params))]
         (-> n convert/to-number f0)))

(defn update-joined-digits
  ; @description
  ; - Joins the decimal digits of the given 'n' value and applies the given 'f' function on it.
  ; - Provides the joined digits as a string to the given 'f' function.
  ; - Returns the output as it returned from the given 'f' function.
  ;
  ; @param (*) n
  ; @param (function) f
  ; @param (list of *)(opt) params
  ;
  ; @usage
  ; (update-joined-digits "abc123def456" str "...")
  ; =>
  ; "123457..."
  ;
  ; @return (*)
  [n f & params]
  (letfn [(f0 [%] (apply f % params))]
         (-> n join/join-digits f0)))

(defn update-joined-digits-as-integer
  ; @description
  ; - Joins the decimal digits of the given 'n' value, converts it into an integer and applies the given 'f' function on it.
  ; - Provides the joined digits as an integer to the given 'f' function.
  ; - Returns the output as it returned from the given 'f' function.
  ;
  ; @param (*) n
  ; @param (function) f
  ; @param (list of *)(opt) params
  ;
  ; @usage
  ; (update-joined-digits-as-integer "abc123def456" inc)
  ; =>
  ; 123457
  ;
  ; @usage
  ; (update-joined-digits-as-integer "abc-123.456def789" - 10)
  ; =>
  ; 123456779
  ;
  ; @return (*)
  [n f & params]
  (letfn [(f0 [%] (apply f % params))]
         (-> n join/join-digits derive/derive-integer f0)))

(defn update-numeric-part
  ; @description
  ; - Applies the given 'f' function on the first numeric part of the given 'n' value.
  ; - Keeps the surrounding parts of the given 'n' value.
  ; - Returns the output as a string.
  ;
  ; @param (*) n
  ; @param (function) f
  ; @param (list of *)(opt) params
  ;
  ; @usage
  ; (update-numeric-part "123" inc)
  ; =>
  ; "124"
  ;
  ; @usage
  ; (update-numeric-part "123" + 1)
  ; =>
  ; "124"
  ;
  ; @usage
  ; (update-numeric-part 123 + 1)
  ; =>
  ; "124"
  ;
  ; @usage
  ; (update-numeric-part "abc-123.456def789" - 10)
  ; =>
  ; "abc-133.456def789"
  ;
  ; @usage
  ; (update-numeric-part "Value: +10" - 12)
  ; =>
  ; "Value: -2"
  ;
  ; @return (string)
  [n f & params]
  ; - This function, unlike other number related functions in the 'fruits.mixed.api' library,
  ;   uses a regex pattern that matches the leading plus sign, and the leading zeros as well!
  ;   E.g., (update-numeric-part "Value: +10" - 12) => "value: -2"   <- Good :)
  ;         (update-numeric-part "Value: +10" - 12) => "value: +-2"  <- Bad  :(
  ;   E.g., (update-numeric-part "Value:  05" + 20) => "value: 25"   <- Good :)
  ;         (update-numeric-part "Value:  05" + 20) => "value: 025"  <- Bad  :(
  ; - Another solution could be if this function would provide the numeric part as a string to the 'f' function:
  ;   E.g., (update-numeric-part "Value: +10" println) => (println "+10") <- Maybe it would be better?
  ;         (update-numeric-part "Value: +10" println) => (println 10)    <- This is how it works now.
  (let [n (str n)]
       (if-let [number (->> n (re-find #"[\-\+]?[0-9]+(\.[0-9]+)?") first)]
               (let [number-starts-at (clojure.string/index-of n number)
                     number-ends-at   (+ number-starts-at (count number))]
                    (str (subs n 0 number-starts-at)
                         (apply update-as-number number f params)
                         (subs n number-ends-at)))
               (-> n))))
