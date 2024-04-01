
(ns fruits.format.number
    (:require [fruits.mixed.api  :as mixed]
              [fruits.regex.api  :as regex]
              [fruits.string.api :as string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn sign-number
  ; @description
  ; Prepends a unary operator ("-", "+") to the first numeric part of the given 'n' value (except it is a zero).
  ;
  ; @param (number or string) n
  ;
  ; @usage
  ; (sign-number 123.456)
  ; =>
  ; "+123.456"
  ;
  ; @usage
  ; (sign-number -123.456)
  ; =>
  ; "-123.456"
  ;
  ; @usage
  ; (sign-number 0)
  ; =>
  ; "0"
  ;
  ; @return (string)
  [n]
  (letfn [(f0 [%] (cond (-> % string/first-character (= "-")) (->      %)
                        (-> % (regex/re-match? #"^0+$"))      (->      %)
                        :else                                 (str "+" %)))]
         (mixed/update-numeric-part n f0)))

(defn format-number
  ; @description
  ; Formats the given 'n' value to conform the given format.
  ;
  ; @param (number or string) n
  ; @param (string) format
  ; @param (map)(opt) options
  ; {:cut? (boolean)(opt)
  ;   If TRUE, cuts the remaining part of the number longer than the given format.
  ;   Default: false
  ;  :repeat? (boolean)(opt)
  ;   If TRUE, repeatedly applies the format on longer numbers.
  ;   Default: false}
  ;
  ; @usage
  ; (format-number 123456789 "000-000-000")
  ; =>
  ; "123-456-789"
  ;
  ; @return (string)
  ([n format]
   (format-number n format {}))

  ([n format {:keys [cut? repeat?]}]
   (letfn [(f0 [%] (re-find #"\d" %))
           (f1 [%] (loop [% (str %) format (str format) dex 0 pos 0 result ""]
                         (if (-> format count dec (>= dex))
                             (cond (-> % count dec (< pos)) (-> result)
                                   (-> format (nth dex) f0) (recur % (->  format)        (inc dex) (inc pos) (str result (nth %      pos)))                                                            ; <- Corresponding place in 'format' is a digit.
                                   :return                  (recur % (->  format)        (inc dex) (->  pos) (str result (nth format dex))))
                             (cond (-> repeat?)             (recur % (str format format) (->  dex) (->  pos) (->  result))
                                   (-> cut?)                (->  result)
                                   :else                    (str result (subs % pos))))))]
          (mixed/update-joined-digits n f1))))

(defn group-number
  ; @description
  ; Splits the first found numeric part in the given 'n' value into 3-digit blocks separated by the given delimiter (except decimals).
  ;
  ; @param (number or string) n
  ; @param (string)(opt) delimiter
  ; Default: ","
  ;
  ; @usage
  ; (group-number 123456.7890)
  ; =>
  ; "123,456.7890"
  ;
  ; @return (string)
  ([n]
   (group-number n ","))

  ([n delimiter]
   ; group-count: How many 3-digits blocks is the 'whole-number' string divisible by.
   ; offset:      After dividing the 'whole-number' string into 3-digit blocks,
   ;              how many digits are left out (at the beginning of the 'whole-number' string).
   (letfn [; In case the 'offset' value is 0 (when the number of digits in the 'whole-number' string is divisible by 3),
           ; it removes the unnecessary separator character from the beginning of the output in every iteration.
           (f0 [%] (let [whole-number (string/before-first-occurence % "." {:return? true})
                         decimals     (string/from-first-occurence   % "." {:return? false})
                         group-count  (quot (count whole-number) 3)
                         offset       (-    (count whole-number) (* 3 group-count))]
                        (str (string/trim (reduce (fn [result dex]
                                                      (let [x (+ offset (* 3 dex))]
                                                           (if (-> x zero?)
                                                               (str result           (subs whole-number x (+ x 3)))
                                                               (str result delimiter (subs whole-number x (+ x 3))))))
                                                  (subs whole-number 0 offset)
                                                  (range group-count)))
                             (-> decimals))))]
          (mixed/update-numeric-part n f0))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn fill-leading-zeros
  ; @description
  ; Extends the first numeric part of the given 'n' value with leading zeros to match the given length.
  ;
  ; @param (number or string) n
  ; @param (integer) length
  ;
  ; @usage
  ; (leading-zeros "123" 6)
  ; =>
  ; "000123"
  ;
  ; @usage
  ; (fill-leading-zeros 123 3)
  ; =>
  ; "123"
  ;
  ; @return (string)
  [n length]
  (letfn [(f0 [%] (if (->  % count (< length))
                      (->> % (str "0") f0)
                      (->  %)))
          (f1 [%] (if (-> % string/first-character (= "-"))
                      (str "-" (-> % str (subs 1) f0))
                      (str     (-> % str f0))))]
         (mixed/update-numeric-part n f1)))

(defn remove-leading-zeros
  ; @description
  ; Removes the leading zeros from the first numeric part of the given 'n' value.
  ;
  ; @param (number or string) n
  ;
  ; @usage
  ; (remove-leading-zeros "000123")
  ; =>
  ; "123"
  ;
  ; @usage
  ; (remove-leading-zeros "-000123")
  ; =>
  ; "-123"
  ;
  ; @return (string)
  [n]
  (letfn [(f0 [%] (if (-> % string/first-character (= "0"))
                      (-> % (subs 1) f0)
                      (-> %)))
          (f1 [%] (if (-> % string/first-character (= "-"))
                      (str "-" (-> % str (subs 1) f0))
                      (str     (-> % str f0))))]
         (mixed/update-numeric-part n f1)))

(defn fill-trailing-zeros
  ; @description
  ; Extends the first numeric part of the given 'n' value with trailing zeros to match the given length.
  ;
  ; @param (number or string) n
  ; @param (integer) length
  ;
  ; @usage
  ; (fill-trailing-zeros "123" 6)
  ; =>
  ; "123000"
  ;
  ; @return (string)
  [n length]
  (letfn [(f0 [%] (if (-> % count (< length))
                      (-> % (str "0") f0)
                      (-> %)))
          (f1 [%] (if (-> % string/first-character (= "-"))
                      (str "-" (-> % str (subs 1) f0))
                      (str     (-> % str f0))))]
         (mixed/update-numeric-part n f1)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn decimal-scale
  ; @note
  ; Scale is the number of digits to the right of the decimal point in a number.
  ;
  ; @description
  ; Cuts decimal places from the first numeric part of the given 'n' value, or extends it with zeros to match the given decimal scale.
  ;
  ; @param (number or string) n
  ; @param (integer)(opt) scale
  ; Default: 2
  ;
  ; @usage
  ; (decimal-scale "123" 3)
  ; =>
  ; "123.000"
  ;
  ; @usage
  ; (decimal-scale "123.0000" 3)
  ; =>
  ; "123.000"
  ;
  ; @usage
  ; (decimal-scale nil 3)
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n]
   (decimal-scale n 2))

  ([n scale]
   (letfn [(f0 [%] (if (-> % str empty?) (-> %)
                       (if-let [separator-position (string/first-dex-of % ".")]
                               (let [diff (-> % str count (- separator-position scale 1))]
                                    (cond (> diff 0) (subs % 0 (+ separator-position scale 1))
                                          (< diff 0) (str  % (string/repeat "0" (- diff)))
                                          (= diff 0) (->   %)))
                               (str % "." (string/repeat "0" scale)))))]
          (mixed/update-numeric-part n f0))))

(defn abbreviate-number
  ; @description
  ; Abbreviate the first found numeric part in the given 'n' value.
  ;
  ; @param (number or string) n
  ;
  ; @usage
  ; (abbreviate-number 1740)
  ; =>
  ; "1.7K"
  ;
  ; @usage
  ; (abbreviate-number 2023)
  ; =>
  ; "2K"
  ;
  ; @usage
  ; (abbreviate-number 1000420)
  ; =>
  ; "1M"
  ;
  ; @usage
  ; (abbreviate-number 1000420069)
  ; =>
  ; "1B"
  ;
  ; @return (string)
  [n]
  (letfn [(f0 [%] (-> % (string/insert-part "." -2)))
          ; A divided negative number can be 'ratio' type that cannot be rounded by the 'Math/round' function.
          ; Therefore, it must be converted to double type.
          (f1 [%] (cond (>= %  1000000000) (str (-> % (/ 100000000)        Math/round f0) "B")
                        (>= %  1000000)    (str (-> % (/    100000)        Math/round f0) "M")
                        (>= %  1000)       (str (-> % (/       100)        Math/round f0) "K")
                        (<= % -1000000000) (str (-> % (/ 100000000) double Math/round f0) "B")
                        (<= % -1000000)    (str (-> % (/    100000) double Math/round f0) "M")
                        (<= % -1000)       (str (-> % (/       100) double Math/round f0) "K")
                        :return            (str (-> % Math/round))))]
         (mixed/update-numeric-part n f1)))
