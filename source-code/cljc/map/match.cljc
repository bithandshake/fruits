
(ns map.match
    (:require [map.core :as core]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn any-key-matches?
  ; @description
  ; Returns TRUE if the given 'test-f' function returns TRUE with any key of the given 'n' map as its parameter.
  ;
  ; @param (map) n
  ; @param (function) test-f
  ;
  ; @usage
  ; (any-key-matches? {:a "A"} keyword?)
  ;
  ; @example
  ; (any-key-matches? {:a "A" "b" "B"} string?)
  ; =>
  ; true
  ;
  ; @example
  ; (any-key-matches? {:a "A" :b "B"} string?)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n test-f]
  (letfn [(f [%] (-> % first test-f))]
         (boolean (some f n))))

(defn any-value-matches?
  ; @description
  ; Returns TRUE if the given 'test-f' function returns TRUE with any value of the given 'n' map as its parameter.
  ;
  ; @param (map) n
  ; @param (function) test-f
  ;
  ; @usage
  ; (any-value-matches? {:a "A"} string?)
  ;
  ; @example
  ; (any-value-matches? {:a :A :b "B"} string?)
  ; =>
  ; true
  ;
  ; @example
  ; (any-value-matches? {:a "A" :b "B"} keyword?)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n test-f]
  (letfn [(f [%] (-> % second test-f))]
         (boolean (some f n))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn all-keys-match?
  ; @description
  ; Returns TRUE if the given 'test-f' function returns TRUE with all keys of the given 'n' map as its parameter.
  ;
  ; @param (map) n
  ; @param (function) test-f
  ;
  ; @example
  ; (all-keys-match? {:a "A"} keyword?)
  ;
  ; @example
  ; (all-keys-match? {:a "A" :b "B"} keyword?)
  ; =>
  ; true
  ;
  ; @example
  ; (all-keys-match? {:a "A" "b" "B"} keyword?)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n test-f]
  (letfn [(f [%] (-> % first test-f))]
         (every? f n)))

(defn all-values-match?
  ; @description
  ; Returns TRUE if the given 'test-f' function returns TRUE with all values of the given 'n' map as its parameter.
  ;
  ; @param (map) n
  ; @param (function) test-f
  ;
  ; @example
  ; (all-values-match? {:a "A"} string?)
  ;
  ; @example
  ; (all-values-match? {:a "A" :b "B"} string?)
  ; =>
  ; true
  ;
  ; @example
  ; (all-values-match? {:a :A :b "B"} string?)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n test-f]
  (letfn [(f [%] (-> % second test-f))]
         (every? f n)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn first-match-key
  ; @warning
  ; Clojure maps are unordered data structures.
  ;
  ; @description
  ; Returns the first key of the given 'n' map for which the given 'test-f' function returns TRUE when applied to the corresponding value of the key.
  ;
  ; @param (map) n
  ; @param (function) test-f
  ;
  ; @usage
  ; (first-match-key {:a "A"} string?)
  ;
  ; @example
  ; (first-match-key {:a "A" :b "B"} string?)
  ; =>
  ; :a
  ;
  ; @example
  ; (first-match-key {:a "A" :b "B"} keyword?)
  ; =>
  ; nil
  ;
  ; @return (*)
  [n test-f]
  (letfn [(f [%] (if (-> % second test-f)
                     (-> % first)))]
         (some f n)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn first-matching-key
  ; @warning
  ; Clojure maps are unordered data structures.
  ;
  ; @description
  ; Returns the first key of the given 'n' map for which the given 'test-f' function returns TRUE when applied to all keys of the 'n' map.
  ;
  ; @param (map) n
  ; @param (function) test-f
  ;
  ; @usage
  ; (first-matching-key {:a "A"} keyword?)
  ;
  ; @example
  ; (first-matching-key {:a "A" "b" "B"} string?)
  ; =>
  ; "b"
  ;
  ; @example
  ; (first-matching-key {:a "A" :b "B"} string?)
  ; =>
  ; nil
  ;
  ; @return (*)
  [n test-f]
  (letfn [(f [%] (if (-> % first test-f)
                     (-> % first)))]
         (some f n)))

(defn first-matching-value
  ; @warning
  ; Clojure maps are unordered data structures.
  ;
  ; @description
  ; Returns the first value of the given 'n' map for which the given 'test-f' function returns TRUE when applied to all values of the 'n' map.
  ;
  ; @param (map) n
  ; @param (function) test-f
  ;
  ; @usage
  ; (first-matching-value {:a "A"} string?)
  ;
  ; @example
  ; (first-matching-value {:a :A :b "B"} string?)
  ; =>
  ; "B"
  ;
  ; @example
  ; (first-matching-value {:a {:id "apple"} :b {:id "banana"}} #(= "apple" (:id %)))
  ; =>
  ; {:id "apple"}
  ;
  ; @example
  ; (first-matching-value {:a "A" :b "B"} keyword?)
  ; =>
  ; nil
  ;
  ; @return (*)
  [n test-f]
  (letfn [(f [%] (if (-> % second test-f)
                     (-> % second)))]
         (some f n)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn matches-pattern?
  ; @description
  ; Returns TRUE if all key-value pairs in the 'pattern' map are present in the 'n' map and, optionally, checks for strict matching.
  ;
  ; @param (map) n
  ; @param (map) pattern
  ; @param (map)(opt) options
  ; {:strict-matching? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (matches-pattern? {:a "A" :b "B"} {:a "A"})
  ;
  ; @example
  ; (matches-pattern? {:a "A" :b "B"} {:a "A"})
  ; =>
  ; true
  ;
  ; @example
  ; (matches-pattern? {:a "A" :b "B"} {:a "A" :c "C"})
  ; =>
  ; false
  ;
  ; @example
  ; (matches-pattern? {:a "A" :b "B"} {:a "A"} {:strict-matching? true})
  ; =>
  ; false
  ;
  ; @example
  ; (matches-pattern? {:a "A" :b "B"} {:a "A" :b "B"} {:strict-matching? true})
  ; =>
  ; true
  ;
  ; @return (boolean)
  ([n pattern]
   (matches-pattern? n pattern {}))

  ([n pattern {:keys [strict-matching?]}]
   (let [difference (core/difference n pattern)]
        (or ; If 'strict-matching?' is TRUE ...
            (and (not strict-matching?)
                 (= (count n)
                    (+ (count difference)
                       (count pattern))))
            ; If 'strict-matching?' is FALSE ...
            (and (boolean strict-matching?)
                 (= (count n)
                    (count pattern))
                 (empty? difference))))))
