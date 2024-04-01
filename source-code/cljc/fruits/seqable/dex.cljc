
(ns fruits.seqable.dex
    (:require [fruits.mixed.api :as mixed]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn normalize-dex
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; - Normalizes the given index value of the given 'n' sequence.
  ; - The output is a non-negative index value that is not less than 0 and not more than the last index value.
  ; - The output could be NIL, in case the ':adjust?' parameter is not TRUE!
  ;
  ; @param (*) string
  ; @param (integer) dex
  ; {:adjust? (boolean)(opt)
  ;   If TRUE, adjusts out of bound index values, otherwise ignores them.
  ;   Default: false
  ;  :mirror? (boolean)(opt)
  ;   If TRUE, mirrors negative index values, otherwise handles them as out of bound indices.
  ;   Default: false}
  ;
  ; @usage
  ; (normalize-dex [:a :b :c] -2)
  ; =>
  ; 0
  ;
  ; @usage
  ; (normalize-dex [:a :b :c] -5)
  ; =>
  ; 0
  ;
  ; @usage
  ; (normalize-dex [:a :b :c] 5)
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n dex]
   (normalize-dex n dex {}))

  ([n dex {:keys [adjust? mirror?]}]
   (let [n   (mixed/to-seqable n)
         dex (mixed/to-integer dex)]
        (letfn [(f0 [%] (-> n count (+ %)))
                (f1 [%] (-> n count (> %)))]
               (if (-> dex neg-int?)
                   (cond (-> mirror?) (-> n (normalize-dex (f0 dex) {:adjust? adjust?})) ; Negative index                    -> mirroring.
                         (-> adjust?) (-> 0))                                            ; Negative index, possibly mirrored -> adjusting.
                   (cond (-> dex f1)  (-> dex)                                           ; Positive index, in bounds         -> returning.
                         (-> adjust?) (-> n count dec)))))))                             ; Positive index, out of bounds     -> adjusting.

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn dex-in-bounds?
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns TRUE if the given 'dex' value falls between 0 and the highest possible index value (= item count minus one).
  ;
  ; @param (*) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (dex-in-bounds? [:a :b :c] 2)
  ; =>
  ; true
  ;
  ; @usage
  ; (dex-in-bounds? [:a :b :c] 3)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n dex]
  (let [n (mixed/to-seqable n)]
       (and (-> dex nat-int?)
            (-> n count (> dex)))))

(defn dex-out-of-bounds?
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns TRUE if the given 'dex' value does not fall between 0 and the highest possible index value (= item count minus one).
  ;
  ; @param (*) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (dex-out-of-bounds? [:a :b :c] 3)
  ; =>
  ; true
  ;
  ; @usage
  ; (dex-out-of-bounds? [:a :b :c] 2)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n dex]
  (let [n (mixed/to-seqable n)]
       (or (-> dex nat-int? not)
           (-> n count (<= dex)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn next-dex
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the index that follows the given 'dex' value of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (next-dex [:a :b :c] 1)
  ; =>
  ; 2
  ;
  ; @usage
  ; (next-dex [:a :b :c] 2)
  ; =>
  ; 0
  ;
  ; @return (integer)
  [n dex]
  (let [n   (mixed/to-seqable n)
        dex (mixed/to-integer dex)]
       (cond (-> n count zero?)        (-> nil)
             (-> n count dec (<= dex)) (-> 0)
             (-> dex (< 0))            (-> 0)
             :return                   (-> dex inc))))

(defn prev-dex
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the index that precedes the given 'dex' value of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (prev-dex [:a :b :c] 1)
  ; =>
  ; 0
  ;
  ; @usage
  ; (prev-dex [:a :b :c] 0)
  ; =>
  ; 2
  ;
  ; @return (integer)
  [n dex]
  (let [n   (mixed/to-seqable n)
        dex (mixed/to-integer dex)]
       (cond (-> n count zero?)       (-> nil)
             (-> dex (<= 0))          (-> n count dec)
             (-> n count dec (< dex)) (-> n count dec)
             :return                  (-> dex dec))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn dex-range
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns a vector of possible indices of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ;
  ; @usage
  ; (dex-range [:a :b :c])
  ; =>
  ; [0 1 2]
  ;
  ; @return (vector)
  [n]
  (let [n (mixed/to-seqable n)]
       (-> n count range vec)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn dex-first?
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns TRUE if the given 'dex' value is the index of the first item (if any) of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (dex-first? [:a :b :c] 0)
  ; =>
  ; true
  ;
  ; @usage
  ; (dex-first? "abc" 0)
  ; =>
  ; true
  ;
  ; @usage
  ; (dex-first? "abc" 1)
  ; =>
  ; false
  ;
  ; @usage
  ; (dex-first? [] 0)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n dex]
  (let [n (mixed/to-seqable n)]
       (and (-> n count zero? not)
            (-> dex zero?))))

(defn dex-last?
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns TRUE if the given 'dex' value is the index of the last item (if any) of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (dex-last? [:a :b :c] 2)
  ; =>
  ; true
  ;
  ; @usage
  ; (dex-last? "abc" 2)
  ; =>
  ; true
  ;
  ; @usage
  ; (dex-last? "abc" 1)
  ; =>
  ; false
  ;
  ; @usage
  ; (dex-last? [] 0)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n dex]
  (let [n (mixed/to-seqable n)]
       (and (-> n count zero? not)
            (-> n count dec (= dex)))))

(defn first-dex
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the first index of the first item (if any) of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ;
  ; @usage
  ; (first-dex [:a :b :c])
  ; =>
  ; 0
  ;
  ; @usage
  ; (first-dex "abc")
  ; =>
  ; 0
  ;
  ; @return (integer)
  [n]
  (let [n (mixed/to-seqable n)]
       (if (-> n count zero? not)
           (-> 0))))

(defn last-dex
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the index of the last item (if any) of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ;
  ; @usage
  ; (last-dex [:a :b :c])
  ; =>
  ; 2
  ;
  ; @usage
  ; (last-dex "abc")
  ; =>
  ; 2
  ;
  ; @return (integer)
  [n]
  (let [n (mixed/to-seqable n)]
       (if (-> n count zero? not)
           (-> n count dec))))

(defn new-dex
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the next index that follows the index of the last item (if any) of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ;
  ; @usage
  ; (new-dex [:a :b :c])
  ; =>
  ; 3
  ;
  ; @usage
  ; (new-dex "abc")
  ; =>
  ; 3
  ;
  ; @usage
  ; (new-dex [])
  ; =>
  ; 0
  ;
  ; @return (integer)
  [n]
  (let [n (mixed/to-seqable n)]
       (-> n count)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn inc-dex
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Increments the given 'dex' value in case the result falls between the bounds of the given 'n' sequence,
  ; otherwise returns the given 'dex' value.
  ;
  ; @param (seqable) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (inc-dex [:a :b :c] 1)
  ; =>
  ; 2
  ;
  ; @usage
  ; (inc-dex [:a :b :c] 2)
  ; =>
  ; 2
  ;
  ; @return (integer)
  [n dex]
  (let [n   (mixed/to-seqable n)
        dex (mixed/to-integer dex)]
       (cond (-> n count zero?)       (-> nil)
             (-> n count dec (= dex)) (-> dex)
             (-> n count dec (< dex)) (-> n count dec)
             (-> dex         (<   0)) (-> 0)
             :else                    (-> dex inc))))

(defn dec-dex
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Decrements the given 'dex' value in case the result falls between the bounds of the given 'n' sequence,
  ; otherwise returns the given 'dex' value.
  ;
  ; @param (seqable) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (dec-dex [:a :b :c] 2)
  ; =>
  ; 1
  ;
  ; @usage
  ; (dec-dex [:a :b :c] 0)
  ; =>
  ; 0
  ;
  ; @return (integer)
  [n dex]
  (let [n   (mixed/to-seqable n)
        dex (mixed/to-integer dex)]
       (cond (-> n count zero?)       (-> nil)
             (-> n count dec (< dex)) (-> n count dec)
             (-> dex         (<   2)) (-> 0)
             :else                    (-> dex dec))))
