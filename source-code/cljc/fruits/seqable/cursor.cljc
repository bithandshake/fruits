
(ns fruits.seqable.cursor
    (:require [fruits.mixed.api :as mixed]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn normalize-cursor
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; - Normalizes the given cursor value of the given 'n' sequence.
  ; - The output is a non-negative cursor value that is not less than 0 and not more than the last cursor value.
  ; - The output could be NIL, in case the ':adjust?' parameter is not TRUE!
  ;
  ; @param (*) n
  ; @param (integer) cursor
  ; @param (map)(opt) options
  ; {:adjust? (boolean)(opt)
  ;   If TRUE, adjusts out of bound cursor values, otherwise ignores them.
  ;   Default: false
  ;  :mirror? (boolean)(opt)
  ;   If TRUE, mirrors negative cursor values, otherwise handles them as out of bound cursors.
  ;   Default: false}
  ;
  ; @usage
  ; (normalize-cursor [:a :b :c] 1)
  ; =>
  ; 1
  ;
  ; @usage
  ; (normalize-cursor [:a :b :c] 5)
  ; =>
  ; nil
  ;
  ; @usage
  ; (normalize-cursor [:a :b :c] 5 {:adjust? true})
  ; =>
  ; 0
  ;
  ; @usage
  ; (normalize-cursor [:a :b :c] -1)
  ; =>
  ; nil
  ;
  ; @usage
  ; (normalize-cursor [:a :b :c] -1 {:mirror? true})
  ; =>
  ; 3
  ;
  ; @return (integer)
  ([n cursor]
   (normalize-cursor n cursor {}))

  ([n cursor {:keys [adjust? mirror?]}]
   (let [n      (mixed/to-seqable n)
         cursor (mixed/to-integer cursor)]
        (letfn [(f0 [%] (-> n count inc (+ %)))
                (f1 [%] (-> n count (>= %)))]
               (if (-> cursor neg-int?)
                   (cond (-> mirror?)   (-> n (normalize-cursor (f0 cursor) {:adjust? adjust?})) ; Negative cursor                    -> mirroring.
                         (-> adjust?)   (-> 0))                                                  ; Negative cursor, possibly mirrored -> adjusting.
                   (cond (-> cursor f1) (-> cursor)                                              ; Positive cursor, in bounds         -> returning.
                         (-> adjust?)   (-> n count)))))))                                       ; Positive cursor, out of bounds     -> adjusting.

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn cursor-in-bounds?
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns TRUE if the given 'cursor' value falls between 0 and the highest possible cursor value (= item count).
  ;
  ; @param (*) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (cursor-in-bounds? [:a :b :c] 3)
  ; =>
  ; true
  ;
  ; @usage
  ; (cursor-in-bounds? [:a :b :c] 4)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n cursor]
  (let [n (mixed/to-seqable n)]
       (and (-> cursor nat-int?)
            (-> n count (>= cursor)))))

(defn cursor-out-of-bounds?
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns TRUE if the given 'cursor' value does not fall between 0 and the highest possible cursor value (= item count).
  ;
  ; @param (*) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (cursor-out-of-bounds? [:a :b :c] 4)
  ; =>
  ; true
  ;
  ; @usage
  ; (cursor-out-of-bounds? [:a :b :c] 3)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n cursor]
  (let [n (mixed/to-seqable n)]
       (or (-> cursor nat-int? not)
           (-> n count (< cursor)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn next-cursor
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the cursor that follows the given 'cursor' value of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (next-cursor [:a :b :c] 1)
  ; =>
  ; 2
  ;
  ; @usage
  ; (next-cursor [:a :b :c] 3)
  ; =>
  ; 0
  ;
  ; @return (integer)
  [n cursor]
  (let [n      (mixed/to-seqable n)
        cursor (mixed/to-integer cursor)]
       (cond (-> n count (<= cursor)) (-> 0)
             (-> cursor (< 0))        (-> 0)
             :return                  (-> cursor inc))))

(defn prev-cursor
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the cursor that precedes the given 'cursor' value of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (prev-cursor [:a :b :c] 1)
  ; =>
  ; 0
  ;
  ; @usage
  ; (prev-cursor [:a :b :c] 0)
  ; =>
  ; 3
  ;
  ; @return (integer)
  [n cursor]
  (let [n      (mixed/to-seqable n)
        cursor (mixed/to-integer cursor)]
       (cond (-> cursor (<= 0))      (-> n count)
             (-> n count (< cursor)) (-> n count)
             :return                 (-> cursor dec))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn cursor-range
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns a vector of possible cursors of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ;
  ; @usage
  ; (cursor-range [:a :b :c])
  ; =>
  ; [0 1 2 3]
  ;
  ; @return (vector)
  [n]
  (let [n (mixed/to-seqable n)]
       (-> n count inc range vec)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn cursor-first?
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns TRUE if the given 'cursor' value is the first cursor of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ; @param (cursor) cursor
  ;
  ; @usage
  ; (cursor-first? [:a :b :c] 0)
  ; =>
  ; true
  ;
  ; @usage
  ; (cursor-first? "abc" 0)
  ; =>
  ; true
  ;
  ; @usage
  ; (cursor-first? "abc" 1)
  ; =>
  ; false
  ;
  ; @usage
  ; (cursor-first? [] 0)
  ; =>
  ; true
  ;
  ; @return (boolean)
  [_ cursor]
  (-> cursor (= 0)))

(defn cursor-last?
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns TRUE if the given 'cursor' value is the last cursor of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (cursor-last? [:a :b :c] 3)
  ; =>
  ; true
  ;
  ; @usage
  ; (cursor-last? "abc" 3)
  ; =>
  ; true
  ;
  ; @usage
  ; (cursor-last? "abc" 2)
  ; =>
  ; false
  ;
  ; @usage
  ; (cursor-last? [] 0)
  ; =>
  ; true
  ;
  ; @return (boolean)
  [n cursor]
  (let [n (mixed/to-seqable n)]
       (-> n count (= cursor))))

(defn first-cursor
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the first cursor of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ;
  ; @usage
  ; (first-cursor [:a :b :c])
  ; =>
  ; 0
  ;
  ; @usage
  ; (first-cursor "abc")
  ; =>
  ; 0
  ;
  ; @return (integer)
  [n]
  (-> 0))

(defn last-cursor
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Returns the last cursor of the given 'n' sequence.
  ;
  ; @param (seqable) n
  ;
  ; @usage
  ; (last-cursor [:a :b :c])
  ; =>
  ; 3
  ;
  ; @usage
  ; (last-cursor "abc")
  ; =>
  ; 3
  ;
  ; @return (integer)
  [n]
  (let [n (mixed/to-seqable n)]
       (-> n count)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn inc-cursor
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Increments the given 'cursor' value in case the result falls between the bounds of the given 'n' sequence,
  ; otherwise returns the given 'cursor' value.
  ;
  ; @param (seqable) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (inc-cursor [:a :b :c] 2)
  ; =>
  ; 3
  ;
  ; @usage
  ; (inc-cursor [:a :b :c] 3)
  ; =>
  ; 3
  ;
  ; @return (integer)
  [n cursor]
  (let [n      (mixed/to-seqable n)
        cursor (mixed/to-integer cursor)]
       (cond (-> n count (= cursor)) (-> cursor)
             (-> n count (< cursor)) (-> n count)
             (-> cursor  (<      0)) (-> 0)
             :else                   (-> cursor inc))))

(defn dec-cursor
  ; @note
  ; An N item long sequence has N+1 cursors and N indices.
  ; E.g., Indices of "abc": 0, 1, 2
  ;       Cursors of "abc": 0, 1, 2, 3
  ;
  ; @description
  ; Decrements the given 'cursor' value in case the result falls between the bounds of the given 'n' sequence,
  ; otherwise returns the given 'cursor' value.
  ;
  ; @param (seqable) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (dec-cursor [:a :b :c] 3)
  ; =>
  ; 2
  ;
  ; @usage
  ; (dec-cursor [:a :b :c] 0)
  ; =>
  ; 0
  ;
  ; @return (integer)
  [n cursor]
  (let [n      (mixed/to-seqable n)
        cursor (mixed/to-integer cursor)]
       (cond (-> n count (< cursor)) (-> n count)
             (-> cursor  (<      2)) (-> 0)
             :else                   (-> cursor dec))))
