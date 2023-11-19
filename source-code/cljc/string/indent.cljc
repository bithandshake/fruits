
(ns string.indent
    (:require [clojure.string]
              [seqable.api :as seqable]
              [string.set  :as set]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn inline-position
  ; @description
  ; Returns the given cursor position's inline position (distance from the nearest preceeding newline / break character).
  ;
  ; @param (*) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (inline-position "\nabc\ndef" 7)
  ;
  ; @example
  ; (inline-position "\nabc\ndef" 7)
  ; =>
  ; 2
  ;
  ; @example
  ; (inline-position "\nabc\rdef" 6)
  ; =>
  ; 1
  ;
  ; @example
  ; (inline-position "abc" 3)
  ; =>
  ; 3
  ;
  ; @return (integer)
  [n cursor]
  (let [n      (str n)
        cursor (seqable/normalize-cursor n cursor)]
       (min (if-let [newline-position (clojure.string/last-index-of (subs n 0 cursor) "\n")]
                    (-> cursor (- newline-position 1))
                    (-> cursor))
            (if-let [break-position (clojure.string/last-index-of (subs n 0 cursor) "\r")]
                    (-> cursor (- break-position 1))
                    (-> cursor)))))

(defn position-indent-length
  ; @description
  ; Returns the whitespace count preceeding the given cursor position whithin the actual line.
  ;
  ; @param (*) n
  ; @param (integer) cursor
  ;
  ; @usage
  ; (position-indent-length "\n   abc" 4)
  ;
  ; @example
  ; (position-indent-length "\n   abc" 4)
  ; =>
  ; 3
  ;
  ; @example
  ; (position-indent-length "\n   abc" 5)
  ; =>
  ; 0
  ;
  ; @example
  ; (position-indent-length "   abc" 3)
  ; =>
  ; 3
  ;
  ; @return (integer)
  [n cursor]
  (let [n      (str n)
        cursor (seqable/normalize-cursor n cursor)]
       (loop [length 0]
             (cond (-> cursor (- length) zero?)                 (-> length)
                   (-> n (nth (- cursor length 1)) str (= " ")) (-> length inc recur)
                   :return length))))

(defn fix-inline-position
  ; @description
  ; Ensures that the given cursor position has a specific inline position by
  ; adding / removing extra whitespaces before the cursor position or even inserting
  ; a newline character if needed.
  ;
  ; @param (*) n
  ; @param (integer) cursor
  ; @param (integer) indent
  ;
  ; @usage
  ; (fix-inline-position "abc" 0 2)
  ;
  ; @example
  ; (fix-inline-position "abc" 0 2)
  ; =>
  ; "  abc"
  ;
  ; @example
  ; (fix-inline-position "abcdef" 3 6)
  ; =>
  ; "abc   def"
  ;
  ; @example
  ; (fix-inline-position "abcdef" 3 1)
  ; =>
  ; "abc\n def"
  ;
  ; @return (string)
  [n cursor inline-position]
  (let [n      (str n)
        cursor (seqable/normalize-cursor n cursor)]
       (let [surplus (position-indent-length n cursor)
             shift   (- cursor inline-position)]
            (cond (= shift 0)        (-> n)
                  (< shift 0)        (str (subs n 0 cursor)
                                          (set/repeat " " (- shift))
                                          (subs n   cursor))
                  (>= surplus shift) (str (subs n 0 (- cursor shift))
                                          (subs n   cursor))
                  :surplus<shift     (str (subs n 0 cursor) "\n"
                                          (set/repeat " " inline-position)
                                          (subs n   cursor))))))
