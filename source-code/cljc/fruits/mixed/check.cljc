
(ns fruits.mixed.check
    (:refer-clojure :exclude [empty?]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn empty?
  ; @description
  ; Returns TRUE if the given 'n' value is an empty seqable value.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (empty? nil)
  ; =>
  ; true
  ;
  ; @usage
  ; (empty? "")
  ; =>
  ; true
  ;
  ; @usage
  ; (empty? [])
  ; =>
  ; true
  ;
  ; @usage
  ; (empty? {})
  ; =>
  ; true
  ;
  ; @usage
  ; (empty? {:a "A"})
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n]
  ; The 'clojure.core/empty?' function can be applied only on seqable values.
  ; (Nonseqable values cannot be empty, e.g., :keyword)
  (and (-> n seqable?)
       (-> n clojure.core/empty?)))

(defn not-empty?
  ; @description
  ; Returns TRUE if the given 'n' value is a nonempty seqable value or a nonseqable value.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (not-empty? nil)
  ; =>
  ; false
  ;
  ; @usage
  ; (not-empty? "")
  ; =>
  ; false
  ;
  ; @usage
  ; (not-empty? [])
  ; =>
  ; false
  ;
  ; @usage
  ; (not-empty? {})
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n]
  ; The 'clojure.core/empty?' function can be applied only on seqable values.
  ; (A) If the value of 'n' is not seqable, then it is considered as a nonempty value (e.g., keyword, integer, etc.).
  ; (B) If the value of 'n' is seqable, then it is checked whether is it empty (e.g., nil, map, string, vector, etc.)
  (or ; (A)
      (-> n seqable? not)
      ; (B)
      (-> n clojure.core/empty? not)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn kv?
  ; @description
  ; Returns TRUE if the given 'n' value is a possible key-value pair.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (kv? [:a "A"])
  ; =>
  ; true
  ;
  ; @usage
  ; (kv? '(:a "A"))
  ; =>
  ; true
  ;
  ; @usage
  ; (kv? {:a "A"})
  ; =>
  ; true
  ;
  ; @usage
  ; (kv? "ab")
  ; =>
  ; false
  ;
  ; @usage
  ; (kv? nil)
  ; =>
  ; false
  ;
  ; @usage
  ; (kv? [:a :b :c])
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n]
  (or (and (-> n map?)  (-> n count (= 1)))
      (and (-> n coll?) (-> n count (= 2)))))

(defn kvs?
  ; @description
  ; Returns TRUE if the given 'n' value is a collection of possible key-value pairs.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (kvs? [[:a "A"] '(:b "B") {:c "C"}])
  ; =>
  ; true
  ;
  ; @usage
  ; (kvs? '([:a "A"] '(:b "B") {:c "C"}))
  ; =>
  ; true
  ;
  ; @usage
  ; (kvs? ["ab" "cd" "ef"])
  ; =>
  ; false
  ;
  ; @usage
  ; (kvs? [nil])
  ; =>
  ; false
  ;
  ; @usage
  ; (kvs? [[:a :b :c]])
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n]
  (and (coll? n)
       (every? kv? n)))
