
(ns fruits.mixed.convert
    (:require [fruits.mixed.check :as check]
              [fruits.mixed.derive :as derive]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn to-kv
  ; @note
  ; Possible key-value pairs are two-item collections or maps with one key-value pair.
  ;
  ; @description
  ; Converts the given 'n' value into a key-value pair vector in case it is a possible key-value pair.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-kv [:a "A"])
  ; =>
  ; [:a "A"]
  ;
  ; @usage
  ; (to-kv '(:a "A"))
  ; =>
  ; [:a "A"]
  ;
  ; @usage
  ; (to-kv {:a "A"})
  ; =>
  ; [:a "A"]
  ;
  ; @usage
  ; (to-kv "ab")
  ; =>
  ; nil
  ;
  ; @usage
  ; (to-kv nil)
  ; =>
  ; nil
  ;
  ; @usage
  ; (to-kv [:a :b :c])
  ; =>
  ; nil
  ;
  ; @return (vector)
  [n]
  (cond (and (-> n map?)  (-> n count (= 1))) [(-> n keys first) (-> n vals first)] ;  {:a "A"} -> [:a "A"]
        (and (-> n coll?) (-> n count (= 2))) [(-> n first)      (-> n second)]))   ; '(:a "A") -> [:a "A"]

(defn to-kvs
  ; @note
  ; Possible key-value pairs are two-item collections or maps with one key-value pair.
  ;
  ; @description
  ; Converts the items within the given 'n' value (if collection) into a key-value pair vectors in case they are possible key-value pairs.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-kvs [[:a "A"] '(:b "B") {:c "C"}])
  ; =>
  ; [[:a "A"] [:b "B"] [:c "C"]]
  ;
  ; @usage
  ; (to-kvs '([:a "A"] '(:b "B") {:c "C"}))
  ; =>
  ; [[:a "A"] [:b "B"] [:c "C"]]
  ;
  ; @usage
  ; (to-kvs ["ab" "cd" "ef"])
  ; =>
  ; nil
  ;
  ; @usage
  ; (to-kvs [nil])
  ; =>
  ; nil
  ;
  ; @usage
  ; (to-kvs [[:a :b :c]])
  ; =>
  ; nil
  ;
  ; @return (vectors in vector)
  [n]
  (if (->  n check/kvs?)
      (->> n (map to-kv) vec)
      (->  [])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn to-data-url
  ; @description
  ; Converts the given 'n' value into a string type data URL.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-data-url "My text file content")
  ; =>
  ; "data:text/plain;charset=utf-8,My text file content"
  ;
  ; @return (string)
  [n]
  (str "data:text/plain;charset=utf-8," n))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn to-string
  ; @description
  ; Converts the given 'n' value into a string.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-string [{:a "a"}])
  ; =>
  ; "[{:a a}]"
  ;
  ; @return (string)
  [n]
  (str n))

(defn to-vector
  ; @description
  ; Converts the given 'n' value into a vector.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-vector [:a :b :c])
  ; =>
  ; [:a :b :c]
  ;
  ; @usage
  ; (to-vector nil)
  ; =>
  ; []
  ;
  ; @usage
  ; (to-vector {:a "A" :b "B" :c "C"})
  ; =>
  ; [[:a "A"] [:b "B"] [:c "C"]]
  ;
  ; @usage
  ; (to-vector :a)
  ; =>
  ; [:a]
  ;
  ; @usage
  ; (to-vector "abc")
  ; =>
  ; ["abc"]
  ;
  ; @return (vector)
  [n]
  (cond (vector? n) (-> n)     ;  [:a] -> [:a]
        (nil?    n) (-> [])    ;  nil  -> []
        (coll?   n) (-> n vec) ; '(:a) -> [:a], {:a "A"} -> [[:a "A"]]
        :else       (-> [n]))) ; :a    -> [:a]

(defn to-map
  ; @description
  ; Converts the given 'n' value into a map.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-map {:a "A" :b "B" :c "C"})
  ; =>
  ; {:a "A" :b "B" :c "C"}
  ;
  ; @usage
  ; (to-map nil)
  ; =>
  ; {}
  ;
  ; @usage
  ; (to-map [[:a "A"] [:b "B"] [:c "C"]])
  ; =>
  ; {:a "A" :b "B" :c "C"}
  ;
  ; @usage
  ; (to-map [:a :b :c])
  ; =>
  ; {0 [:a :b :c]}
  ;
  ; @usage
  ; (to-map "abc")
  ; =>
  ; {0 "abc"}
  ;
  ; @usage
  ; (to-map :a)
  ; =>
  ; {0 :a}
  ;
  ; @return (map)
  [n]
  (cond (map?       n) (->  n)                           ; {:a "A"}   -> {:a "A"}
        (nil?       n) (->  {})                          ; nil        -> {}
        (check/kvs? n) (->> n (map to-kv) vec (into {})) ; [[:a "A"]] -> {:a "A"}, '('(:a "A")) -> {:a "A"}
        :else          (-> {0 n})))                      ; "a"        -> {0  "A"}

(defn to-integer
  ; @description
  ; Converts the given 'n' value into an integer.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-integer 3)
  ; =>
  ; 3
  ;
  ; @usage
  ; (to-integer nil)
  ; =>
  ; 0
  ;
  ; @usage
  ; (to-integer "a")
  ; =>
  ; 0
  ;
  ; @usage
  ; (to-integer "-123")
  ; =>
  ; -123
  ;
  ; @usage
  ; (to-integer "123.456")
  ; =>
  ; 123
  ;
  ; @usage
  ; (to-integer "abc-123.456def789")
  ; =>
  ; -123
  ;
  ; @usage
  ; (to-integer "abc-008")
  ; =>
  ; 8
  ;
  ; @return (integer)
  [n]
  (or (derive/derive-integer n) 0))

(defn to-number
  ; @description
  ; Converts the given 'n' value into a number.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-number 3)
  ; =>
  ; 3
  ;
  ; @usage
  ; (to-number nil)
  ; =>
  ; 0
  ;
  ; @usage
  ; (to-number "abc")
  ; =>
  ; 0
  ;
  ; @usage
  ; (to-number "-123")
  ; =>
  ; -123
  ;
  ; @usage
  ; (to-number "123.456")
  ; =>
  ; 123.456
  ;
  ; @usage
  ; (to-number "abc-123.456def789")
  ; =>
  ; -123.456
  ;
  ; @usage
  ; (to-number "abc-008")
  ; =>
  ; 8
  ;
  ; @return (number)
  [n]
  (or (derive/derive-number n) 0))

(defn to-keyword
  ; @description
  ; Converts the given 'n' value into a keyword.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-keyword :a)
  ; =>
  ; :a
  ;
  ; @usage
  ; (to-keyword "abc")
  ; =>
  ; :abc
  ;
  ; @usage
  ; (to-keyword 123)
  ; =>
  ; :123
  ;
  ; @usage
  ; (to-keyword [:a :b :c])
  ; =>
  ; ::a
  ;
  ; @usage
  ; (to-keyword {:a "A" :b "B"})
  ; =>
  ; ::a
  ;
  ; @usage
  ; (to-keyword nil)
  ; =>
  ; :_
  ;
  ; @usage
  ; (to-keyword [])
  ; =>
  ; :_
  ;
  ; @return (*)
  [n]
  (or (derive/derive-keyword n) :_))

(defn to-fn
  ; @description
  ; Returns a noop function in case the given 'n' value is not a function.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-fn (fn [a b c] ...))
  ; =>
  ; (fn [a b c] ...)
  ;
  ; @usage
  ; (to-fn nil)
  ; =>
  ; (fn [& _])
  ;
  ; @usage
  ; (to-fn "abc")
  ; =>
  ; (fn [& _])
  ;
  ; @return (function)
  [n]
  (if (-> n fn?)
      (-> n)       ; (fn [& _]) -> (fn [& _])
      (fn [& _]))) ; "abc"      -> (fn [& _]), 123 -> (fn [& _])

(defn to-ifn
  ; @description
  ; Returns a noop function in case the given 'n' value does not implement the IFn protocol.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-ifn (fn [a b c] ...))
  ; =>
  ; (fn [a b c] ...)
  ;
  ; @usage
  ; (to-ifn :abc)
  ; =>
  ; :abc
  ;
  ; @usage
  ; (to-ifn nil)
  ; =>
  ; (fn [& _])
  ;
  ; @usage
  ; (to-ifn "abc")
  ; =>
  ; (fn [& _])
  ;
  ; @return (function)
  [n]
  (if (-> n ifn?)
      (-> n)       ; (fn [& _]) -> (fn [& _]), :a  -> :a
      (fn [& _]))) ; "abc"      -> (fn [& _]), 123 -> (fn [& _])

(defn to-nil
  ; @description
  ; Converts the given 'n' value into a NIL in case it is an empty value.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-nil [])
  ; =>
  ; nil
  ;
  ; @usage
  ; (to-nil [:a :b :c])
  ; =>
  ; [:a :b :c]
  ;
  ; @usage
  ; (to-nil "abc")
  ; =>
  ; "abc"
  ;
  ; @return (nil or nonempty *)
  [n]
  ; Alternative: 'not-empty'
  ; https://clojuredocs.org/clojure.core/not-empty
  (cond (-> n seqable? not) (-> n)
        (-> n empty?   not) (-> n)
        :else nil))

(defn to-seqable
  ; @description
  ; Converts the given 'n' value into a vector in case it does not implement the ISeqable protocol.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-seqable [:a :b :c])
  ; =>
  ; [:a :b :c]
  ;
  ; @usage
  ; (to-seqable "abc")
  ; =>
  ; "abc"
  ;
  ; @usage
  ; (to-seqable nil)
  ; =>
  ; nil
  ;
  ; @usage
  ; (to-seqable 123)
  ; =>
  ; [123]
  ;
  ; @return (*)
  [n]
  (if (-> n seqable?)
      (-> n)
      (-> [n])))

(defn to-associative
  ; @description
  ; Converts the given 'n' value into a vector in case it does not implement the IAssociative protocol.
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (to-associative [:a :b :c])
  ; =>
  ; [:a :b :c]
  ;
  ; @usage
  ; (to-associative "abc")
  ; =>
  ; ["abc"]
  ;
  ; @usage
  ; (to-associative nil)
  ; =>
  ; []
  ;
  ; @usage
  ; (to-associative 123)
  ; =>
  ; [123]
  ;
  ; @return (*)
  [n]
  (cond (-> n associative?) (-> n)     ;  {:a "A"} -> {:a "A"}, [:a] -> [:a]
        (-> n nil?)         (-> [])    ;  nil      -> []
        (-> n coll?)        (-> n vec) ; '(:a)     -> [:a], #{:a} -> [:a]
        :else               (-> [n]))) ;  :a       -> [:a]    ???
