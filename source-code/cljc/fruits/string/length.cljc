
(ns fruits.string.length
    (:require [fruits.seqable.api :as seqable]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn length
  ; @description
  ; Returns the length of the given 'n' string.
  ;
  ; @param (string) n
  ;
  ; @usage
  ; (length "One Flew Over the Cuckoo's Nest")
  ; =>
  ; 31
  ;
  ; @usage
  ; (length [])
  ; =>
  ; 2
  ;
  ; @return (integer)
  [n]
  (let [n (str n)]
       (if (-> n empty?)
           (-> 0)
           (-> n count))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn same-length?
  ; @description
  ; Returns TRUE if the given 'a' and 'b' strings have a the same length.
  ;
  ; @param (string) a
  ; @param (string) b
  ;
  ; @usage
  ; (same-length? "abc" "def")
  ; =>
  ; true
  ;
  ; @usage
  ; (same-length? "abc" "defghi")
  ; =>
  ; false
  ;
  ; @return (boolean)
  [a b]
  (= (-> a str count)
     (-> b str count)))

(defn length-min?
  ; @description
  ; Returns TRUE if the length of the given 'n' string is not smaller than the given 'min' value.
  ;
  ; @param (string) n
  ; @param (integer) min
  ;
  ; @usage
  ; (length-min? "abc" 3)
  ; =>
  ; true
  ;
  ; @usage
  ; (length-min? "abc" 4)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n min]
  (let [n (str n)]
       (and (-> min integer?)
            (<= min (-> n count)))))

(defn length-max?
  ; @description
  ; Returns TRUE if the length of the given 'n' string is not greater than the given 'max' value.
  ;
  ; @param (string) n
  ; @param (integer) max
  ;
  ; @usage
  ; (length-max? "abc" 3)
  ; =>
  ; true
  ;
  ; @usage
  ; (length-max? "abc" 2)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n max]
  (let [n (str n)]
       (and (-> max integer?)
            (>= max (-> n count)))))

(defn length-between?
  ; @description
  ; Returns TRUE if the length of the given 'n' string falls between the given 'min' and 'max' values.
  ;
  ; @param (string) n
  ; @param (integer) min
  ; @param (integer) max
  ;
  ; @usage
  ; (length-between? "abc" 3 4)
  ; =>
  ; true
  ;
  ; @usage
  ; (length-between? "abc" 2 4)
  ; =>
  ; true
  ;
  ; @return (boolean)
  [n min max]
  (let [n (str n)]
       (and (<= min (count n))
            (>= max (count n)))))

(defn length?
  ; @description
  ; Returns TRUE if the length of the given 'n' string matches the given 'length' value.
  ;
  ; @param (string) n
  ; @param (integer) length
  ;
  ; @usage
  ; (length? "abc" 3)
  ; =>
  ; true
  ;
  ; @usage
  ; (length? "abc" 2)
  ; =>
  ; false
  ;
  ; @return (boolean)
  [n length]
  (let [n (str n)]
       (= length (-> n count))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn max-length
  ; @description
  ; Ensures that the given 'n' string is not longer than the given 'limit' value.
  ;
  ; @param (string) n
  ; @param (integer) limit
  ; @param (string)(opt) suffix
  ;
  ; @usage
  ; (max-length "One Flew Over the Cuckoo's Nest" 10)
  ; =>
  ; "One Flew O"
  ;
  ; @usage
  ; (max-length "One Flew Over the Cuckoo's Nest" 10 " ...")
  ; =>
  ; "One Flew O ..."
  ;
  ; @usage
  ; (max-length nil 10)
  ; =>
  ; ""
  ;
  ; @return (string)
  [n limit & [suffix]]
  (let [n (str n)]
       (if (and (-> n empty? not)
                (-> n (seqable/dex-in-bounds? limit)))
           (str (subs n 0 limit) suffix)
           (-> n))))
