
(ns string.set
    (:refer-clojure :exclude [repeat])
    (:require [clojure.string]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn repeat
  ; @param (*) n
  ; @param (integer) x
  ;
  ; @usage
  ; (repeat "a" 3)
  ;
  ; @example
  ; (repeat "a" 3)
  ; =>
  ; "aaa"
  ;
  ; @return (string)
  [n x]
  (if (nat-int? x)
      (letfn [(f [result]
                 (if (= (-> n      count (* x))
                        (-> result count))
                     (-> result)
                     (-> result (str n) f)))]
             (f ""))))

(defn join
  ; @param (collection) n
  ; @param (*) separator
  ; @param (map)(opt) options
  ; {:join-empty? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (join ["a" "b"] ".")
  ;
  ; @example
  ; (join ["my-image" "png"] ".")
  ; =>
  ; "my-image.png"
  ;
  ; @example
  ; (join ["a" "b" ""] ".")
  ; =>
  ; "a.b."
  ;
  ; @example
  ; (join ["a" "b" ""] "." {:join-empty? false})
  ; =>
  ; "a.b"
  ;
  ; @return (string)
  ([n separator]
   (join n separator {}))

  ([n separator {:keys [join-empty?] :or {join-empty? true}}]
   (letfn [(f [result dex]
              (cond ; ...
                    (-> n count (= dex))
                    (-> result)
                    ; ...
                    (or join-empty? (-> n (nth dex) empty? not))
                    (if (and (-> n count dec (not= dex))
                             (-> (nth n (inc dex)) str empty? not))
                        (str result (nth n dex) separator)
                        (str result (nth n dex)))
                    ; ...
                    :return result))]
          ; ...
          (if (seqable? n)
              (f "" 0)))))

(defn split
  ; @param (*) n
  ; @param (clj: regex, cljs: regex or string) delimiter
  ;
  ; @example
  ; (split "a.b.c" ".")
  ; =>
  ; ["a" "b" "c"]
  ;
  ; @example
  ; (split "a.b.c" #".")
  ; =>
  ; []
  ;
  ; @example
  ; (split "a.b.c" #"[.]")
  ; =>
  ; ["a" "b" "c"]
  ;
  ; @example
  ; (split ".b.c" #"[.]")
  ; =>
  ; ["" "b" "c"]
  ;
  ; @example
  ; (split "a.b.c" #"_")
  ; =>
  ; ["a.b.c"]
  ;
  ; @example
  ; (split "" #".")
  ; =>
  ; []
  ;
  ; @return (strings in vector)
  [n delimiter]
  (let [n (str n)]
       (cond (-> n empty?)        []
             (-> delimiter some?) (clojure.string/split n delimiter)
             :return              [n])))
