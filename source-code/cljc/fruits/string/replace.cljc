
(ns fruits.string.replace
    (:require [clojure.string]
              [fruits.string.check :as check]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn replace-part
  ; @description 
  ; Replaces the given 'x' value (optionally recursively) with the given 'y' string in the given 'n' string.
  ;
  ; @param (string) n
  ; @param (regex pattern or string) x
  ; @param (string) replacement
  ; @param (map)(opt) options
  ; {:recur? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (replace-part "abc" "b" "x")
  ; =>
  ; "axc"
  ;
  ; @usage
  ; (replace-part "abc" #"[b]*" "x")
  ; =>
  ; "axc"
  ;
  ; @usage
  ; (replace-part "abc" "b" nil)
  ; =>
  ; "ac"
  ;
  ; @usage
  ; (replace-part "///" "//" "/")
  ; =>
  ; "//"
  ;
  ; @usage
  ; (replace-part "///" "//" "/" {:recur? true})
  ; =>
  ; "/"
  ;
  ; @return (string)
  ([n x replacement]
   (replace-part n x replacement {}))

  ([n x replacement {:keys [recur?]}]
   (letfn [(f0 [n] (clojure.string/replace (str n) x
                                           (str replacement)))
           (f1 [n] (if (-> n f0 (= n))
                       (-> n)
                       (-> n f0 f1)))]
          (if recur? (f1 n)
                     (f0 n)))))

(defn use-replacements
  ; @note
  ; Replacement markers contain numbers in case of multiple replacement is provided.
  ; If only one replacement is provided, the marker is a single percent sign character ('%').
  ;
  ; @description
  ; Replaces the markers in the given 'n' string with the corresponding values from the given 'replacements' vector.
  ;
  ; @param (string) n
  ; @param (vector) replacements
  ; @param (map)(opt) options
  ; {:ignore? (boolean)(opt)
  ;   The function returns NIL if any of the replacements is NIL or empty.
  ;   Default: true}
  ;
  ; @usage
  ; (use-replacements "Hi, my name is %" ["John"])
  ; =>
  ; "Hi, my name is John"
  ;
  ; @usage
  ; (use-replacements "My favorite colors are: %1, %2 and %3" ["red" "green" "blue"])
  ; =>
  ; "My favorite colors are: red, green and blue"
  ;
  ; @usage
  ; (use-replacements "%1 / %2 items downloaded" [nil 3])
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n replacements]
   (use-replacements n replacements {}))

  ([n replacements {:keys [ignore?] :or {ignore? true}}]
   (let [n (str n)]
        (when (vector? replacements)
                      ; ...
              (letfn [(f? [] (= 1 (count replacements)))
                      ; ...
                      (f1 [n marker replacement]
                          ; Replacement could be any type, it will be converted to string!
                          (if (or (-> replacement str empty? not)
                                  (not ignore?))
                              (clojure.string/replace n marker (str replacement))))
                      ; ...
                      (f2 [n dex replacement]
                          (let [marker (str "%" (inc dex))]
                               (f1 n marker replacement)))]
                     ; ...
                     (if (f?) (f1 n "%" (first replacements))
                              (reduce-kv f2 n replacements)))))))

(defn use-replacement
  ; @description
  ; Replaces the marker in the given 'n' string with given 'replacement' string.
  ;
  ; @param (string) n
  ; @param (string) replacement
  ; @param (map)(opt) options
  ; {:ignore? (boolean)(opt)
  ;   The function returns nil if the replacement is nil or empty.
  ;   Default: true}
  ;
  ; @usage
  ; (use-replacement "Hi, my name is %"
  ;                  "John")
  ; =>
  ; "Hi, my name is John"
  ;
  ; @return (string)
  ([n replacement]
   (use-replacement n replacement {}))

  ([n replacement {:keys [ignore?] :or {ignore? true}}]
   ; The replacement's value could be any type!
   (if (or (-> replacement str empty? not)
           (not ignore?))
       (clojure.string/replace (str n) "%"
                               (str replacement)))))

(defn use-placeholder
  ; @description
  ; Returns the given 'n' string in case it is not blank, otherwise returns the given 'placeholder' string.
  ;
  ; @param (string) n
  ; @param (string) placeholder
  ;
  ; @usage
  ; (use-placeholder "My content"
  ;                  "My placeholder")
  ; =>
  ; "My content"
  ;
  ; @usage
  ; (use-placeholder ""
  ;                  "My placeholder")
  ; =>
  ; "My placeholder"
  ;
  ; @return (string)
  [n placeholder]
  (if (-> n check/not-blank?)
      (-> n)
      (-> placeholder)))
