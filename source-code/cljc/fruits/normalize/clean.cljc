
(ns fruits.normalize.clean
    #?(:clj  (:require [clojure.string]
                       [fruits.string.api :as string])
       :cljs (:require [clojure.string]
                       [fruits.string.api      :as string]
                       ["normalize-diacritics" :refer [normalizeSync]])))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn deaccent
  ; @description
  ; Removes the diacritical marks (accents) from the given 'n' string.
  ;
  ; @param (string) n
  ;
  ; @usage
  ; (deaccent "aáAÁ")
  ; =>
  ; "aaAA"
  ;
  ; @return (string)
  [n]
  #?(:clj  (-> n (java.text.Normalizer/normalize java.text.Normalizer$Form/NFD)
                 (clojure.string/replace #"\p{InCombiningDiacriticalMarks}+" ""))
     :cljs (-> n (normalizeSync))))

(defn remove-special-characters
  ; @description
  ; Removes the special characters from the given 'n' string including accented characters.
  ;
  ; @param (string) n
  ; @param (string)(opt) exceptions
  ; Special characters that are allowed in the output string.
  ; Default: "-"
  ; @param (string)(opt) replacement
  ; Replacement string for special characters.
  ; Default: ""
  ;
  ; @usage
  ; (remove-special-characters "abc+-.")
  ; =>
  ; "abc-"
  ;
  ; @usage
  ; (remove-special-characters "abc+-.?" "-+")
  ; =>
  ; "abc+-"
  ;
  ; @return (string)
  ([n]
   (remove-special-characters n "-" ""))

  ([n exceptions]
   (remove-special-characters n exceptions ""))

  ([n exceptions replacement]
   (let [exceptions  (string/escape-characters exceptions)
         pattern     (re-pattern (str "[^\\w\\s"exceptions"]"))
         replacement (str replacement)]
        (clojure.string/replace n pattern replacement))))

(defn replace-blank-characters
  ; @description
  ; Replaces the blank characters / blank character groups in the given 'n' string with single hyphens.
  ;
  ; @param (string) n
  ;
  ; @usage
  ; (replace-blank-chars "a b  \n  c")
  ; =>
  ; "a-b-c"
  ;
  ; @return (string)
  [n]
  (clojure.string/replace n #"[ |-]+" "-"))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn clean-text
  ; @description
  ; Normalizes the given 'n' string by removing the diacritical marks, special characters,
  ; and blank characters, and converting the result into lowercase string.
  ;
  ; @param (*) n
  ; @param (string)(opt) exceptions
  ; Special characters that are allowed in the output string.
  ; Default: "-"
  ; @param (string)(opt) replacement
  ; Replacement string for special characters.
  ; Default: ""
  ;
  ; @usage
  ; (clean-text "a b  c")
  ; =>
  ; "a-b-c"
  ;
  ; @usage
  ; (clean-text "aá AÁ")
  ; =>
  ; "aa-AA"
  ;
  ; @usage
  ; (clean-text "1+2")
  ; =>
  ; "12"
  ;
  ; @usage
  ; (clean-text "1+2-4" "+-")
  ; =>
  ; "1+2-4"
  ;
  ; @usage
  ; (clean-text "abc?!123+-" "-" "*")
  ; =>
  ; "abc**123*-"
  ;
  ; @return (string)
  ([n]
   (clean-text n "-" ""))

  ([n exceptions]
   (clean-text n exceptions ""))

  ([n exceptions replacement]
   (-> n (str)
         (deaccent)
         (remove-special-characters exceptions replacement)
         (replace-blank-characters)
         (clojure.string/lower-case))))
