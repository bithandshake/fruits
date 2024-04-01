
(ns fruits.normalize.api
    (:require [fruits.normalize.clean :as clean]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @redirect (fruits.normalize.clean/*)
(def deaccent                  clean/deaccent)
(def remove-special-characters clean/remove-special-characters)
(def replace-blank-characters  clean/replace-blank-characters)
(def clean-text                clean/clean-text)
