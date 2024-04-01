
(ns fruits.format.api
    (:require [fruits.format.cover   :as cover]
              [fruits.format.number  :as number]
              [fruits.format.version :as version]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @redirect (fruits.format.cover/*)
(def cover-email-address cover/cover-email-address)

; @redirect (fruits.format.number/*)
(def sign-number          number/sign-number)
(def format-number        number/format-number)
(def group-number         number/group-number)
(def fill-leading-zeros   number/fill-leading-zeros)
(def remove-leading-zeros number/remove-leading-zeros)
(def fill-trailing-zeros  number/fill-trailing-zeros)
(def decimal-scale        number/decimal-scale)
(def abbreviate-number    number/abbreviate-number)

; @redirect (fruits.format.version/*)
(def inc-version version/inc-version)
