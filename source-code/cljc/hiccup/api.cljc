
(ns hiccup.api
    (:require [hiccup.attributes :as attributes]
              [hiccup.content    :as content]
              [hiccup.convert    :as convert]
              [hiccup.parse      :as parse]
              [hiccup.type       :as type]
              [hiccup.walk       :as walk]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @redirect (hiccup.attributes)
(def get-attributes attributes/get-attributes)
(def get-style      attributes/get-style)
(def set-style      attributes/set-style)
(def join-class     attributes/join-class)
(def value          attributes/value)

; @redirect (hiccup.content)
(def content-length content/content-length)

; @redirect (hiccup.convert)
(def to-string convert/to-string)

; @redirect (hiccup.parse)
(def parse-css        parse/parse-css)
(def unparse-css      parse/unparse-css)
(def parse-newlines   parse/parse-newlines)
(def unparse-newlines parse/unparse-newlines)

; @redirect (hiccup.type)
(def hiccup?   type/hiccup?)
(def tag-name? type/tag-name?)

; @redirect (hiccup.walk)
(def walk             walk/walk)
(def explode          walk/explode)
(def put              walk/put)
(def put-with         walk/put-with)
(def put-with-indexed walk/put-with-indexed)
