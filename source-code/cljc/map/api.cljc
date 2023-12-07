
(ns map.api
    (:refer-clojure :exclude [empty? keys namespace])
    (:require [map.check     :as check]
              [map.collapse  :as collapse]
              [map.compare   :as compare]
              [map.convert   :as convert]
              [map.core      :as core]
              [map.filter    :as filter]
              [map.key       :as key]
              [map.match     :as match]
              [map.merge     :as merge]
              [map.namespace :as namespace]
              [map.remove    :as remove]
              [map.update    :as update]
              [map.value     :as value]
              [map.walk      :as walk]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @redirect (map.check)
(def empty?    check/empty?)
(def nonempty? check/nonempty?)

; @redirect (map.collapse)
(def collapse collapse/collapse)

; @redirect (map.compare)
(def difference compare/difference)

; @redirect (map.convert)
(def to-vector convert/to-vector)

; @redirect (map.core)
(def swap      core/swap)
(def dissoc-in core/dissoc-in)
(def toggle    core/toggle)
(def toggle-in core/toggle-in)
(def move      core/move)
(def move-in   core/move-in)
(def copy      core/copy)
(def copy-in   core/copy-in)
(def get-by    core/get-by)
(def assoc-by  core/assoc-by)
(def dissoc-by core/dissoc-by)
(def update-by core/update-by)

; @redirect (map.filter)
(def filter-values filter/filter-values)

; @redirect (map.key)
(def keys              key/keys)
(def keys-by           key/keys-by)
(def first-key         key/first-key)
(def contains-key?     key/contains-key?)
(def contains-any-key? key/contains-any-key?)
(def contains-all-key? key/contains-all-key?)

; @redirect (map.match)
(def any-key-matches?     match/any-key-matches?)
(def any-value-matches?   match/any-value-matches?)
(def all-keys-match?      match/all-keys-match?)
(def all-values-match?    match/all-values-match?)
(def first-match-key      match/first-match-key)
(def first-matching-key   match/first-matching-key)
(def first-matching-value match/first-matching-value)
(def matches-pattern?     match/matches-pattern?)

; @redirect (map.merge)
(def deep-merge     merge/deep-merge)
(def reversed-merge merge/reversed-merge)
(def merge-some     merge/merge-some)

; @redirect (map.namespace)
(def namespace        namespace/namespace)
(def namespaced?      namespace/namespaced?)
(def add-namespace    namespace/add-namespace)
(def remove-namespace namespace/remove-namespace)
(def assoc-ns         namespace/assoc-ns)
(def get-ns           namespace/get-ns)

; @redirect (map.remove)
(def remove-key       remove/remove-key)
(def remove-keys      remove/remove-keys)
(def remove-keys-by   remove/remove-keys-by)
(def remove-value     remove/remove-value)
(def remove-values    remove/remove-values)
(def remove-values-by remove/remove-values-by)
(def keep-key         remove/keep-key)
(def keep-keys        remove/keep-keys)
(def keep-keys-by     remove/keep-keys-by)
(def keep-value       remove/keep-value)
(def keep-values      remove/keep-values)
(def keep-values-by   remove/keep-values-by)

; @redirect (map.update)
(def update-all-key   update/update-all-key)
(def update-all-value update/update-all-value)
(def update-keys-by   update/update-keys-by)
(def update-values-by update/update-values-by)

; @redirect (map.value)
(def values          value/values)
(def first-value     value/first-value)
(def contains-value? value/contains-value?)
(def values-equal?   value/values-equal?)

; @redirect (map.walk)
(def ->keys              walk/->keys)
(def ->>keys             walk/->>keys)
(def ->values            walk/->values)
(def ->>values           walk/->>values)
(def ->kv                walk/->kv)
(def ->>kv               walk/->>kv)
