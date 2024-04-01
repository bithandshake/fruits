
(ns fruits.gestures.convert
    (:require [fruits.gestures.type :as type]
              [fruits.loop.api      :refer [do-while]]
              [fruits.mixed.api     :as mixed]
              [fruits.string.api    :as string]
              [fruits.vector.api    :as vector]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn item-label->copy-label
  ; @description
  ; - Appends a copy marker suffix to the given 'item-label' string.
  ; - Optionally avoids name conflicts by checking other labels in the given 'concurent-labels' vector.
  ;
  ; @param (string) item-label
  ; @param (strings in vector)(opt) concurent-labels
  ;
  ; @usage
  ; (item-label->copy-label "My item")
  ; =>
  ; "My item #2"
  ;
  ; @usage
  ; (item-label->copy-label "My item" ["Another item"])
  ; =>
  ; "My item #2"
  ;
  ; @usage
  ; (item-label->copy-label "My item" ["My item" "My item #2"])
  ; =>
  ; "My item #3"
  ;
  ; @usage
  ; (item-label->copy-label "My item #2" ["Another item"])
  ; =>
  ; "My item #3"
  ;
  ; @usage
  ; (item-label->copy-label "" [])
  ; =>
  ; "#2"
  ;
  ; @return (string)
  ([item-label]
   (item-label->copy-label item-label []))

  ([item-label concurent-labels]
   (letfn [(f0 [n] (-> (vector/contains-item? concurent-labels n) not))
           (f1 [n] (if (type/ordered-label? n)
                       (let [copy-dex      (-> n (string/after-last-occurence  "#" {:return? false}))
                             label-base    (-> n (string/before-last-occurence "#" {:return? true}))
                             next-copy-dex (-> copy-dex mixed/to-integer inc)]
                            (str label-base "#" next-copy-dex))
                       (if (-> n   (string/not-empty?))
                           (-> n   (str " #2"))
                           (-> nil (str  "#2")))))]
          (do-while f1 item-label f0))))
