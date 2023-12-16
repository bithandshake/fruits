
(ns fruits.vector.move
    (:require [fruits.math.api      :as math]
              [fruits.seqable.api   :as seqable]
              [fruits.vector.dex    :as dex]
              [fruits.vector.remove :as remove]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn move-nth-item
  ; @param (vector) n
  ; @param (integer) from
  ; @param (integer) to
  ;
  ; @usage
  ; (move-nth-item [:a :b :c] 0 2)
  ;
  ; @example
  ; (move-nth-item [:a :b :c :d :e :f :g :h] 2 2)
  ; =>
  ; [:a :b :c :d :e :f :g :h]
  ;
  ; @example
  ; (move-nth-item [:a :b :c :d :e :f :g :h] 2 5)
  ; =>
  ; [:a :b :d :e :f :c :g :h]
  ;
  ; @example
  ; (move-nth-item [:a :b :c :d :e :f :g :h] 5 2)
  ; =>
  ; [:a :b :f :c :d :e :g :h]
  ;
  ; @return (vector)
  [n from to]
  (let [from (seqable/normalize-dex n from)
        to   (seqable/normalize-dex n to)]
       (cond ; Keeps the item in place ...
             (= from to) (-> n)
             ; Moves the item fwd ...
             (< from to) (vec (concat (subvec n 0 from)
                                      (subvec n (inc from) (inc to))
                                      (subvec n from (inc from))
                                      (subvec n (inc to))))
             ; Moves the item bwd ...
             (> from to) (vec (concat (subvec n 0 to)
                                      (subvec n from (inc from))
                                      (subvec n to from)
                                      (subvec n (inc from)))))))

(defn move-nth-item-bwd
  ; @param (vector) n
  ; @param (integer) th
  ;
  ; @usage
  ; (move-nth-item-bwd [:a :b :c] 1)
  ;
  ; @example
  ; (move-nth-item-bwd [:a :b :c :d] 2)
  ; =>
  ; [:a :c :b :d]
  ;
  ; @example
  ; (move-nth-item-bwd [:a :b :c :d] 0)
  ; =>
  ; [:b :c :d :a]
  ;
  ; @return (vector)
  [n th]
  (move-nth-item n th (seqable/prev-dex n th)))

(defn move-nth-item-fwd
  ; @param (vector) n
  ; @param (integer) th
  ;
  ; @usage
  ; (move-nth-item-fwd [:a :b :c] 1)
  ;
  ; @example
  ; (move-nth-item-fwd [:a :b :c :d] 2)
  ; =>
  ; [:a :b :d :c]
  ;
  ; @example
  ; (move-nth-item-fwd [:a :b :c :d] 3)
  ; =>
  ; [:d :a :b :c]
  ;
  ; @return (vector)
  [n th]
  (move-nth-item n th (seqable/next-dex n th)))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn move-item-to-last
  ; @param (vector) n
  ; @param (*) x
  ;
  ; @usage
  ; (move-item-to-last [:a :b :c] :a)
  ;
  ; @example
  ; (move-item-to-last [:a :b] :a)
  ; =>
  ; [:b :a]
  ;
  ; @example
  ; (move-item-to-last [:b] :a)
  ; =>
  ; [:b :a]
  ;
  ; @return (vector)
  [n x]
  (conj (remove/remove-item n x) x))

(defn move-item-to-first
  ; @param (vector) n
  ; @param (*) x
  ;
  ; @example
  ; (move-item-to-first [:a :b :c] :c)
  ;
  ; @example
  ; (move-item-to-first [:a :b] :b)
  ; =>
  ; [:b :a]
  ;
  ; @example
  ; (move-item-to-first [:a] :b)
  ; =>
  ; [:b :a]
  ;
  ; @return (vector)
  [n x]
  (vec (cons x (remove/remove-item n x))))

(defn move-first-occurence
  ; @param (vector) n
  ; @param (*) x
  ; @param (integer) to
  ;
  ; @usage
  ; (move-first-occurence [:a :b :c] :c 0)
  ;
  ; @example
  ; (move-first-occurence [:a :b :c :a :b :c] :b 3)
  ; =>
  ; [:a :c :b :a :b :c]
  ;
  ; @example
  ; (move-first-occurence [:a :b :c :a :b :c] :b 1)
  ; =>
  ; [:a :b :c :a :b :c]
  ;
  ; @example
  ; (move-first-occurence [:a :b :c :a :b :c] :b 20)
  ; =>
  ; [:a :c :a :b :c :b]
  ;
  ; @return (vector)
  [n x to]
  (if-let [item-first-dex (dex/first-dex-of n x)]
          (move-nth-item n item-first-dex to)
          (-> n)))