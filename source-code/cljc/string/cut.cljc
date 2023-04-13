
(ns string.cut
    (:require [clojure.string]
              [math.api :as math]
              [noop.api :refer [return]]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn part
  ; @param (*) n
  ; @param (integer) start
  ; @param (integer)(opt) end
  ;
  ; @usage
  ; (part "abc" 0 2)
  ;
  ; @example
  ; (part "abcdef" 2 4)
  ; =>
  ; "cd"
  ;
  ; @example
  ; (part "abcdef" 4 2)
  ; =>
  ; "cd"
  ;
  ; @example
  ; (part 12345 2 4)
  ; =>
  ; "34"
  ;
  ; @example
  ; (part [:a :b] 0 6)
  ; =>
  ; "[:a, :"
  ;
  ; @return (string)
  ([n start]
   (let [n (str n)]
        (part n start (count n))))

  ([n start end]
   (let [n (str n)]
        (if (and (-> n empty? not)
                 (math/between? end   0 (count n))
                 (math/between? start 0 (count n)))
            (subs   n start end)
            (return n)))))

(defn cut
  ; @param (*) n
  ; @param (integer) start
  ; @param (integer)(opt) end
  ;
  ; @usage
  ; (cut "abc" 0 2)
  ;
  ; @example
  ; (cut "abcdef" 2 4)
  ; =>
  ; "abef"
  ;
  ; @example
  ; (cut "abcdef" 4 2)
  ; =>
  ; "abef"
  ;
  ; @example
  ; (cut 12345 2 4)
  ; =>
  ; "125"
  ;
  ; @example
  ; (cut [:a :b] 0 3)
  ; =>
  ; " :b]"
  ;
  ; @return (string)
  ([n start]
   (let [n (str n)]
        (cut n 0 start)))

  ([n start end]
   (let [n (str n)]
        (if (and (-> n empty? not)
                 (math/between? end   0 (count n))
                 (math/between? start 0 (count n)))
            (str (subs n 0 (min start end))
                 (subs n   (max start end)))
            (return n)))))

(defn trim
  ; @param (*) n
  ;
  ; @usage
  ; (trim " abc")
  ;
  ; @example
  ; (trim " abc  ")
  ; =>
  ; "abc"
  ;
  ; @return (string)
  [n]
  (-> n str clojure.string/trim))

(defn remove-part
  ; @param (*) n
  ; @param (regex pattern or string) x
  ;
  ; @usage
  ; (remove-part "abc" "b")
  ;
  ; @example
  ; (remove-part "abc" "b")
  ; =>
  ; "ac"
  ;
  ; @example
  ; (remove-part "abc abc" "b")
  ; =>
  ; "ac ac"
  ;
  ; @example
  ; (remove-part "abc abc 123" #"\d")
  ; =>
  ; "abc abc"
  ;
  ; @example
  ; (remove-part "///" "//")
  ; =>
  ; ""
  ;
  ; @return (string)
  [n x]
  (clojure.string/replace (str n) x ""))

(defn filter-characters
  ; @param (*) n
  ; @param (vector) allowed-characters
  ;
  ; @example
  ; (filter-characters "+3630 / 123 - 4567" ["+" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"])
  ; =>
  ; "+36301234567"
  ;
  ; @example
  ; (filter-characters [:a :b] [":" "a" "b"])
  ; =>
  ; ":a:b"
  ;
  ; @return (string)
  [n allowed-characters]
  (letfn [(f [result x] (if (some #(= x %) allowed-characters)
                            (str result x) result))]
         (reduce f "" (str n))))

(defn max-length
  ; @param (*) n
  ; @param (integer) limit
  ; @param (*)(opt) suffix
  ;
  ; @usage
  ; (max-length "One Flew Over the Cuckoo's Nest" 5)
  ;
  ; @example
  ; (max-length "One Flew Over the Cuckoo's Nest" 10)
  ; =>
  ; "One Flew O"
  ;
  ; @example
  ; (max-length "One Flew Over the Cuckoo's Nest" 10 " ...")
  ; =>
  ; "One Flew O ..."
  ;
  ; @example
  ; (max-length nil 10)
  ; =>
  ; ""
  ;
  ; @return (string)
  [n limit & [suffix]]
  (let [n (str n)]
       (if (and (-> n empty? not)
                (-> limit integer?)
                (<  limit (count n)))
           (str (subs n 0 limit) suffix)
           (return n))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn before-first-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true
  ;  :return? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (before-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                         "never")
  ;
  ; @example
  ; (before-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                         "never")
  ; =>
  ; "With insomnia, you're "
  ;
  ; @example
  ; (before-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                         "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (before-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                         "abc"
  ;                         {:return? true})
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @example
  ; (before-first-occurence nil "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (before-first-occurence nil "abc" {:return? true})
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (before-first-occurence n x {}))

  ([n x {:keys [case-sensitive? return?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/index-of o x)]
                              (subs n 0 dex)
                              (if return? n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn before-last-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true
  ;  :return? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (before-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "never")
  ;
  ; @example
  ; (before-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "never")
  ; =>
  ; "With insomnia, you're never really awake; but you're "
  ;
  ; @example
  ; (before-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (before-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "abc"
  ;                        {:return? true})
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @example
  ; (before-last-occurence nil "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (before-last-occurence nil "abc" {:return? true})
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (before-last-occurence n x {}))

  ([n x {:keys [case-sensitive? return?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/last-index-of o x)]
                              (subs n 0 dex)
                              (if return? n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn after-first-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true
  ;  :return? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (after-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "never")
  ;
  ; @example
  ; (after-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "never")
  ; =>
  ; " really awake; but you're never really asleep."
  ;
  ; @example
  ; (after-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (after-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "abc"
  ;                        {:return? true})
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @example
  ; (after-first-occurence nil "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (after-first-occurence nil "abc" {:return? true})
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (after-first-occurence n x {}))

  ([n x {:keys [case-sensitive? return?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/index-of o x)]
                              (subs n (+ dex (count x)))
                              (if return? n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn after-last-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true
  ;  :return? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (after-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                       "never")
  ;
  ; @example
  ; (after-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                       "never")
  ; =>
  ; "really asleep."
  ;
  ; @example
  ; (after-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                       "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (after-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                       "abc"
  ;                       {:return? true})
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @example
  ; (after-last-occurence nil "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (after-last-occurence nil "abc" {:return? true})
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (after-last-occurence n x {}))

  ([n x {:keys [case-sensitive? return?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/last-index-of o x)]
                              (subs n (+ dex (count x)))
                              (if return? n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn from-first-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true
  ;  :return? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (from-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                       "never")
  ;
  ; @example
  ; (from-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                       "never")
  ; =>
  ; "never really awake; but you're never really asleep."
  ;
  ; @example
  ; (from-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                       "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (from-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                       "abc"
  ;                       {:return? true})
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @example
  ; (from-first-occurence nil "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (from-first-occurence nil "abc" {:return? true})
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (from-first-occurence n x {}))

  ([n x {:keys [case-sensitive? return?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/index-of o x)]
                              (subs n dex)
                              (if return? n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn from-last-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true
  ;  :return? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (from-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                      "never")
  ;
  ; @example
  ; (from-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                      "never")
  ; =>
  ; "never really asleep."
  ;
  ; @example
  ; (from-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                      "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (from-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                      "abc"
  ;                      {:return? true})
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @example
  ; (from-last-occurence nil "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (from-last-occurence nil "abc" {:return? true})
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (from-last-occurence n x {}))

  ([n x {:keys [case-sensitive? return?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/last-index-of o x)]
                              (subs n dex)
                              (if return? n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn to-first-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true
  ;  :return? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (to-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "never")
  ;
  ; @example
  ; (to-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "never")
  ; =>
  ; "With insomnia, you're never"
  ;
  ; @example
  ; (to-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (to-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "abc"
  ;                     {:return? true})
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @example
  ; (to-first-occurence nil "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (to-first-occurence nil "abc" {:return? true})
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (to-first-occurence n x {}))

  ([n x {:keys [case-sensitive? return?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/index-of o x)]
                              (subs n 0 (+ dex (count x)))
                              (if return? n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn to-last-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true
  ;  :return? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (to-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                    "never")
  ;
  ; @example
  ; (to-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                    "never")
  ; =>
  ; "With insomnia, you're never really awake; but you're never"
  ;
  ; @example
  ; (to-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                    "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (to-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                    "abc"
  ;                    {:return? true})
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @example
  ; (to-last-occurence nil "abc")
  ; =>
  ; nil
  ;
  ; @example
  ; (to-last-occurence nil "abc" {:return? true})
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (to-last-occurence n x {}))

  ([n x {:keys [case-sensitive? return?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/last-index-of o x)]
                              (subs n 0 (+ dex (count x)))
                              (if return? n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn remove-first-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (remove-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                         "never")
  ;
  ; @example
  ; (remove-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                         "never")
  ; =>
  ; "With insomnia, you're really awake; but you're never really asleep."
  ;
  ; @example
  ; (remove-first-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                         "abc")
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @return (string)
  ([n x]
   (remove-first-occurence n x {}))

  ([n x {:keys [case-sensitive?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/index-of o x)]
                              (str (subs n 0 dex)
                                   (subs n (+ dex (count x))))
                              (return n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn remove-last-occurence
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (remove-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "never")
  ;
  ; @example
  ; (remove-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "never")
  ; =>
  ; "With insomnia, you're never really awake; but you're really asleep."
  ;
  ; @example
  ; (remove-last-occurence "With insomnia, you're never really awake; but you're never really asleep."
  ;                        "abc")
  ; =>
  ; "With insomnia, you're never really awake; but you're never really asleep."
  ;
  ; @return (string)
  ([n x]
   (remove-last-occurence n x {}))

  ([n x {:keys [case-sensitive?] :or {case-sensitive? true}}]
   (letfn [(f [n o x] (if-let [dex (clojure.string/last-index-of o x)]
                              (str (subs n 0 dex)
                                   (subs n (+ dex (count x))))
                              (return n)))]
          (if case-sensitive? (f (-> n str)
                                 (-> n str)
                                 (-> x str))
                              (f (-> n str)
                                 (-> n str clojure.string/lower-case)
                                 (-> x str clojure.string/lower-case))))))

(defn between-occurences
  ; @param (*) n
  ; @param (*) x
  ; @param (*) y
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (between-occurences "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "never" "never")
  ;
  ; @example
  ; (between-occurences "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "never" "asleep.")
  ; =>
  ; " really awake; but you're never really "
  ;
  ; @example
  ; (between-occurences "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "never" "never")
  ; =>
  ; " really awake; but you're "
  ;
  ; @example
  ; (between-occurences "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "abc" "never")
  ; =>
  ; nil
  ;
  ; @example
  ; (between-occurences "With insomnia, you're never really awake; but you're never really asleep."
  ;                     "abc" "def")
  ; =>
  ; nil
  ;
  ; @return (string)
  ([n x y]
   (between-occurences n x y {}))

  ([n x y {:keys [case-sensitive?] :or {case-sensitive? true}}]
   (-> n (after-first-occurence x {:return? false :case-sensitive? case-sensitive?})
         (before-last-occurence y {:return? false :case-sensitive? case-sensitive?}))))
