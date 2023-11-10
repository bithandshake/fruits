
(ns string.core
    (:require [clojure.string]
              [math.api      :as math]
              [string.check  :as check]
              [string.cursor :as cursor]
              [string.cut    :as cut]
              [string.dex    :as dex]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn abc
  ; @description
  ; Takes the value 'a' (converted to string) and value 'b' (converted to string)
  ; and returns the one that is less than in alphabetical order.
  ;
  ; @param (*) a
  ; @param (*) b
  ;
  ; @usage
  ; (abc "abc" "def")
  ;
  ; @example
  ; (abc "abc" "def")
  ; =>
  ; "abc"
  ;
  ; @example
  ; (abc "def" "abc")
  ; =>
  ; "abc"
  ;
  ; @example
  ; (abc "abc" "abc")
  ; =>
  ; "abc"
  ;
  ; @example
  ; (abc 10 12)
  ; =>
  ; "10"
  ;
  ; @example
  ; (abc "" "abc")
  ; =>
  ; ""
  ;
  ; @return (string)
  [a b]
  (let [a (str a)
        b (str b)]
       (if (check/abc? a b)
           (-> a)
           (-> b))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn first-character
  ; @description
  ; - Returns the first character of the given 'n' value (converted to string.)
  ; - Converts the output to string because one character long strings (in Java language) could be character types!
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (first-character "abc")
  ;
  ; @example
  ; (first-character "abc")
  ; =>
  ; "a"
  ;
  ; @example
  ; (first-character {:a "A"})
  ; =>
  ; "{"
  ;
  ; @param (string)
  [n]
  (let [n (str n)]
       (-> n first str)))

(defn second-character
  ; @description
  ; - Returns the second character of the given 'n' value (converted to string.)
  ; - Converts the output to string because one character long strings (in Java language) could be character types!
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (second-character "abc")
  ;
  ; @example
  ; (second-character "abc")
  ; =>
  ; "b"
  ;
  ; @example
  ; (second-character {:a "A"})
  ; =>
  ; ":"
  ;
  ; @param (string)
  [n]
  (let [n (str n)]
       (-> n second str)))

(defn last-character
  ; @description
  ; - Returns the first character of the given 'n' value (converted to string.)
  ; - Converts the output to string because one character long strings (in Java language) could be character types!
  ;
  ; @param (*) n
  ;
  ; @usage
  ; (last-character "abc")
  ;
  ; @example
  ; (last-character "abc")
  ; =>
  ; "c"
  ;
  ; @example
  ; (last-character {:a "A"})
  ; =>
  ; "}"
  ;
  ; @param (string)
  [n]
  (let [n (str n)]
       (-> n last str)))

(defn nth-character
  ; @description
  ; - Returns the nth character of the given 'n' value (converted to string.)
  ; - Converts the output to string because in Java language one character long strings could be character types!
  ;
  ; @param (*) n
  ; @param (integer) dex
  ;
  ; @usage
  ; (nth-character "abc" 2)
  ;
  ; @example
  ; (nth-character "abc" 2)
  ; =>
  ; "c"
  ;
  ; @example
  ; (nth-character {:a "A"} 1)
  ; =>
  ; ":"
  ;
  ; @param (string)
  [n dex]
  (let [n (str n)]
       (if (-> n (dex/dex-in-bounds? dex))
           (-> n (nth                dex) str))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn multiply
  ; @param (*) n
  ; @param (integer) x
  ;
  ; @usage
  ; (multiply "a" 3)
  ;
  ; @example
  ; (multiply "a" 3)
  ; =>
  ; "aaa"
  ;
  ; @return (string)
  [n x]
  (when (integer? x)
        (letfn [(f [result _]
                   (str result n))]
               (reduce f "" (range 0 x)))))

(defn join
  ; @param (collection) coll
  ; @param (*) separator
  ; @param (map)(opt) options
  ; {:join-empty? (boolean)(opt)
  ;   Default: true}
  ;
  ; @usage
  ; (join ["filename" "extension"] ".")
  ;
  ; @example
  ; (join ["filename" "extension"] ".")
  ; =>
  ; "filename.extension"
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
  ([coll separator]
   (join coll separator {}))

  ([coll separator {:keys [join-empty?] :or {join-empty? true}}]
   (let [last-dex (-> coll count dec)]
        (letfn [(separate? [dex] (and (not= dex last-dex)
                                      (-> (nth coll (inc dex)) str empty? not)))
                (join? [part] (or join-empty? (-> part str empty? not)))
                (f [result dex part] (if (join? part)
                                         (if (separate? dex)
                                             (str result part separator)
                                             (str result part))
                                         (-> result)))]
               ; The reduce-kv takes vectors and maps but doesn't take lists!
               (reduce-kv f "" (vec coll))))))

(defn cover
  ; @param (*) n
  ; @param (*) x
  ; @param (integer)(opt) offset
  ;
  ; @usage
  ; (cover "user@email.com" "**")
  ;
  ; @example
  ; (cover "user@email.com" "**")
  ; =>
  ; "**er@email.com"
  ;
  ; @example
  ; (cover "user@email.com" "**" 2)
  ; =>
  ; "us**@email.com"
  ;
  ; @return (string)
  ([n x]
   (cover n x 0))

  ([n x offset]
   (let [n (str n)
         x (str x)]
        (str (subs n 0 offset)
             (subs x 0 (- (count n) offset))
             (subs n   (+ (count x) offset))))))

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

(defn prefix
  ; @param (*) n
  ; @param (*) x
  ; @param (*)(opt) separator
  ;
  ; @usage
  ; (prefix "420" "$")
  ;
  ; @example
  ; (prefix "420" "$")
  ; =>
  ; "$420"
  ;
  ; @example
  ; (prefix 420 "$")
  ; =>
  ; "$420"
  ;
  ; @example
  ; (prefix "" "$")
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (prefix n x nil))

  ([n x separator]
   (let [n (str n)]
        (if (->  n empty?)
            (->  n)
            (str x separator n)))))

(defn suffix
  ; @param (*) n
  ; @param (*) x
  ; @param (*)(opt) separator
  ;
  ; @usage
  ; (suffix "420" "px")
  ;
  ; @example
  ; (suffix "420" "px")
  ; =>
  ; "420px"
  ;
  ; @example
  ; (suffix 420 "px")
  ; =>
  ; "420px"
  ;
  ; @example
  ; (suffix "" "px")
  ; =>
  ; ""
  ;
  ; @return (string)
  ([n x]
   (suffix n x nil))

  ([n x separator]
   (let [n (str n)]
        (if (->  n empty?)
            (->  n)
            (str n separator x)))))

(defn prepend
  ; @param (*) n
  ; @param (*) x
  ; @param (*)(opt) separator
  ;
  ; @usage
  ; (prepend "my-domain.com" "https://")
  ;
  ; @example
  ; (prepend "my-domain.com" "https://")
  ; =>
  ; "https://my-domain.com"
  ;
  ; @return (string)
  ([n x]
   (prepend n x nil))

  ([n x separator]
   (prefix n x)))

(defn append
  ; @param (*) n
  ; @param (*) x
  ; @param (*)(opt) separator
  ;
  ; @usage
  ; (append "https://" "my-domain.com")
  ;
  ; @example
  ; (append "https://" "my-domain.com")
  ; =>
  ; "https://my-domain.com"
  ;
  ; @return (string)
  ([n x]
   (append n x nil))

  ([n x separator]
   (suffix n x)))

(defn insert-part
  ; @param (*) n
  ; @param (*) x
  ; @param (integer) cursor
  ;
  ; @usage
  ; (insert-part "abcd" "xx" 2)
  ;
  ; @example
  ; (insert-part "abcd" "xx" 2)
  ; =>
  ; "abxxcd"
  ;
  ; @return (string)
  [n x cursor]
  (let [n (str n)]
       (if (-> n (cursor/cursor-in-bounds? cursor))
           (str (subs n 0 cursor) x
                (subs n   cursor)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn count-occurences
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:separate-matches? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (count-occurences "abc" "a")
  ;
  ; @example
  ; (count-occurences "abca" "a")
  ; =>
  ; 2
  ;
  ; @example
  ; (count-occurences "abca" "ab")
  ; =>
  ; 1
  ;
  ; @example
  ; (count-occurences "aaaa" "aa")
  ; =>
  ; 3
  ;
  ; @example
  ; (count-occurences "aaaa" "aa" {:separate-matches? true})
  ; =>
  ; 2
  ;
  ; @return (integer)
  ([n x]
   (count-occurences n x {}))

  ([n x {:keys [separate-matches?]}]
   (let [n (str n)
         x (str x)]
        (if (check/contains-part? n x)
            (let [step (if separate-matches? (count x) 1)]
                 (letfn [(f [cursor match-count]
                            (if-let [first-dex (dex/first-dex-of (cut/part n cursor) x)]
                                    (let [step (if separate-matches? (count x) 1)]
                                         (f (+   cursor first-dex step)
                                            (inc match-count)))
                                    (-> match-count)))]
                        (f 0 0)))
            (-> 0)))))

(defn min-occurence?
  ; @param (*) n
  ; @param (*) x
  ; @param (integer) min
  ; @param (map)(opt) options
  ; {:separate-matches? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (min-occurence? "abc" "a" 1)
  ;
  ; @example
  ; (min-occurence? "abc abc" "a" 2)
  ; =>
  ; true
  ;
  ; @example
  ; (min-occurence? "abc abc" "a" 3)
  ; =>
  ; false
  ;
  ; @example
  ; (min-occurence? "aaaa" "aa" 3)
  ; =>
  ; true
  ;
  ; @example
  ; (min-occurence? "aaaa" "aa" 3 {:separate-matches? true})
  ; =>
  ; false
  ;
  ; @return (boolean)
  ([n x min]
   (min-occurence? n x min {}))

  ([n x min options]
   (let [occurence-count (count-occurences n x options)]
        (<= min occurence-count))))

(defn max-occurence?
  ; @param (*) n
  ; @param (*) x
  ; @param (integer) max
  ; @param (map)(opt) options
  ; {:separate-matches? (boolean)(opt)
  ;   Default: false}
  ;
  ; @usage
  ; (max-occurence? "abc" "a" 1)
  ;
  ; @example
  ; (max-occurence? "abc abc" "a" 2)
  ; =>
  ; true
  ;
  ; @example
  ; (max-occurence? "abc abc abc" "a" 2)
  ; =>
  ; false
  ;
  ; @example
  ; (max-occurence? "aaaa" "aa" 2)
  ; =>
  ; false
  ;
  ; @example
  ; (max-occurence? "aaaa" "aa" 2 {:separate-matches? true})
  ; =>
  ; true
  ;
  ; @return (boolean)
  ([n x max]
   (max-occurence? n x max {}))

  ([n x max options]
   (let [occurence-count (count-occurences n x options)]
        (>= max occurence-count))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn ends-with?
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (ends-with? "The things you used to own, now they own you." ".")
  ;
  ; @example
  ; (ends-with? "The things you used to own, now they own you." ".")
  ; =>
  ; true
  ;
  ; @example
  ; (ends-with? "The things you used to own, now they own you." "")
  ; =>
  ; true
  ;
  ; @example
  ; (ends-with? "The things you used to own, now they own you." "!")
  ; =>
  ; false
  ;
  ; @return (boolean)
  ([n x]
   (ends-with? n x {}))

  ([n x {:keys [case-sensitive?] :or {case-sensitive? true}}]
   (let [n (str n)
         x (str x)]
        (letfn [(f [n x] (clojure.string/ends-with? n x))]
               (if case-sensitive? (f n x)
                                   (f (clojure.string/lower-case n)
                                      (clojure.string/lower-case x)))))))

(defn not-ends-with?
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (not-ends-with? "The things you used to own, now they own you." ".")
  ;
  ; @example
  ; (not-ends-with? "The things you used to own, now they own you." "!")
  ; =>
  ; true
  ;
  ; @example
  ; (not-ends-with? "The things you used to own, now they own you." ".")
  ; =>
  ; false
  ;
  ; @example
  ; (not-ends-with? "The things you used to own, now they own you." "")
  ; =>
  ; false
  ;
  ; @return (boolean)
  ([n x]
   (not-ends-with? n x {}))

  ([n x options]
   (let [ends-with? (ends-with? n x options)]
        (not ends-with?))))

(defn ends-with!
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (ends-with! "The things you used to own, now they own you." ".")
  ;
  ; @example
  ; (ends-with! "The things you used to own, now they own you." ".")
  ; =>
  ; "The things you used to own, now they own you."
  ;
  ; @example
  ; (ends-with! "The things you used to own, now they own you" ".")
  ; =>
  ; "The things you used to own, now they own you."
  ;
  ; @return (string)
  ([n x]
   (ends-with! n x {}))

  ([n x options]
   (if (ends-with? n x options)
       (->  n)
       (str n x))))

(defn not-ends-with!
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (not-ends-with! "The things you used to own, now they own you." ".")
  ;
  ; @example
  ; (not-ends-with! "The things you used to own, now they own you" ".")
  ; =>
  ; "The things you used to own, now they own you"
  ;
  ; @example
  ; (not-ends-with! "The things you used to own, now they own you." ".")
  ; =>
  ; "The things you used to own, now they own you"
  ;
  ; @example
  ; (not-ends-with! nil ".")
  ; =>
  ; nil
  ;
  ; @return (string)
  ([n x]
   (not-ends-with! n x {}))

  ([n x options]
   (if (ends-with?                n x options)
       (cut/before-last-occurence n x)
       (-> n))))

(defn starts-with?
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (starts-with? "On a long enough time line, the survival rate for everyone drops to zero."
  ;               "On a")
  ;
  ; @example
  ; (starts-with? "On a long enough time line, the survival rate for everyone drops to zero."
  ;               "On a")
  ; =>
  ; true
  ;
  ; @example
  ; (starts-with? "On a long enough time line, the survival rate for everyone drops to zero."
  ;               "")
  ; =>
  ; true
  ;
  ; @example
  ; (starts-with? "On a long enough time line, the survival rate for everyone drops to zero."
  ;               "The ")
  ; =>
  ; false
  ;
  ; @return (boolean)
  ([n x]
   (starts-with? n x {}))

  ([n x {:keys [case-sensitive?] :or {case-sensitive? true}}]
   (let [n (str n)
         x (str x)]
        (letfn [(f [n x] (clojure.string/starts-with? n x))]
               (if case-sensitive? (f n x)
                                   (f (clojure.string/lower-case n)
                                      (clojure.string/lower-case x)))))))

(defn not-starts-with?
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (not-starts-with? "On a long enough time line, the survival rate for everyone drops to zero."
  ;                   "On a")
  ;
  ; @example
  ; (not-starts-with? "On a long enough time line, the survival rate for everyone drops to zero."
  ;                   "The ")
  ; =>
  ; true
  ;
  ; @example
  ; (not-starts-with? "On a long enough time line, the survival rate for everyone drops to zero."
  ;                   "")
  ; =>
  ; false
  ;
  ; @example
  ; (not-starts-with? "On a long enough time line, the survival rate for everyone drops to zero."
  ;                   "On a")
  ; =>
  ; false
  ;
  ; @return (boolean)
  ([n x]
   (not-starts-with? n x {}))

  ([n x options]
   (let [starts-with? (starts-with? n x options)]
        (not starts-with?))))

(defn starts-with!
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (starts-with! "On a long enough time line, the survival rate for everyone drops to zero."
  ;               "On a")
  ;
  ; @example
  ; (starts-with! "On a long enough time line, the survival rate for everyone drops to zero."
  ;               "On a")
  ; =>
  ; "On a long enough time line, the survival rate for everyone drops to zero."
  ;
  ; @example
  ; (starts-with! " long enough time line, the survival rate for everyone drops to zero."
  ;               "On a")
  ; =>
  ; "On a long enough time line, the survival rate for everyone drops to zero."
  ;
  ; @return (string)
  ([n x]
   (starts-with! n x {}))

  ([n x options]
   (if (starts-with? n x options)
       (->  n)
       (str x n))))

(defn not-starts-with!
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (not-starts-with! "On a long enough time line, the survival rate for everyone drops to zero."
  ;                   "On a")
  ;
  ; @example
  ; (not-starts-with! " long enough time line, the survival rate for everyone drops to zero."
  ;                   "On a")
  ; =>
  ; " long enough time line, the survival rate for everyone drops to zero."
  ;
  ; @example
  ; (not-starts-with! " long enough time line, the survival rate for everyone drops to zero."
  ;                   "On a")
  ; =>
  ; " long enough time line, the survival rate for everyone drops to zero."
  ;
  ; @example
  ; (not-starts-with! nil ".")
  ; =>
  ; nil
  ;
  ; @return (string)
  ([n x]
   (not-starts-with! n x {}))

  ([n x options]
   (if (starts-with?              n x options)
       (cut/after-first-occurence n x)
       (-> n))))

(defn matches-with?
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @example
  ; (matches-with? "abc" "ab")
  ; =>
  ; false
  ;
  ; @example
  ; (matches-with? "abc" "abc")
  ; =>
  ; true
  ;
  ; @example
  ; (matches-with? "abc" "Abc")
  ; =>
  ; false
  ;
  ; @example
  ; (matches-with? "abc" "Abc" {:case-sensitive? false})
  ; =>
  ; true
  ;
  ; @return (boolean)
  ([n x]
   (matches-with? n x {}))

  ([n x {:keys [case-sensitive?] :or {case-sensitive? true}}]
   (let [n (str n)
         x (str x)]
        (or (= n x)
            (and (not case-sensitive?)
                 (= (clojure.string/lower-case n)
                    (clojure.string/lower-case x)))))))

(defn not-matches-with?
  ; @param (*) n
  ; @param (*) x
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @example
  ; (not-matches-with? "abc" "ab")
  ; =>
  ; true
  ;
  ; @example
  ; (not-matches-with? "abc" "abc")
  ; =>
  ; false
  ;
  ; @example
  ; (not-matches-with? "abc" "Abc")
  ; =>
  ; true
  ;
  ; @example
  ; (not-matches-with? "abc" "Abc" {:case-sensitive? false})
  ; =>
  ; false
  ;
  ; @return (boolean)
  ([n x]
   (not-matches-with? n x {}))

  ([n x options]
   (let [matches-with? (matches-with? n x options)]
        (not matches-with?))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn starts-at?
  ; @param (*) n
  ; @param (*) x
  ; @param (integer) cursor
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (starts-at? "The things you used to own, now they own you." "things" 4)
  ;
  ; @example
  ; (starts-at? "The things you used to own, now they own you." "things" 4)
  ; =>
  ; true
  ;
  ; @example
  ; (starts-at? "The things you used to own, now they own you." "things" 2)
  ; =>
  ; false
  ;
  ; @return (boolean)
  ([n x cursor]
   (starts-at? n x cursor {}))

  ([n x cursor {:keys [case-sensitive?] :or {case-sensitive? true}}]
   (let [n (str n)
         x (str x)]
        (letfn [(f [n x] (= x (subs n cursor (-> x count (+ cursor)))))]
               (if case-sensitive? (f n x)
                                   (f (clojure.string/lower-case n)
                                      (clojure.string/lower-case x)))))))

(defn ends-at?
  ; @param (*) n
  ; @param (*) x
  ; @param (integer) cursor
  ; @param (map)(opt) options
  ; {:case-sensitive? (boolean)
  ;   Default: true}
  ;
  ; @usage
  ; (ends-at? "The things you used to own, now they own you." "things" 10)
  ;
  ; @example
  ; (ends-at? "The things you used to own, now they own you." "things" 10)
  ; =>
  ; true
  ;
  ; @example
  ; (ends-at? "The things you used to own, now they own you." "things" 15)
  ; =>
  ; false
  ;
  ; @return (boolean)
  ([n x cursor]
   (ends-at? n x cursor {}))

  ([n x cursor {:keys [case-sensitive?] :or {case-sensitive? true}}]
   (let [n (str n)
         x (str x)]
        (letfn [(f [n x] (= x (subs n (- cursor (count x)) cursor)))]
               (if case-sensitive? (f n x)
                                   (f (clojure.string/lower-case n)
                                      (clojure.string/lower-case x)))))))
