
(ns fruits.string.api
    (:refer-clojure :exclude [empty? repeat])
    (:require [fruits.string.apply   :as apply]
              [fruits.string.bounds  :as bounds]
              [fruits.string.check   :as check]
              [fruits.string.config  :as config]
              [fruits.string.contain :as contain]
              [fruits.string.convert :as convert]
              [fruits.string.cover   :as cover]
              [fruits.string.cut     :as cut]
              [fruits.string.dex     :as dex]
              [fruits.string.filter  :as filter]
              [fruits.string.get     :as get]
              [fruits.string.inline  :as inline]
              [fruits.string.insert  :as insert]
              [fruits.string.length  :as length]
              [fruits.string.lines   :as lines]
              [fruits.string.order   :as order]
              [fruits.string.remove  :as remove]
              [fruits.string.replace :as replace]
              [fruits.string.search  :as search]
              [fruits.string.set     :as set]
              [fruits.string.trim    :as trim]
              [fruits.string.walk    :as walk]
              [fruits.string.escape :as escape]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

; @redirect (fruits.string.apply/*)
(def apply-on-range apply/apply-on-range)

; @redirect (fruits.string.bounds/*)
(def starts-with?      bounds/starts-with?)
(def ends-with?        bounds/ends-with?)
(def not-starts-with?  bounds/not-starts-with?)
(def not-ends-with?    bounds/not-ends-with?)
(def start-with        bounds/start-with)
(def end-with          bounds/end-with)
(def not-start-with    bounds/not-start-with)
(def not-end-with      bounds/not-end-with)
(def starts-at?        bounds/starts-at?)
(def ends-at?          bounds/ends-at?)
(def not-starts-at?    bounds/not-starts-at?)
(def not-ends-at?      bounds/not-ends-at?)
(def matches-with?     bounds/matches-with?)
(def not-matches-with? bounds/not-matches-with?)

; @redirect (fruits.string.check/*)
(def empty?      check/empty?)
(def not-empty?  check/not-empty?)
(def blank?      check/blank?)
(def not-blank?  check/not-blank?)
(def lowercase?  check/lowercase?)
(def uppercase?  check/uppercase?)

; @redirect (fruits.string.config/*)
(def EMPTY-STRING config/EMPTY-STRING)
(def BREAK        config/BREAK)
(def NEWLINE      config/NEWLINE)
(def WHITE-SPACE  config/WHITE-SPACE)

; @redirect (fruits.string.contain/*)
(def contains-part?   contain/contains-part?)
(def if-contains-part contain/if-contains-part)

; @redirect (fruits.string.convert/*)
(def to-integer     convert/to-integer)
(def to-capitalized convert/to-capitalized)
(def to-uppercase   convert/to-uppercase)
(def to-lowercase   convert/to-lowercase)
(def to-nil         convert/to-nil)
(def to-seqable     convert/to-seqable)

; @redirect (fruits.string.cover/*)
(def cover cover/cover)

; @redirect (fruits.string.cut/*)
(def keep-range             cut/keep-range)
(def cut-range              cut/cut-range)
(def before-first-occurence cut/before-first-occurence)
(def before-last-occurence  cut/before-last-occurence)
(def after-first-occurence  cut/after-first-occurence)
(def after-last-occurence   cut/after-last-occurence)
(def from-first-occurence   cut/from-first-occurence)
(def from-last-occurence    cut/from-last-occurence)
(def to-first-occurence     cut/to-first-occurence)
(def to-last-occurence      cut/to-last-occurence)
(def between-occurences     cut/between-occurences)

; @redirect (fruits.string.dex/*)
(def first-dex-of dex/first-dex-of)
(def last-dex-of  dex/last-dex-of)
(def nth-dex-of   dex/nth-dex-of)

; @redirect (fruits.string.escape/*)
(def escape-characters escape/escape-characters)

; @redirect (fruits.string.filter/*)
(def filter-characters filter/filter-characters)

; @redirect (fruits.string.get/*)
(def first-character  get/first-character)
(def second-character get/second-character)
(def last-character   get/last-character)
(def nth-character    get/nth-character)

; @redirect (fruits.string.inline/*)
(def inline-position      inline/inline-position)
(def left-spacing-length  inline/left-spacing-length)
(def right-spacing-length inline/right-spacing-length)
(def left-indent-length   inline/left-indent-length)
(def right-indent-length  inline/right-indent-length)
(def fix-inline-position  inline/fix-inline-position)

; @redirect (fruits.string.insert/*)
(def prefix      insert/prefix)
(def suffix      insert/suffix)
(def prepend     insert/prepend)
(def append      insert/append)
(def insert-part insert/insert-part)

; @redirect (fruits.string.length/*)
(def length          length/length)
(def same-length?    length/same-length?)
(def length-min?     length/length-min?)
(def length-max?     length/length-max?)
(def length-between? length/length-between?)
(def length?         length/length?)
(def max-length      length/max-length)

; @redirect (fruits.string.lines/*)
(def line-position          lines/line-position)
(def prev-line-position     lines/prev-line-position)
(def next-line-position     lines/next-line-position)
(def containing-line        lines/containing-line)
(def remove-containing-line lines/remove-containing-line)
(def in-empty-line?         lines/in-empty-line?)
(def not-in-empty-line?     lines/not-in-empty-line?)
(def in-blank-line?         lines/in-blank-line?)
(def not-in-blank-line?     lines/not-in-blank-line?)
(def remove-newlines        lines/remove-newlines)
(def newline-count          lines/newline-count)
(def line-count             lines/line-count)
(def limit-lines            lines/limit-lines)

; @redirect (fruits.string.order/*)
(def abc? order/abc?)
(def abc  order/abc)

; @redirect (fruits.string.remove/*)
(def remove-part            remove/remove-part)
(def remove-first-character remove/remove-first-character)
(def remove-last-character  remove/remove-last-character)
(def remove-first-occurence remove/remove-first-occurence)
(def remove-last-occurence  remove/remove-last-occurence)

; @redirect (fruits.string.replace/*)
(def replace-part     replace/replace-part)
(def use-replacements replace/use-replacements)
(def use-replacement  replace/use-replacement)
(def use-placeholder  replace/use-placeholder)

; @redirect (fruits.string.search/*)
(def occurence-count search/occurence-count)
(def occurence-min?  search/occurence-min?)
(def occurence-max?  search/occurence-max?)

; @redirect (fruits.string.set/*)
(def repeat set/repeat)
(def join   set/join)
(def split  set/split)

; @redirect (fruits.string.trim/*)
(def trim            trim/trim)
(def horizontal-trim trim/horizontal-trim)
(def vertical-trim   trim/vertical-trim)
(def trim-start      trim/trim-start)
(def trim-end        trim/trim-end)
(def trim-newlines   trim/trim-newlines)
(def trim-gaps       trim/trim-gaps)

; @redirect (fruits.string.walk/*)
(def walk walk/walk)
