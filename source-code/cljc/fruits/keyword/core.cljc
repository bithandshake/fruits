
(ns fruits.keyword.core)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn join
  ; @param (keywords and/or strings in vector) n
  ; @param (string)(opt) delimiter
  ;
  ; @usage
  ; (join [:a :b "c"])
  ;
  ; @example
  ; (join [:a :b "c" :d])
  ; =>
  ; :abcd
  ;
  ; @example
  ; (join [:x/a :x/b "c" :d])
  ; =>
  ; :abcd
  ;
  ; @example
  ; (join [:a :b "c" :d] ".")
  ; =>
  ; :a.b.c.d
  ;
  ; @return (keyword)
  ([n]
   (join n nil))

  ([n delimiter]
   (letfn [(f0 [result x]
               (if (keyword? x) (str result (if result delimiter) (name x))
                                (str result (if result delimiter) x)))]
          (keyword (reduce f0 nil n)))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn prepend
  ; @param (keyword) n
  ; @param (keyword) x
  ; @param (string)(opt) delimiter
  ;
  ; @example
  ; (prepend :a :b)
  ; =>
  ; :ba
  ;
  ; @example
  ; (prepend :a/b :c)
  ; =>
  ; :a/cb
  ;
  ; @example
  ; (prepend :a/b :c "--")
  ; =>
  ; :a/c--b
  ;
  ; @return (keyword)
  ([n x]
   (if-let [namespace (namespace n)]
           ; If 'n' is namespaced ...
           (keyword namespace (str (name x) (name n)))
           ; If 'n' is NOT namespaced ...
           (keyword (str (name x) (name n)))))

  ([n x delimiter]
   (if-let [namespace (namespace n)]
           ; If 'n' is namespaced ...
           (keyword namespace (str (name x) delimiter (name n)))
           ; If 'n' is NOT namespaced ...
           (keyword (str (name x) delimiter (name n))))))

(defn append
  ; @param (keyword) n
  ; @param (keyword) x
  ; @param (string)(opt) delimiter
  ;
  ; @example
  ; (append :a :b)
  ; =>
  ; :ab
  ;
  ; @example
  ; (append :a/b :c)
  ; =>
  ; :a/bc
  ;
  ; @example
  ; (append :a/b :c "--")
  ; =>
  ; :a/b--c
  ;
  ; @return (keyword)
  ([n x]
   (if-let [namespace (namespace n)]
           ; If 'n' is namespaced ...
           (keyword namespace (str (name n) (name x)))
           ; If 'n' is NOT namespaced ...
           (keyword (str (name n) (name x)))))

  ([n x delimiter]
   (if-let [namespace (namespace n)]
           ; If 'n' is namespaced ...
           (keyword namespace (str (name n) delimiter (name x)))
           ; If 'n' is NOT namespaced ...
           (keyword (str (name n) delimiter (name x))))))
