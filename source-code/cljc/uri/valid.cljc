
(ns uri.valid
    (:require [noop.api    :refer [return]]
              [string.api  :as string]
              [uri.convert :as convert]))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defn valid-url
  ; @param (string) n
  ;
  ; @usage
  ; (valid-url "my-domain.com")
  ;
  ; @example
  ; (valid-url "my-domain.com")
  ; =>
  ; "https://my-domain.com"
  ;
  ; @example
  ; (valid-url "my-domain.com/")
  ; =>
  ; "https://my-domain.com"
  ;
  ; @example
  ; (valid-url "http://my-domain.com")
  ; =>
  ; "http://my-domain.com"
  ;
  ; @example
  ; (valid-url "/my-path")
  ; =>
  ; "/my-path"
  ;
  ; @example
  ; (valid-url "my-path")
  ; =>
  ; "/my-path"
  ;
  ; @return (string)
  [n]
  ; The 'valid-url' function ...
  ; ... checks if the 'n' string contains a valid domain.
  ; ... converts the value to a lowercase string.
  ; ... removes the trailing slash (if necessary).
  ; ... prepends the protocol (if necessary).
  (if-let [domain (convert/to-domain n)]
          (if (string/contains-part? n "://")
              (-> n (convert/to-lowercase)
                    (string/not-ends-with! "/"))
              (-> n (convert/to-lowercase)
                    (string/not-ends-with! "/")
                    (string/starts-with!   "https://")))
          (-> n (string/not-ends-with! "/")
                (string/starts-with!   "/"))))

(defn valid-url-path
  ; @param (string) n
  ;
  ; @usage
  ; (valid-url-path "/my-path")
  ;
  ; @example
  ; (valid-url-path "my-path")
  ; =>
  ; "/my-path"
  ;
  ; @example
  ; (valid-url-path "/my-path")
  ; =>
  ; "/my-path"
  ;
  ; @example
  ; (valid-url-path "/my-path/")
  ; =>
  ; "/my-path"
  ;
  ; @return (string)
  [n]
  ; The 'valid-url-path' function ...
  ; ... takes the path from the given 'n' string.
  ; ... if given string not contains path, returns with the root path ("/").
  (if-let [url-path (convert/to-url-path n)]
          (return url-path)
          (return "/")))
