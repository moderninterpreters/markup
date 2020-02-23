;; Copyright 2019, Modern Interpreters Inc

(defpackage #:markup
  (:use #:cl
        #:named-readtables)
  (:export #:read-xml
           #:make-xml-tag
           #:make-merge-tag
           #:write-xml ;; deprecated
           #:write-html
           #:unescaped
           #:enable-reader
           #:write-xml-to-stream ;; deprecated
           #:write-html-to-stream
           #:deftag
           #:get-attr
           #:write
           #:syntax
           #:markup-enable-reader
           #:read-xml-from-string))
