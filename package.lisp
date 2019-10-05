;; Copyright 2019, Modern Interpreters Inc

(defpackage #:markup
  (:use #:cl
        #:named-readtables)
  (:export #:read-xml
           #:make-xml-tag
           #:make-merge-tag
           #:write-xml
           #:unescaped
           #:enable-reader
           #:write-xml-to-stream
           #:deftag
           #:write-xml-to-stdout
           #:syntax
           #:markup-enable-reader
           #:read-xml-from-string))
