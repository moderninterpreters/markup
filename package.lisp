;; Copyright 2019, Modern Interpreters Inc

(defpackage #:markup
  (:use #:cl
        #:named-readtables)
  (:export #:read-xml
           #:make-xml-tag
           #:make-merge-tag
           #:write-xml ;; deprecated
           #:write-html
           #:xml-tag-name
           #:xml-tag-attributes
           #:unescaped
           #:xml-merge-tag-children
           #:xml-merge-tag
           #:enable-reader
           #:write-xml-to-stream ;; deprecated
           #:write-html-to-stream
           #:deftag
           #:walk
           #:add-attrs
           #:xml-tag
           #:xml-tag-children
           #:get-attr
           #:write
           #:syntax
           #:markup-enable-reader
           #:read-xml-from-string))
