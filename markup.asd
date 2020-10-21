;; Copyright 2019, Modern Interpreters Inc

(asdf:defsystem #:markup
  :description "markup provides a reader-macro to read HTML/XML tags inside of Common Lisp code"
  :author "Arnold Noronha <arnold@tdrhq.com>"
  :license  "Apache License, Version 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:str
               #:alexandria
               #:named-readtables
               #:bordeaux-threads ;; for locks
               #:trivial-gray-streams)
  :components ((:file "package")
               (:file "stream")
               (:file "markup")
               (:file "walk")))
