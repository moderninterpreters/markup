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
               #:trivial-gray-streams)
  :components ((:file "stream")
               (:file "tags")
               (:file "markup")
               (:file "optimizer")
               (:file "walk")
               (:file "all")))

(defsystem :markup/lispworks
  :description "A LispWorks IDE editor mode for markup, ported from lisp-markup.el"
  :author "Arnold Noronha <arnold@tdrhq.com>"
  :license  "Apache License, Version 2.0"
  :serial t
  :depends-on (#:markup)
  :components ((:file "lispworks")))

(defsystem :markup/tests
  :description "Tests for the markup library"
  :author "Arnold Noronha <arnold@tdrhq.com>"
  :license  "Apache License, Version 2.0"
  :depends-on (:markup
               :fiveam
               ;; The editor mode only exists on LispWorks; on any other
               ;; implementation the test file below is skipped entirely,
               ;; so nothing here refers to the EDITOR package.
               (:feature :lispworks #:markup/lispworks))
  :serial t
  :components ((:file "test-markup")
               (:file "test-walk")
               (:file "test-optimizer")
               (:file "test-stream")
               (:file "test-lispworks" :if-feature :lispworks))

  :perform (test-op (op system)
                    (funcall (read-from-string "fiveam:run-all-tests"))))
