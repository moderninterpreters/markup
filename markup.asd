;;;; markup.asd

(asdf:defsystem #:markup
  :description "Describe markup here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:named-readtables #:cl-who)
  :components ((:file "package")
               (:file "markup")))
