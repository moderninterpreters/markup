(defsystem :markup.test
  :description "Describe markup here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :depends-on (:markup
               :fiveam)
  :serial t
  :components ((:file "test-markup"))

  :perform (test-op (op system)
                    (funcall (read-from-string "fiveam:run-all-tests"))))
