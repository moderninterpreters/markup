(defsystem :markup.test
  :description "Tests for the markup library"
  :author "Arnold Noronha <arnold@tdrhq.com>"
  :license  "Apache License, Version 2.0"
  :depends-on (:markup
               :fiveam)
  :serial t
  :components ((:file "test-markup"))

  :perform (test-op (op system)
                    (funcall (read-from-string "fiveam:run-all-tests"))))