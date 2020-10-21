(defpackage :markup.test-symbol-info
  (:use :cl
   :alexandria
        :fiveam)
  (:import-from :markup
   :symbol-info
   :*symbol-infos*))
(in-package :markup.test-symbol-info)

(def-suite* :markup.test-symbol-info)

(test symbol-info
  (let ((*symbol-infos* nil))
    (is (eql (symbol-info 'foo) (symbol-info 'foo)))
    (is (eql (symbol-info 'bar) (symbol-info 'bar)))
    (is (not (eql (symbol-info 'foo) (symbol-info 'bar))))))
