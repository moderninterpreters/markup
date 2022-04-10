(defpackage :markup/test-optimizer
  (:use #:cl
        #:fiveam)
  (:import-from #:markup/markup
                #:make-xml-tag
                #:optimize-markup)
  (:local-nicknames (#:a #:alexandria)))
(in-package :markup/test-optimizer)


(def-suite* :markup/test-optimizer :in :markup)

(def-fixture state ()
  (&body))

(test happy-path
  (with-fixture state ()
   (is (equal '(let nil (make-xml-tag foo :attributes nil  :children nil :unused nil))
               (optimize-markup '(make-xml-tag foo :children nil :attributes nil :unused nil))))))

(test simple-escape
  (with-fixture state ()
    (is (equal '(let ((r1 (format nil "~a" car)))
                 (make-xml-tag foo :attributes nil :children (list r1) :unused nil))
                (optimize-markup '(make-xml-tag foo :children (list
                                                               (format nil "~a" car))
                                   :attributes nil
                                   :unused nil))))))

(test escaping-multiple
  (with-fixture state ()
    (is (equal '(let ((r1 (format nil "~a" car))
                      (r2 bar))
                 (make-xml-tag foo :attributes nil :children (list r1 r2) :unused nil))
                (optimize-markup '(make-xml-tag foo :children (list
                                                               (format nil "~a" car)
                                                               bar)
                                   :attributes nil
                                   :unused nil))))))

(test escaping-attributes
  (with-fixture state ()
    (is (equal '(let ((r1 (format nil "~a" car)))
                 (make-xml-tag foo :attributes (list (cons "car" r1)) :children nil
                                  :unused nil))
                (optimize-markup '(make-xml-tag foo :children nil
                                   :attributes (list (cons "car" (format nil "~a" car)))
                                   :unused nil))))))

(test attributes-get-the-first-register
  (with-fixture state ()
    (is (equal '(let ((r1 (format nil "~a" car))
                      (r2 bar))
                 (make-xml-tag foo :attributes (list (cons "car" r1)) :children (list r2)
                                   :unused nil))
                (optimize-markup '(make-xml-tag foo :children (list bar)
                                   :attributes (list (cons "car" (format nil "~a" car)))
                                   :unused nil))))))
