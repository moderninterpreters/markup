(defpackage :markup/test-optimizer
  (:use #:cl
        #:fiveam)
  (:import-from #:markup/markup
                #:make-xml-tag
                #:optimize-markup)
  (:import-from #:markup/optimizer
                #:make-lazy-xml-tag)
  (:local-nicknames (#:a #:alexandria)))
(in-package :markup/test-optimizer)


(def-suite* :markup/test-optimizer :in :markup)

(def-fixture state ()
  (&body))

(test happy-path
  (with-fixture state ()
    (is (equal '(let nil
                 (make-lazy-xml-tag
                  (make-xml-tag 'div :attributes nil  :children nil :unused nil)))
               (optimize-markup '(make-xml-tag 'div :children nil :attributes nil :unused nil))))))

(test simple-escape
  (with-fixture state ()
    (is (equal '(let ((r1 (format nil "~a" car)))
                 (make-lazy-xml-tag
                  (make-xml-tag 'div :attributes nil :children (list r1) :unused nil)))
                (optimize-markup '(make-xml-tag 'div :children (list
                                                               (format nil "~a" car))
                                   :attributes nil
                                   :unused nil))))))

(test escaping-multiple
  (with-fixture state ()
    (is (equal '(let ((r1 (format nil "~a" car))
                      (r2 bar))
                 (make-lazy-xml-tag
                  (make-xml-tag 'span :attributes nil :children (list r1 r2) :unused nil)))
                (optimize-markup '(make-xml-tag 'span :children (list
                                                               (format nil "~a" car)
                                                               bar)
                                   :attributes nil
                                   :unused nil))))))

(test escaping-attributes
  (with-fixture state ()
    (is (equal '(let ((r1 (format nil "~a" car)))
                 (make-lazy-xml-tag
                  (make-xml-tag 'input :attributes (list (cons "car" r1)) :children nil
                                    :unused nil)))
                (optimize-markup '(make-xml-tag 'input :children nil
                                   :attributes (list (cons "car" (format nil "~a" car)))
                                   :unused nil))))))

(test attributes-get-the-first-register
  (with-fixture state ()
    (is (equal '(let ((r1 (format nil "~a" car))
                      (r2 bar))
                 (make-lazy-xml-tag
                  (make-xml-tag 'div :attributes (list (cons "car" r1)) :children (list r2)
                                    :unused nil)))
                (optimize-markup '(make-xml-tag 'div :children (list bar)
                                   :attributes (list (cons "car" (format nil "~a" car)))
                                   :unused nil))))))

(test if-the-tag-is-not-builtin-then-we-move-it-to-register
  (with-fixture state ()
    (is (equal '(let ((r1 (make-xml-tag 'foo :children nil :attributes nil  :unused nil)))
                 r1)
               (optimize-markup '(make-xml-tag 'foo :children nil :attributes nil :unused nil))))))
