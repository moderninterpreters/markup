;; Copyright 2019, Modern Interpreters Inc

(defpackage :markup/optimizer
  (:use #:cl)
  (:import-from #:markup/markup
                #:optimize-markup
                #:abstract-xml-tag
                #:xml-tag-attributes
                #:standard-name?
                #:xml-tag-children
                #:xml-tag-name
                #:make-xml-tag)
  (:local-nicknames (#:a #:alexandria)))
(in-package :markup/optimizer)

(defclass lazy-xml-tag (abstract-xml-tag)
  ((delegate )
   (xml-tag-builder :initarg :builder
                    :reader xml-tag-builder)))

(defmethod print-object :around ((tree lazy-xml-tag) stream)
  (handler-case
      (call-next-method)
    (error (e)
      (format stream "#<LAZY-XML-TAG error printing>"))))

(defun delegate (tag)
  (if (slot-boundp tag 'delegate)
      (slot-value tag 'delegate)
      (setf (slot-value tag 'delegate)
            (funcall (xml-tag-builder tag)))))

(defmethod xml-tag-name ((tag abstract-xml-tag))
  (xml-tag-name (delegate tag)))

(defmethod xml-tag-attributes ((Tag abstract-xml-tag))
  (xml-tag-attributes (delegate tag)))

(defmethod xml-tag-children ((Tag abstract-xml-tag))
  (xml-tag-children (delegate tag)))

(defun optimize-markup (tree)
  "Rewrite the tree of (make-xml-tag ...)s into something that can be
  more efficiently rendered in the most frequent case."

  ;; First we rewrite any "parameters" into a top-level LET. This
  ;; let's us do more cleverer operations on this later.
  (let ((params)
        (register-counter 0))
    (labels ((make-sym ()
               (intern (format nil "R~a" (incf register-counter))))
             (walk-alist (x)
               (when x
                 (assert (eql 'list (car x)))
                 (list* 'list
                         (loop for (nil k  v) in (cdr x)
                               collect `(cons ,k ,(walk v))))))
             (walk-list (x)
               (when x ;; for test readability
                 (assert (eql 'list (car x)))
                 (list* 'list
                         (mapcar #'walk (cdr x)))))
             (walk (sexp)
               "Replaces the node with the optimized version, with all
             parameters replaced and rewritten in params."
               (cond
                 ((null sexp)
                  sexp)
                 ((and (consp sexp)
                       (eql 'make-xml-tag (car sexp))
                       (or
                        (keywordp (cadr sexp))
                        (standard-name? (cadr (cadr sexp)))))
                  (destructuring-bind (name &key attributes children unused)
                      (cdr sexp)
                    (list
                     'make-xml-tag
                      name
                      :attributes (walk-alist attributes)
                      :children (walk-list children)
                      :unused unused)))
                 ((and (consp sexp)
                       (stringp (car sexp)))
                  ;; attributes?
                  (cons
                   (car sexp)
                   (walk (cdr sexp))))
                 ((and
                   (atom sexp)
                   (not (symbolp sexp)))
                  sexp)
                 (t
                  ;; this needs to be evaluated separately
                  (let ((sym (make-sym)))
                    (push (list sym sexp) params)
                    sym)))))
      (let ((inner (walk tree)))

        ;; At this point `inner` is comprised of just MAKE-XML-TAG,
        ;; LIST, CONS, and symbols referening registers or symbols
        ;; referencing names. This allows for an easier optimization.
        ;;
        ;; As of this writing/commit, there's no actual
        ;; optimizer. Please check in here again to see if this
        ;; comment is removed. We should have an optimizer soon that
        ;; does not explode the DOM in most cases.


        `(let ,(reverse params)
           ,(cond
             ((symbolp inner)
              inner)
             (t
              `(make-lazy-xml-tag
                ,inner))))))))

(defmacro make-lazy-xml-tag (body)
  `(make-instance 'lazy-xml-tag
                   :builder (lambda ()
                              ,body)))
