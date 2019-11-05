;; Copyright 2019, Modern Interpreters Inc

(in-package #:markup)

(defmacro enable-reader ()
  `(named-readtables:in-readtable syntax))

(defun read-tag (stream)
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t)))
    (coerce
     (loop
        until
           (or
            (whitespacep (peek-next-char))
            (eq (peek-next-char) #\/)
            (eq (peek-next-char) #\>))
        collect
          (progn
            (read-next-char)))
     'string)))

(defparameter *void-tags*
  (mapcar #'symbol-name
   (list :br
         :hr
         :img
         :input
         :link
         :meta
         :area
         :base
         :col
         :command
         :embed
         :keygen
         :param
         :source
         :track
         :wbr)))


(defvar *standard-names*
  (mapcar #'symbol-name
          (list :span
                :div
                :script
                :i
                :header
                :template
                :b
                :ol
                :em
                :canvas
                :img
                :path
                :hidden
                :svg
                :polygon
                :g
                :rect
                :nav
                :section
                :footer
                :strong
                :a
                :h1
                :h2
                :th
                :thead
                :br
                :meta
                :title
                :link
                :table
                :head
                :td
                :tr
                :body
                :html
                :li
                :form
                :input
                :button
                :ul
                :h3
                :h4
                :label
                :small
                :option
                :select
                :h5
                :h6
                :p
                :hr
                :textarea
                :a
                :tt
                :bold
                :style)))

(defun read-tag-from-string (name)
  (let ((name (read-from-string name)))
    (cond
      ((keywordp name) name)
      (t (list 'quote name)))))

(defun whitespacep (char)
  (or
   (eq char #\Space)
   (eq char #\Newline)
   (eq char #\Linefeed)))

(defun read-whitespace (stream)
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t)))
    (loop
       while (whitespacep (peek-next-char))
       do (read-next-char))
    (peek-next-char)))

(defun read-attr-key (stream)
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t)))
    (coerce
     (loop
        until  (or
                (eq #\= (peek-next-char))
                (eq #\/ (peek-next-char))
                (eq #\> (peek-next-char)))


        collect (read-next-char))
     'string)))

(defun read-attr-val (stream)
  (read-preserving-whitespace stream))


(defun read-attributes (stream)
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t)))
    (read-whitespace stream)
    (loop
       until (or
              (eq (peek-next-char) #\>)
              (eq (peek-next-char) #\/))
       collect
         (let ((attr-key (read-attr-key stream)))
           (cons attr-key
                 (progn
                   (let ((next-char (read-next-char)))
                     (assert (eq #\= next-char)
                             () "expected = after attribute ~S" attr-key))
                   (let ((ret (read-attr-val stream)))
                     (read-whitespace stream)
                     ret)))))))

(defun read-string-from-xml (stream next)
  (declare (ignore next))
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t)))
    (coerce
     (loop
        until (or
               (eq (peek-next-char) #\<)
               (eq (peek-next-char) #\,))
        collect (read-next-char))
     'string)))

(defun read-comment (stream)
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t)))
    (let ((state 0))
      (coerce
       (loop while (< state 3)
          collect
            (progn
              (cond
                ((eq (peek-next-char) #\-)
                 (setf state (min (+ 1 state) 2)))
                ((and
                  (eq (peek-next-char) #\>)
                  (eq state 2))
                 (setf state 3))
                (t
                 (setf state 0)))
              (read-next-char)))
       'string))))

(defun read-xml-after-bracket (stream char)
  (declare (ignore char))
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t)))
    (let ((name (read-tag stream))
          (ends-with-slash nil)
          children
          attributes)

      (if (equal name "")
          (return-from read-xml-after-bracket (intern "<" "CL")))

      (if (equal name "=")
          (return-from read-xml-after-bracket (intern "<=" "CL")))

      (if (equal name "!--")
          (return-from read-xml-after-bracket (list 'unescaped (concatenate 'string "<!--" (read-comment stream)))))

      (read-whitespace stream)
      (setf attributes (read-attributes stream))
      (read-whitespace stream)

      (when (eq #\/ (peek-next-char))
        (setf ends-with-slash t)
        (read-next-char))
      (assert (eq #\> (read-next-char))) ;; to read #\>

      (unless (or ends-with-slash (void-tag? name))
        (block children-loop
          (loop
             (let ((next-char (peek-next-char)))
               (cond
                 ((eq #\< next-char)
                  (read-next-char)
                  (when (eq (peek-next-char) #\/)
                    (return-from children-loop))
                  (push (read-xml-after-bracket stream (peek-next-char)) children))
                 ((eq #\, next-char)
                  (read-next-char)
                  (cond
                    ((eq #\@ (peek-next-char))
                     (read-next-char)
                     (push (list 'make-merge-tag (read-preserving-whitespace stream)) children))
                    ((eq #\( (peek-next-char))
                     (push `(make-escaped ,(read-preserving-whitespace stream)) children ))
                    (t
                     (push "," children))))
                 (t
                  (push (read-string-from-xml stream next-char) children))))))

        (setf children (reverse children))

        ;; now we reach the /name> part of this, so let's read it out
        (let ((next-char (read-next-char)))
          (unless (eq #\/ next-char)
            (error "expected to see a terminating element, got ~A" next-char)))

        (let ((end-name (read-tag stream)))
          (unless (equal end-name name)
            (error "ending of xml element doesn't match, got ~A instead of ~A" end-name name )))

        ;; read the #\>
        (if (not (eq #\> (read-next-char)))
            (error "not terminating with >")))


      (let ((ret (list 'make-xml-tag (read-tag-from-string name))))
        (when attributes
          (setf ret
                (append ret
                        (list
                         :attributes
                         `(list
                           ,@(loop for att in attributes
                                collect `(cons ,(car att) ,(cdr att))))))))
        (when children
          (setf ret (append ret (list :children `(list ,@children)))))
        ret))))


(defun read-xml (stream char)
  (declare (ignore char))
  (read-xml-after-bracket stream (peek-char nil stream t nil t)))

(defreadtable syntax
  (:merge :standard)
  (:macro-char #\< #'read-xml t))

(defun read-xml-from-string (string)
  (let ((stream (make-string-input-stream string)))
    (read-xml stream (read-char stream t nil t))))

(defclass xml-tag ()
  ((attributes :initarg :attributes :accessor xml-tag-attributes)
   (children :initarg :children :accessor xml-tag-children)
   (name :initarg :name :accessor xml-tag-name)))

(defclass xml-merge-tag ()
  ((children :initarg :children :accessor xml-tag-children)))

(defun make-merge-tag (children)
  (make-instance 'xml-merge-tag :children children))

(defun make-xml-tag (name &key children attributes)
  (cond
    ((or
      (keywordp name)
      (and (not (fboundp name))
           (member (symbol-name name) *standard-names* :test 'equal)))
     (make-instance 'xml-tag :name name
                    :children children
                    :attributes attributes))
    (t
     (let ((args))
       (when children
         (setf args (append (list :children children) args)))

       (when attributes
         (setf args (append (list :attributes attributes) args)))
       (apply (fdefinition name) args)))))

(defgeneric write-html-to-stream (tree stream))

(defmethod write-html-to-stream ((tree integer) stream)
  (format stream "~A" tree))


(defun write-html (tree)
  (let ((stream (make-string-output-stream)))
    (write-html-to-stream tree stream)
    (get-output-stream-string stream)))

(defun format-attr-val (val)
  (if val
      (format nil "\"~A\"" (cl-who:escape-string (format nil "~a" val)))
      "\"\""))


(defun write-attributes (attributes stream)
  (loop for attr in attributes
     if (cdr attr)
     do
       (format stream " ~A=~A" (car attr) (format-attr-val (cdr attr)))))

(defparameter *void-tag-cache* (make-hash-table))

(defun void-tag? (tag)
  (multiple-value-bind (res present-p) (gethash tag *void-tag-cache*)
    (cond
      (present-p res)
      (t
       (setf (gethash tag *void-tag-cache*)
             (let ((tag (if (stringp tag) (string-upcase tag)
                            (symbol-name tag))))
               (member tag *void-tags* :test 'equal)))))))

(defmethod write-html-to-stream ((tree xml-tag) stream)
  (let ((tag-name (string-downcase (xml-tag-name tree))))
    (when (equal tag-name "html")
      (format stream "<!DOCTYPE html>~%"))
    (format stream "<~A" tag-name))

  (write-attributes (xml-tag-attributes tree) stream)

  (cond
    ((not (void-tag? (xml-tag-name tree)))
     (format stream ">")
     (loop for child in (xml-tag-children tree)
        do
          (write-html-to-stream child stream))
     (format stream "</~A>" (string-downcase (xml-tag-name tree))))
    (t
     (format stream " />"))))


(defmethod write-html-to-stream ((tree string) stream)
  (format stream "~A" tree))

(defmethod write-html-to-stream ((tree xml-merge-tag) stream)
  (loop for child in (xml-tag-children tree)
     do
       (write-html-to-stream child stream)))

(defmethod write-html-to-stream ((tree (eql nil)) stream))

(defmethod print-object ((tree xml-tag) stream)
  (write-html-to-stream tree stream))

(defmethod print-object ((tree xml-merge-tag) stream)
  (write-html-to-stream tree stream))

(defun deftag-symbol (arg)
  (cond
    ((symbolp arg) arg)
    (t (car arg))))

(defun deftag-value (arg)
  (cond
    ((symbolp arg) nil)
    (t (cadr arg))))


(defmacro %deftag (name (children &optional (key-attr '&key) &rest args) &body body)
  (assert (eq '&key key-attr))
  `(defun ,name (&key (attributes nil) (children nil))
     (destructuring-bind (&key ,@args) (loop for x in attributes
                                          append (list (intern (string-upcase (car x)) "KEYWORD") (cdr x)))
       (let ((,children children))
         (declare (ignorable ,children))
         ,@body))))

(defmacro deftag (name (&rest args) &body body)
  "Define a new XML tag that.

Tags are namespaced so you if you define a tag foo in namespace bar
you can refer to it as <bar:foo>...</bar:foo>.

The lambda-list has a very specific syntax: it must be: ([children [&key attr*]).

If children is provided, it will be filled with the list of all
child tags while parsing. For instance <bar:foo>x<h1>y</h1></bar:foo>, will
set children as (\"x\" <h1>y</h1>).
"
  (let ((args
         (cond
           ((not args) (list (gensym)))
           ((eq '&key (car args)) (cons (gensym) args))
           (t args))))
    `(%deftag ,name (,@args) ,@body)))


(defclass unescaped-string ()
  ((content :initarg :content :accessor unescaped-string-content)))

(defclass escaped-string ()
  ((content :initarg :content :accessor escaped-string-content)))

(defun make-escaped (child)
  (make-instance 'escaped-string
                 :content child))

(defmethod write-html-to-stream ((tree unescaped-string) stream)
  (format stream "~a" (unescaped-string-content tree)))

(defmethod write-html-to-stream ((tree escaped-string) stream)
  (let ((content (escaped-string-content tree)))
    (case (type-of content)
      ('unescaped-string (write-html-to-stream content stream))
      ('xml-tag (write-html-to-stream content stream))
      ('xml-merge-tag (write-html-to-stream content stream))
      (t
       ;; else we need to escape this before writing
       (when content
         (format stream "~A" (who:escape-string-minimal (format nil "~A" content))))))))

(defmethod print-object ((tree unescaped-string) stream)
  (write-html-to-stream tree stream))

(defun unescaped (string)
  (make-instance 'unescaped-string :content string))

;; deprecated wrappers for now
(defun write-xml (tree)
  (write-html tree))

(defun write-xml-to-stream (tree stream)
  (write-html-to-stream tree stream))
