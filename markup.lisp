;; Copyright 2019, Modern Interpreters Inc

(in-package #:markup)

(defmacro enable-reader ()
  `(named-readtables:in-readtable syntax))

(define-condition html-parse-error (error)
  ((message :initarg :message)
   (stream :initarg :stream)
   (last-few-chars :initarg :last-few-chars)))

(defmethod print-object ((x html-parse-error) stream)
  (with-slots (message last-few-chars) x
   (if *print-escape* (call-next-method)
       (format stream "~a~%Most recent read chars:~%`~a`"
               message last-few-chars))))

(defun read-tag (stream)
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t)))
    (let ((response nil)
          (found-comment nil))
      (block loop
       (loop
          until
            (or
             (whitespacep (peek-next-char))
             (eql (peek-next-char) #\/)
             (eql (peek-next-char) #\>))
          do
            (progn
              (push (read-next-char) response)
              (when (equalp '(#\- #\- #\!) response)
                (return-from loop)))))
     (coerce (nreverse response) 'string))))

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


(defparameter *standard-names*
  (mapcar #'symbol-name
          (list :html
                :base
                :head
                :link
                :meta
                :style
                :title
                :body
                :address
                :article
                :aside
                :footer
                :header
                :h1
                :h2
                :h3
                :h4
                :h5
                :h6
                :hgroup
                :main
                :nav
                :section
                :blockquote
                :dd
                :dir
                :div
                :dl
                :dt
                :figcaption
                :figure
                :hr
                :li
                :main
                :ol
                :p
                :pre
                :ul
                :a
                :abbr
                :b
                :bdi
                :bdo
                :br
                :cite
                :code
                :data
                :dfn
                :em
                :i
                :kbd
                :mark
                :q
                :rb
                :rp
                :rt
                :rtc
                :ruby
                :s
                :samp
                :small
                :span
                :strong
                :sub
                :sup
                :time
                :tt
                :u
                :var
                :wbr
                :area
                :audio
                :img
                :map
                :track
                :video
                :applet
                :embed
                :iframe
                :noembed
                :object
                :param
                :picture
                :source
                :canvas
                :noscript
                :script
                :del
                :ins
                :caption
                :col
                :colgroup
                :table
                :tbody
                :td
                :tfoot
                :th
                :thead
                :tr
                :button
                :datalist
                :fieldset
                :form
                :input
                :label
                :legend
                :meter
                :optgroup
                :option
                :output
                :progress
                :select
                :textarea
                :details
                :dialog
                :menu
                :menuitem
                :summary
                :content
                :element
                :shadow
                :slot
                :template
                :acronym
                :applet
                :basefont
                :bgsound
                :big
                :blink
                :center
                :command
                :content
                :dir
                :element
                :font
                :frame
                :frameset
                :image
                :isindex
                :keygen
                :listing
                :marquee
                :menuitem
                :multicol
                :nextid
                :nobr
                :noembed
                :noframes
                :plaintext
                :shadow
                :spacer
                :strike
                :tt
                :xmp

                ;; these are not official HTML5 attributes, so I don't
                ;; know where they come from, but they are definitely
                ;; in the templates I use.
                :svg
                :path
                :rect
                :g
                :polygon
                )))

(defun read-tag-from-string (name)
  (let ((name (read-from-string name)))
    (cond
      ((keywordp name) name)
      (t (list 'quote name)))))

(defun whitespacep (char)
  (or
   (not (graphic-char-p char))
   (eql char #\Space)
   (eql char #\Newline)
   (eql char #\Linefeed)))

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
                (eql #\= (peek-next-char))
                (eql #\/ (peek-next-char))
                (eql #\> (peek-next-char)))


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
              (eql (peek-next-char) #\>)
              (eql (peek-next-char) #\/))
       collect
         (let ((attr-key (read-attr-key stream)))
           (cons attr-key
                 (progn
                   (let ((next-char (read-next-char)))
                     (assert (eql #\= next-char)
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
               (eql (peek-next-char) #\<)
               (eql (peek-next-char) #\,))
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
                ((eql (peek-next-char) #\-)
                 (setf state (min (+ 1 state) 2)))
                ((and
                  (eql (peek-next-char) #\>)
                  (eql state 2))
                 (setf state 3))
                (t
                 (setf state 0)))
              (read-next-char)))
       'string))))

(defun read-xml-after-bracket (stream char)
  (declare (ignore char))
  (flet ((peek-next-char () (peek-char nil stream t nil t))
         (read-next-char () (read-char stream t nil t))
         (parse-error (&rest args)
           (error 'html-parse-error
                  :message (apply 'format nil args)
                  :stream stream
                  :last-few-chars (last-few-chars stream))))
    (let ((name (read-tag stream))
          (ends-with-slash nil)
          children
          attributes)

      (if (string= name "")
          (return-from read-xml-after-bracket (intern "<" "CL")))

      (if (string= name "=")
          (return-from read-xml-after-bracket (intern "<=" "CL")))

      (if (string= name "!--")
          (return-from read-xml-after-bracket (list 'unescaped (concatenate 'string "<!--" (read-comment stream)))))

      (read-whitespace stream)
      (setf attributes (read-attributes stream))
      (read-whitespace stream)

      (when (eql #\/ (peek-next-char))
        (setf ends-with-slash t)
        (read-next-char))
      (assert (eql #\> (read-next-char))) ;; to read #\>

      (unless (or ends-with-slash (void-tag? (intern (string-upcase name) "KEYWORD")))
        (block children-loop
          (loop
             (let ((next-char (peek-next-char)))
               (cond
                 ((eql #\< next-char)
                  (read-next-char)
                  (when (eql (peek-next-char) #\/)
                    (return-from children-loop))
                  (push (read-xml-after-bracket stream (peek-next-char)) children))
                 ((eql #\, next-char)
                  (read-next-char)
                  (cond
                    ((eql #\@ (peek-next-char))
                     (read-next-char)
                     (push (list 'make-merge-tag (read-preserving-whitespace stream)) children))
                    ((eql #\( (peek-next-char))
                     (push `(make-escaped ,(read-preserving-whitespace stream)) children ))
                    (t
                     (push "," children))))
                 (t
                  (push (read-string-from-xml stream next-char) children))))))

        (setf children (reverse children))

        ;; now we reach the /name> part of this, so let's read it out
        (let ((next-char (read-next-char)))
          (unless (eql #\/ next-char)
            (parse-error "expected to see a terminating element, got ~A" next-char)))

        (let ((end-name (read-tag stream)))
          (unless (equal end-name name)
            (parse-error "ending of xml element doesn't match, got ~A instead of ~A" end-name name )))

        ;; read the #\>
        (if (not (eql #\> (read-next-char)))
            (parse-error "not terminating with >")))


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


(defmethod read-xml (stream char)
  (read-xml (wrap-stream stream) char))

(defun last-few-chars (stream)
  (let ((str (read-so-far stream))
        (num 80))
    (str:substring (- (length str) num)
                   (length str)
                   str)))

(defmethod read-xml ((stream markup-stream) char)
  (declare (ignore char))
  (handler-bind ((html-parse-error
                  (lambda (e)
                    (declare (ignore e))
                    nil))
                 (error
                  (lambda (e)
                    (declare (ignore e))
                    (warn "Got error while reading HTML. The last few characters we read were: ~% ~a"
                          (last-few-chars stream)))))
      (read-xml-after-bracket stream (peek-char nil stream t nil t))))

(defreadtable syntax
  (:merge :standard)
  (:macro-char #\< #'read-xml t))

(defun read-xml-from-string (string)
  (let ((stream (make-string-input-stream string)))
    (read-xml stream (read-char stream t nil t))))

(defclass xml-tag ()
  ((attributes :initform nil
               :initarg :attributes
               :accessor xml-tag-attributes
               :type (or null cons))
   (children :initform nil
             :initarg :children
             :accessor xml-tag-children
             :type (or null cons))
   (name :initform 'dummy
         :initarg :name
         :accessor xml-tag-name
         :type symbol)))

(defstruct xml-merge-tag
  (children nil :type (or null cons)))

(defun make-merge-tag (children)
  (make-xml-merge-tag :children children))

(defun make-xml-tag (name &key children attributes)
  (cond
    ((or
      (keywordp name)
      (and (not (fboundp name))
           (standard-name? name)))
     (make-instance 'xml-tag
                    :name (intern (string name) "KEYWORD")
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
  (with-output-to-string (stream)
    (write-html-to-stream tree stream)))

(defmacro make-escape-map (&rest alist)
  `(let ((ar (make-array 32000 :element-type '(or null string) :initial-element nil)))
     (loop for (char . escaped) in ',alist
        do (setf (aref ar (char-code char)) escaped))
     ar))

(defparameter *escape-map*
  (make-escape-map (#\& . "&amp;")
                   (#\< . "&lt;")
                   (#\> . "&gt;")
                   (#\" . "&quot;")
                   (#\' . "&#39;")))


;; this mapping is taken from CL-WHO's escape-char-minimal
(defparameter *escape-minimal-map*
  (make-escape-map (#\& . "&amp;")
                   (#\< . "&lt;")
                   (#\> . "&gt;")))

;; This function was copied and tweaked from LSX. Previously I
;; depended on CL-WHO for escaping, but this is better.
(defun print-escaped-text (value stream &optional (escape-map *escape-map*))
  (declare (type string value)
           (type (simple-array (or null string) (*)) escape-map)
           (optimize speed))
  (loop for char of-type character across value
     for escaped = (if (< (char-code char) 256) (aref escape-map (char-code char)))
     if escaped
     do (write-string escaped stream)
     else do (write-char char stream)))

(defun print-escaped-text-minimal (value stream)
  (print-escaped-text value stream *escape-minimal-map*))

(defun format-attr-val (stream val)
  (declare (optimize speed))
  (let ((val (cond
               ((null val) "")
               ((stringp val) val)
               (t (format nil "~a" val)))))
    (write-char #\" stream)
    (print-escaped-text val stream)
    (write-char #\" stream)))


(defun write-attributes (attributes stream)
  (declare (type (or null cons) attributes))
  (declare (optimize speed))
  (dolist (attr attributes)
    (declare (type cons attr))
    (when (cdr attr)
       (write-char #\Space stream)
       (write-string (car attr) stream)
       (write-char #\= stream)
       (format-attr-val stream (cdr attr)))))

(defparameter *void-tag-cache* (make-hash-table))

(defun void-tag? (tag)
  (declare (type symbol tag))
  (declare (optimize speed))
  (multiple-value-bind (res present-p) (gethash tag *void-tag-cache*)
    (cond
      (present-p res)
      (t
       (setf (gethash tag *void-tag-cache*)
             (let ((tag (if (stringp tag) (string-upcase tag)
                            (symbol-name tag))))
               (member tag *void-tags* :test 'string=)))))))

(defvar *standard-name-cache* (make-hash-table))

(defun standard-name? (tag)
  (declare (type symbol tag))
  (declare (optimize speed))
  (multiple-value-bind (res present-p) (gethash tag *standard-name-cache*)
    (cond
      (present-p res)
      (t
       (setf (gethash tag *standard-name-cache*)
             (member (symbol-name tag) *standard-names* :test 'string=))))))

(defmethod write-html-to-stream ((tree xml-tag) stream)
  (declare (optimize speed))
  (let ((tag-name (string-downcase (xml-tag-name tree))))
    (when (equal tag-name "html")
      (format stream "<!DOCTYPE html>~%"))
    (funcall (formatter "<~A") stream tag-name))

  (write-attributes (xml-tag-attributes tree) stream)

  (cond
    ((not (void-tag? (xml-tag-name tree)))
     (write-string ">" stream)
     (dolist (child  (xml-tag-children tree))
       (write-html-to-stream child stream))
     (funcall (formatter "</~A>") stream (string-downcase (xml-tag-name tree))))
    (t
     (write-string " />" stream))))

(defmethod get-attr ((xml-tag xml-tag) name)
  (alexandria:assoc-value (xml-tag-attributes xml-tag)
                          name
                        :test 'equal))


(defmethod write-html-to-stream ((tree string) stream)
  (declare (optimize speed))
  (funcall (formatter "~A") stream tree))

(defmethod write-html-to-stream ((tree xml-merge-tag) stream)
  (declare (optimize speed))
  (loop for child in (xml-merge-tag-children tree)
     do
       (write-html-to-stream child stream)))

(defmethod write-html-to-stream ((tree (eql nil)) stream)
  (declare (optimize speed))  )

(defmethod print-object ((tree xml-tag) stream)
  (declare (optimize speed))
  (write-html-to-stream tree stream))

(defmethod print-object ((tree xml-merge-tag) stream)
  (declare (optimize speed))
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
  (assert (eql '&key key-attr))
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
           ((eql '&key (car args)) (cons (gensym) args))
           (t args))))
    `(%deftag ,name (,@args) ,@body)))


(defclass unescaped-string ()
  ((content :initarg :content :accessor unescaped-string-content)))

(defclass escaped-string ()
  ((content :initarg :content :accessor escaped-string-content)))

(defun make-escaped (child)
  (typecase child
    (xml-tag
     child)
    (t (make-instance 'escaped-string
                    :content child))))

(defmethod write-html-to-stream ((tree unescaped-string) stream)
  (declare (optimize speed))
  (funcall (formatter "~a") stream (unescaped-string-content tree)))

(defmethod write-html-to-stream ((tree escaped-string) stream)
  (declare (optimize speed))
  (let ((content (escaped-string-content tree)))
    (case (type-of content)
      ('unescaped-string (write-html-to-stream content stream))
      ('xml-tag (write-html-to-stream content stream))
      ('xml-merge-tag (write-html-to-stream content stream))
      (t
       ;; else we need to escape this before writing
       (when content
         (print-escaped-text-minimal (format nil "~A" content) stream))))))

(defmethod print-object ((tree unescaped-string) stream)
  (declare (optimize speed))
  (write-html-to-stream tree stream))

(defun unescaped (string)
  (make-instance 'unescaped-string :content string))

;; deprecated wrappers for now
(defun write-xml (tree)
  (write-html tree))

(defun write-xml-to-stream (tree stream)
  (write-html-to-stream tree stream))

(deftag merge-tag (children)
  (make-merge-tag children))
