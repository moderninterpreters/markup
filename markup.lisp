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
        :wbr))


(defvar *standard-names*
  (list "span"
        "div"
        "script"
        "i"
        "em"
        "canvas"
        "img"
        "path"
        "hidden"
        "svg"
        "polygon"
        "g"
        "rect"
        "nav"
        "section"
        "footer"
        "strong"
        "a"
        "h1"
        "h2"
        "th"
        "thead"
        "br"
        "meta"
        "title"
        "link"
        "table"
        "head"
        "td"
        "tr"
        "body"
        "html"
        "li"
        "form"
        "input"
        "button"
        "ul"
        "h3"
        "h4"
        "label"
        "small"
        "option"
        "select"
        "h5"
        "h6"
        "p"
        "hr"
        "textarea"
        "a"
        "tt"
        "bold"
        "style"))

(defun tag-name-to-symbol (name)
  (if (member name *standard-names* :test #'equal)
      (intern  (string-upcase name) "KEYWORD")
      (let ((sym (read-from-string name)))
        (cond
          ((keywordp sym) sym)
          (t (list 'quote sym))))))


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
                 (setq state (min (+ 1 state) 2)))
                ((and
                  (eq (peek-next-char) #\>)
                  (eq state 2))
                 (setq state 3))
                (t
                 (setq state 0)))
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
      (setq attributes (read-attributes stream))
      (read-whitespace stream)

      (when (eq #\/ (peek-next-char))
        (setq ends-with-slash t)
        (read-next-char))
      (assert (eq #\> (read-next-char))) ;; to read #\>

      (unless ends-with-slash
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
                     (push (read-preserving-whitespace stream) children))
                    (t
                     (push "," children))))
                 (t
                  (push (read-string-from-xml stream next-char) children))))))

        (setq children (reverse children))

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


      (let ((ret (list 'make-xml-tag (tag-name-to-symbol name))))
        (when attributes
          (setq ret
                (append ret
                        (list
                         :attributes
                         `(list
                           ,@(loop for att in attributes
                                collect `(cons ,(car att) ,(cdr att))))))))
        (when children
          (setq ret (append ret (list :children `(list ,@children)))))
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
    ((keywordp name)
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

(defgeneric write-xml-to-stream (tree stream))

(defmethod write-xml-to-stream ((tree integer) stream)
  (format stream "~A" tree))


(defun write-xml (tree)
  (let ((stream (make-string-output-stream)))
    (write-xml-to-stream tree stream)
    (get-output-stream-string stream)))

(defun format-attr-val (val)
  (if val
      (format nil "\"~A\"" (cl-who:escape-string (format nil "~a" val)))
      "\"\""))


(defun write-attributes (attributes stream)
  (loop for attr in attributes
     do
       (format stream " ~A=~A" (car attr) (format-attr-val (cdr attr)))))

(defmethod write-xml-to-stream ((tree xml-tag) stream)
  (format stream "<~A" (string-downcase (xml-tag-name tree)))

  (write-attributes (xml-tag-attributes tree) stream)

  (cond
    ((not (member (xml-tag-name tree) *void-tags*))
     (format stream ">")
     (loop for child in (xml-tag-children tree)
        do
          (write-xml-to-stream child stream))
     (format stream "</~A>" (string-downcase (xml-tag-name tree))))
    (t
     (format stream " />"))))


(defmethod write-xml-to-stream ((tree string) stream)
  (format stream "~A" (cl-who:escape-string-minimal tree)))

(defmethod write-xml-to-stream ((tree xml-merge-tag) stream)
  (loop for child in (xml-tag-children tree)
     do
       (write-xml-to-stream child stream)))

(defmethod write-xml-to-stream ((tree (eql nil)) stream))

(defun write-xml-to-stdout (tree)
  (write-xml-to-stream tree *standard-output*))

(defmethod print-object ((tree xml-tag) stream)
  (write-xml-to-stream tree stream))

(defmethod print-object ((tree xml-merge-tag) stream)
  (write-xml-to-stream tree stream))

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
  (let ((args
         (cond
           ((not args) (list (gensym)))
           ((eq '&key (car args)) (cons (gensym) args))
           (t args))))
    `(%deftag ,name (,@args) ,@body)))


(defclass unescaped-string ()
  ((content :initarg :content :accessor unescaped-string-content)))

(defmethod write-xml-to-stream ((tree unescaped-string) stream)
  (format stream "~a" (unescaped-string-content tree)))

(defmethod print-object ((tree unescaped-string) stream)
  (write-xml-to-stream tree stream))

(defun unescaped (string)
  (make-instance 'unescaped-string :content string))
