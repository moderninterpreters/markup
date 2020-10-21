(in-package :markup)

(defclass symbol-info ()
  ((symbol :type symbol
           :initarg :symbol)
   (hidden-symbol :type symbol
                  :reader %hidden-symbol
                  :documentation "A symbol that references this symbol-info")
   (void-tag-p :type boolean
               :accessor void-tag-p
               :initarg :void-tag-p)
   (fdefinition :type function
                :accessor si-fdefinition
                :initarg :fdefinition)))

(defmethod initialize-instance :after ((symbol-info symbol-info) &key symbol &allow-other-keys)
  (with-slots (hidden-symbol) symbol-info
    (let ((sym (gensym (string symbol))))
      (setf (symbol-value sym) symbol-info)
      (setf hidden-symbol sym))))

(defvar *symbol-infos* nil)

;; This isn't meant to be fast. Ideally, we'd call this at macro
;; evaluation time. The symbol-info for a given symbol never changes.
(let ((lock (bt:make-lock)))
  (defun symbol-info (sym)
    (macrolet ((place ()  `(assoc-value *symbol-infos* sym)))
      (or (place)
          (bt:with-lock-held (lock)
            (or
             (place)
             (setf (place) (make-instance 'symbol-info
                                          :symbol sym))))))))

(defun (setf mdefinition) (val sym)
  (setf (si-fdefinition (symbol-info sym))
        val))

(defun %mdefinition (sym)
  (si-fdefinition (symbol-info sym)))

#+nil
(defmacro %mdefinition (sym)
  `(mdefinition ,sym))
