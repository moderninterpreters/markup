(in-package :markup)

(defclass symbol-info ()
  ((symbol :type symbol
           :initarg :symbol)
   (hidden-symbol :type symbol
                  :initform (gensym (string symbol))
                  :reader %hidden-symbol)
   (void-tag-p :type boolean
               :accessor void-tag-p
               :initarg :void-tag-p)
   (fdefinition :type function
                :accessor si-fdefinition
                :initarg :fdefinition)))

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
