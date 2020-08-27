;;;; lisp-markup.el
;;;; Charles Jackson
(require 'sgml-mode)
(require 'lisp-mode)
(require 'mmm-mode)

(defun lisp-back-matcher ()
  (forward-char -1)
  (with-syntax-table lisp-mode-syntax-table
    (forward-sexp))
  0)

(defun html-back-matcher ()
  (forward-char -2)
  (with-syntax-table html-mode-syntax-table
    (sgml-skip-tag-forward 1))
  0)

;;; add mmm classes
(mmm-add-classes
 '((lisp-in-html
    :submode lisp
    :face nil
    :front ",(\\|,@\\|=("
    :include-front t
    :back "."
    :back-offset (lisp-back-matcher))
   (html-in-lisp
    :submode html
    :face nil
    :front "\\(?1:<\\)[^/=[:space:]]"
    :front-match 1
    :include-front t
    :back "."
    :back-offset (html-back-matcher))))
(mmm-add-mode-ext-class 'lisp-mode nil 'html-in-lisp)
(mmm-add-mode-ext-class 'lisp-mode nil 'lisp-in-html)
(provide 'lisp-markup-mode)
;;; lisp-markup.el ends here
