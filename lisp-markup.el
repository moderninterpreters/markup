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
(provide 'lisp-markup-mode)
;;; lisp-markup.el ends here
