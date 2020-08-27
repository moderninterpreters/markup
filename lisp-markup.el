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

;;; indentation
(defun lisp-html-indent-line ()
  "Indent a line of lisp or html.
Only works in when mmm-mode is active."
  (interactive)
  (save-excursion
    (widen)
    (back-to-indentation)
    (cl-flet ((special-lisp-html-indent-line ()
                (let ((prev-exp-ident
                       (save-excursion
                         (funcall (if (equal prev-mm 'lisp-mode)
                                      #'backward-sexp
                                    #'sgml-skip-tag-backward)
                                  1)
                         (skip-chars-backward ",@")
                         (- (point) (progn (beginning-of-line) (point))))))
                  (indent-line-to prev-exp-ident)))
                (lisp-html-submode ()
                  (mmm-update-current-submode)
                  (or mmm-current-submode 'lisp-mode)))
      (let ((prev-mm (save-excursion
                       (forward-line -1)
                       (end-of-line)
                       (lisp-html-submode)))
            (beg-line-mm (save-excursion
                           (beginning-of-line)
                           (lisp-html-submode)))
            (curr-mm (lisp-html-submode))
            (closing-p (looking-at "</")))
        (cond
         ;; special indent
         ((and (equal prev-mm curr-mm)
               (not (equal curr-mm beg-line-mm)))
          (special-lisp-html-indent-line))
         ;; sgml indent
         ((or (and (equal prev-mm 'html-mode)
                   (equal curr-mm 'html-mode))
              (and (equal prev-mm 'html-mode)
                   (equal curr-mm 'lisp-mode))
              (and (equal prev-mm 'lisp-mode)
                   closing-p))
          (with-syntax-table html-mode-syntax-table
            (sgml-indent-line)))
         ;; lisp indent
         (:else (with-syntax-table lisp-mode-syntax-table
                  (let ((indent (calculate-lisp-indent)))
                    (cond ((listp indent) (special-lisp-html-indent-line)) ;special indent if lisp-indent is confused
                          (indent (indent-line-to indent))))))))))
  (when (< (point) (save-excursion (back-to-indentation) (point)))
    (back-to-indentation)))
(defun lisp-html-indent-region (beg end)
  "Indent a region of Lisp and Html mixed together.
Just calls `lisp-html-indent-line' on every line of the region."
  (interactive "r")
  (cl-flet ((current-line () (count-lines (point-min) (line-beginning-position))))
    (save-excursion
      (goto-char beg)
      (let ((last-line (line-number-at-pos end)))
        (while (<= (current-line) last-line)
          (lisp-html-indent-line)
          (forward-line 1))))))
(provide 'lisp-markup-mode)
;;; lisp-markup.el ends here
