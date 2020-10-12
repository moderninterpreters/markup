;;;; lisp-markup.el
;;;; Charles Jackson
(require 'sgml-mode)
(require 'lisp-mode)

(defmacro with-<>-as-brackets (&rest body)
  (declare (indent 1))
  `(let ((syntax-table (syntax-table))
         (< (string (char-syntax ?<)))
         (> (string (char-syntax ?>))))
     (modify-syntax-entry ?< "(" syntax-table)
     (modify-syntax-entry ?> ")" syntax-table)
     ,@body
     (modify-syntax-entry ?< < syntax-table)
     (modify-syntax-entry ?> > syntax-table)))

;;; syntax highlighting
(font-lock-add-keywords
 'lisp-mode
 ;; keyword tag names
 '(("</?\\(:[^>/=[:space:]]+\\)" 1 font-lock-builtin-face)
   ;; regular tag names
   ("</?\\([^>/=[:space:]]*\\)" 1 font-lock-function-name-face)
   ;; attribute names
   ("[[:space:]]\\([-[:alpha:]]+\\)=" 1 font-lock-constant-face)
   ;; deftag faces
   ("deftag" . font-lock-keyword-face)
   ("deftag \\([^ ]+\\) " 1 font-lock-function-name-face)
   ;; warning about single symbol lisp forms at the end of tags
   ("=[^[:space:]<>]+[^\"/) ]\\(/\\|>\\)" 1 font-lock-warning-face))
 'prepend)

(font-lock-add-keywords
 'lisp-mode
 '(("&[^ ]+;" . font-lock-builtin-face))
 'prepend)

;;; context
(defun in-html-p ()
  (save-excursion
    (cl-loop
     with point = (point)
     for start = (progn (goto-char (or start point))
                        (when start (forward-char -1))
                        (html-start-point))
     while (/= start (point-max))
     for end = (html-end-point)
     if (<= start point end)
     return 
     (progn
       (not (cl-loop
             for lisp-start = (progn (goto-char (or lisp-start point))
                                (when lisp-start (forward-char -1))
                                (lisp-start-point))
             while (/= lisp-start (point-max))
             for lisp-end = (lisp-end-point)
             if (<= start lisp-start (+ point 1) lisp-end end)
             return t))))))
(defun lisp-start-point ()
  (condition-case nil
      (search-backward-regexp ",(\\|,@\\|=(")
    (t (point-max))))
(defun lisp-end-point ()
  (condition-case nil
      (progn (skip-chars-forward "=,@")
             (forward-sexp)
             (point))
    (t (point-max))))
(defun html-start-point ()
  (let ((html-start-re "<[^/=[:space:]()]"))
    (condition-case nil
        (if (looking-at-p html-start-re)
            (point)
          (let ((spot (cl-loop
                       while (search-backward "<")
                       if (looking-at-p html-start-re)
                       return (point))))
            (if spot spot (point-max))))
      (t (point-max)))))
(defun html-end-point ()
  (let ((tag-name (save-excursion
                    (buffer-substring-no-properties
                     (+ (point) 1) (- (search-forward-regexp "[>/[:space:]]") 1)))))
    (with-<>-as-brackets
      (forward-sexp 1)
      (if (looking-back "/>" 1)
          (point)
          (condition-case nil
              (progn
                (search-forward (concat "</" tag-name ">"))
                (point))
            (t (point-max)))))
    (point)))

;;; indentation
(defun lisp-html-indent-line ()
  "Indent a line of lisp or html."
  (interactive)
  (save-excursion
    (back-to-indentation)
         ;; closing tag
         ((looking-at "</")
          (let* ((tag-name (save-excursion
                             (buffer-substring-no-properties
                              (+ (point) 2)
                              (progn (search-forward ">")
                                     (- (point) 1)))))
                 (indent
                  (save-excursion
                    (search-backward-regexp (concat "<" tag-name "[ />]"))
                    (- (point) (progn (beginning-of-line) (point))))))
            ;; (message "closing")
            (indent-line-to (max 0 indent))))
    (let ((prev-html (save-excursion
                       (forward-line -1)
                       (end-of-line)
                       (in-html-p)))
          (beg-line-html (save-excursion
                           (beginning-of-line)
                           (in-html-p)))
          (curr-html (in-html-p)))
      (cond
       ;; after closing tag and end of lisp form
       ((and prev-html
             (save-excursion
               (forward-line -1)
               (end-of-line)
               (skip-chars-backward "\t\r\n ")
               (and (= (char-before) 41)
                    (progn
                      (forward-sexp -1)
                      (skip-chars-backward ",@")
                      (= (char-after) ?,)))))
        (indent-line-to
         (save-excursion
           (forward-sexp -1)
           (current-indentation))))
       ;;
       ((and prev-html
             (save-excursion
               (forward-line -1)
               (back-to-indentation)
               (looking-at "</")))
        (indent-line-to
         (save-excursion
           (forward-line -1)
           (back-to-indentation)
           (- (point) (progn (beginning-of-line) (point))))))
       ;; sgml indent
       (prev-html
        ;; (message "html")
        (with-<>-as-brackets
            (sgml-indent-line)))
       ;; lisp indent
       (:else
        (let ((indent (calculate-lisp-indent)))
          ;; (message "lisp")
          (cond
           ((and indent (listp indent)) (indent-line-to (car indent)))
           (indent (indent-line-to indent))
           (:else (special-lisp-html-indent-line))))))))
  (when (< (point) (save-excursion (back-to-indentation) (point)))
    (back-to-indentation)))
(defun lisp-html-indent-region (beg end)
  "Indent a region of Lisp and Html mixed together.
Just calls `lisp-html-indent-line' on every line of the region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((last-line (line-number-at-pos end)))
      (while (and (<= (line-number-at-pos (point)) last-line)
                  (<= (line-number-at-pos (point)) (line-number-at-pos (- (point-max) 1))))
        (lisp-html-indent-line)
        (forward-line 1)))))
;; set indentation functions in hooks
(defun set-lisp-html-indentation ()
  (setq indent-line-function #'lisp-html-indent-line)
  (setq indent-region-function #'lisp-html-indent-region))
(add-hook 'lisp-mode-hook #'set-lisp-html-indentation)
;; define enter to indent
(define-key lisp-mode-map (kbd "<return>") #'newline-and-indent)

;;; neato stuff
(defun html-/-close-tag ()
  (interactive)
  (insert "/")
  (when (= ?< (char-before (1- (point))))
    (backward-delete-char 2)
    (with-<>-as-brackets
        (sgml-close-tag))
    (when (= ?> (char-after))
      (delete-char 1))))
(define-key lisp-mode-map (kbd "/") #'html-/-close-tag)
(define-key lisp-mode-map (kbd "C-c C-o") #'sgml-tag)
(add-hook 'lisp-mode-hook #'sgml-electric-tag-pair-mode)

(provide 'lisp-markup)
;;; lisp-markup.el ends here
