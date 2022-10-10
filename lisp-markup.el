;;;; lisp-markup.el
;;;; Charles Jackson
(require 'cl-lib)
(require 'sgml-mode)
(require 'lisp-mode)

(defvar lisp-markup-minor-mode-map
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "/") #'lisp-markup-/-close-tag)
    (define-key keymap (kbd "C-c C-o") #'sgml-tag)
    (define-key keymap (kbd "<return>") #'newline-and-indent)
    keymap)
  "Additional key bindings for `lisp-markup-minor-mode'.")

(defvar lisp-markup-sgml-tag-syntax-table
  (let ((table (make-syntax-table sgml-tag-syntax-table)))
    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry 40 "|" table)
    (modify-syntax-entry 41 "|" table)
    table)
  "A modified `sgml-tag-syntax-table' that effectively ignores
content between ?( and ?) by mapping them to symbol-escape
characters. Additionally maps ?' to be a punctuation character
which separates symbols.")

(defvar *lisp-markup-mode-keywords*
  '(("</?\\(:[^>/=[:space:]]+\\)" 1 font-lock-builtin-face)
    ;; regular tag names
    ("</?\\([^!>/=[:space:]]*\\)" 1 font-lock-function-name-face)
    ;; attribute names
    ("[[:space:]]\\([-[:alpha:]]+\\)=" 1 font-lock-constant-face)
    ;; deftag faces
    ("(\\(deftag\\)" 1 font-lock-keyword-face)
    ("(deftag \\([^ ]+\\) " 1 font-lock-function-name-face)
    ;; warning about single symbol lisp forms at the end of tags
    ("=[^[:space:]<>]+[^\"/) ]\\(/\\|>\\)" 1 font-lock-warning-face)
    ;; html comments
    ("<!--.*-->" . font-lock-comment-face))
  "`font-lock' configuration for `lisp-markup-minor-mode' to
provide highlighting to HTML code within lisp files.")

(define-minor-mode lisp-markup-minor-mode
  "Enhance `lisp-mode' with additional features to support embedded HTML through markup.

This changes syntax highlighting, indentation rules, and adds
some extra keybindings to make editing of markup in lisp files
easier."
  :lighter " markup"
  :keymap lisp-markup-minor-mode-map
  (if (eq major-mode 'lisp-mode)
      (if lisp-markup-minor-mode
          (enter-lisp-markup-minor-mode)
        (exit-lisp-markup-minor-mode))
    (progn
      (setf lisp-markup-minor-mode nil)
      (error "lisp-markup-minor-mode only supports running in lisp-mode"))))

(defun enter-lisp-markup-minor-mode ()
  "Perform the setup required by `lisp-markup-minor-mode'."
  (set-syntax-table (make-syntax-table (syntax-table)))
  (modify-syntax-entry ?' ".")
  (font-lock-add-keywords nil *lisp-markup-mode-keywords*)
  (font-lock-update)
  (setq-local indent-line-function #'lisp-markup-indent-line)
  (setq-local indent-region-function #'lisp-markup-indent-region)
  (sgml-electric-tag-pair-mode 1))

(defun exit-lisp-markup-minor-mode ()
  "Undo the setup performed by `enter-lisp-markup-minor-mode'."
  (set-syntax-table lisp-mode-syntax-table)
  (font-lock-remove-keywords nil *lisp-markup-mode-keywords*)
  (font-lock-update)
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local indent-region-function #'lisp-indent-region)
  (sgml-electric-tag-pair-mode -1))

(defmacro lisp-markup-with-<>-as-brackets (&rest body)
  "Run BODY in a context where ?< and ?> behave as brackets, and ?(
and ?) behave as string delimiters. This is useful to run SGML
functions on code that contains both Lisp and HTML."
  (declare (indent 0))
  `(with-syntax-table (make-syntax-table (syntax-table))
     (modify-syntax-entry ?< "(")
     (modify-syntax-entry ?> ")")
     (modify-syntax-entry 40 "\"")
     (modify-syntax-entry 41 "\"")
     (progn ,@body)))

(defmacro lisp-markup-with-sgml-tag-table (&rest body)
  "Run BODY in a context where `sgml-tag-syntax-table' is resolved
to be our custom syntax table. This allows us to run SGML
functions which internally change the syntax table without them
getting confused by Lisp code.."
  `(let ((sgml-tag-syntax-table lisp-markup-sgml-tag-syntax-table))
     ,@body))

;;; Determining context
;;; ===================

(defun lisp-markup-in-html-p ()
  "Is point currently in an HTML context?"
  (save-excursion
    (cl-loop
     with point = (point)
     for start = (progn (goto-char (or start point))
                        (when start (forward-char -1))
                        (lisp-markup-html-start-point))
     while (/= start (point-min))
     for end = (lisp-markup-html-end-point)
     if (<= start point end)
     return
     (progn
       (not (cl-loop
             for lisp-start = (progn (goto-char (or lisp-start point))
                                     (when lisp-start (forward-char -1))
                                     (lisp-markup-lisp-start-point))
             while (/= lisp-start (point-min))
             for lisp-end = (lisp-markup-lisp-end-point)
             if (<= start lisp-start (+ point 1) lisp-end end)
             return t))))))

(defun lisp-markup-lisp-start-point ()
  "Move backwards through the buffer, looking for the start of a
Lisp code section. If one is found, move point there and return
point. If none is found, return the minimum point."
  (condition-case nil
      (search-backward-regexp ",(\\|,@\\|=(")
    (t (point-min))))

(defun lisp-markup-lisp-end-point ()
  "Move to the closest Lisp end point, moving forwards from the
current point. This function should only be called if point is at
the beginning of a Lisp code section."
  (condition-case nil
      (progn (skip-chars-forward "=,@")
             (forward-sexp)
             (point))
    (t (point-max))))

(defun lisp-markup-html-start-point ()
  "Move backwards through the buffer, looking for the start of a
HTML code section. If one is found, move point there and return
point. If none is found, return the minimum point."
  (let ((html-start-re "<[^/=[:space:]()]"))
    (condition-case nil
        (if (looking-at-p html-start-re)
            (point)
          (let ((spot (cl-loop
                       while (search-backward "<")
                       if (looking-at-p html-start-re)
                       return (point))))
            (if spot spot (point-max))))
      (t (point-min)))))

(defun lisp-markup-html-end-point ()
  "Move to the closest HTML end point, moving forwards from the
current point. This function should only be called if point is at
the beginning of a HTML code section."
  (save-excursion
    (lisp-markup-with-sgml-tag-table
     (sgml-skip-tag-forward 1))
    (point)))

;;; Indentation
;;; ===========

(defun lisp-markup-indent-line ()
  "Indent a line of Lisp or HTML, according to the line's context."
  (interactive)
  (save-excursion
    (lisp-markup-with-sgml-tag-table
     (with-syntax-table (if (>= emacs-major-version 28)
                            lisp-mode-syntax-table
                          lisp--mode-syntax-table)
       (back-to-indentation)
       (let ((prev-html (save-excursion
                          (forward-line -1)
                          (end-of-line)
                          (lisp-markup-in-html-p))))
         (cond
          ;; closing tag
          ((looking-at "</")
           (let* ((indent
                   (save-excursion
                     (forward-sexp 1)
                     (sgml-skip-tag-backward 1)
                     (- (point) (progn (beginning-of-line) (point))))))
             (indent-line-to (max 0 indent))))
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
           (lisp-markup-with-<>-as-brackets
             (sgml-indent-line)))
          ;; lisp indent
          (:else
           (let ((indent (calculate-lisp-indent)))
             (cond
              ((and indent (listp indent)) (indent-line-to (car indent)))
              (indent (indent-line-to indent))))))))))
  (when (< (point) (save-excursion (back-to-indentation) (point)))
    (back-to-indentation)))

(defun lisp-markup-indent-region (beg end)
  "Indent the region of Lisp and HTML between BEG and END.

This function just calls `lisp-markup-indent-line' on every line
of the region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((last-line (line-number-at-pos end)))
      (cl-loop
       do (lisp-markup-indent-line)
       do (forward-line 1)
       while (and (<= (line-number-at-pos (point)) last-line)
                  (<= (line-number-at-pos (point)) (line-number-at-pos (- (point-max) 1))))))))

;;; Automatic tag closing
;;; =====================

(defun lisp-markup-html-closed-p ()
  "Test whether the current HTML tag has a corresponding closing tag.

This method must be called with point before the opening < of a tag."
  (save-excursion
    (lisp-markup-with-sgml-tag-table
     (when (sgml-skip-tag-forward 1)
       (point)))))

(defun lisp-markup-html-close-tag ()
  "Insert a closing tag for the nearest tag before point that is unclosed.

This function only looks backwards to find unclosed tags, and
thus a tag that is closed further forwards in the file will not
be considered. Hence in an example like this:

  <div>
    <span></span>
    |
  </div>

with point at |, a </div> will be inserted."
  (interactive)
  (insert "</"
          (save-excursion
            (cl-loop with initial = (point)
                     while (/= (point-max) (lisp-markup-html-start-point))
                     for close = (lisp-markup-html-closed-p)
                     if (or (not close)
                            (<= initial close))
                     return (buffer-substring-no-properties
                             (+ (point) 1)
                             (- (search-forward-regexp "[>/[:space:]]") 1))
                     do (forward-char -1)))
          ">"))

(defun lisp-markup-/-close-tag ()
  "Automatically insert a closing tag if this character was typed
after a <. Otherwise, just insert a /."
  (interactive)
  (insert "/")
  (when (= ?< (char-before (1- (point))))
    (backward-delete-char 2)
    (lisp-markup-html-close-tag)
    (when (= ?> (or (char-after) 0))
      (delete-char 1))
    (lisp-markup-indent-line)))

(provide 'lisp-markup)
;;; lisp-markup.el ends here
