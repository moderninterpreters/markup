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
  "Check if point is currently in an HTML context."
  (let ((html (lisp-markup-enclosing-html-tag)))
    (when html
      (let ((lisp (lisp-markup-enclosing-lisp-section)))
        (and (<= (car lisp) (car html))
             (<= (cdr html) (cdr lisp)))))))

(defun lisp-markup-find-enclosing (find-start goto-end not-found)
  "Find the nearest enclosing \"section\" defined by FIND-START and GOTO-END.

This function looks backwards in the buffer to find the start of
the nearest section by calling FIND-START. This function defines
what the start of a section is by moving point to be before the
first character of a section. This will often involve a call to
`search-backward-regexp' or similar. If FIND-START throws an
error the search will end and NOT-FOUND will be returned.

Once the start of a section has been found, GOTO-END will be
called to move point to the end of this section. If GOTO-END
throws an error, `point-max' will be used as the end value.

Returns a pair of beginning and end points, or NOT-FOUND."
  (save-excursion
    (catch 'return
      (let ((initial (point)))
        (while t
          (let ((start (or (ignore-errors
                             (funcall find-start)
                             (point))
                           (throw 'return not-found)))
                (end (or (ignore-errors
                           (funcall goto-end)
                           (point))
                         (throw 'return (cons start (point-max))))))
            (when (<= start initial end)
              (throw 'return (cons start end)))
            ;; Reset for the next iteration
            (goto-char start)))))))

(defun lisp-markup-enclosing-lisp-section ()
  "Find the nearest enclosing Lisp section.

This function looks backwards in the buffer to find the start of
the nearest Lisp section, then looks forwards to find its end. If
no start/end is found, returns the values of `point-min' and
`point-max' as the beginning and end, respectively.

Returns a pair of beginning and end points."
  (lisp-markup-find-enclosing
   (lambda ()
     (search-backward-regexp ",(\\|,@\\|=("))
   (lambda ()
     (skip-chars-forward "=,@")
     (forward-sexp))
   (cons (point-min) (point-max))))

(defun lisp-markup-enclosing-html-tag ()
  "Find the nearest enclosing HTML tag.

This function looks backwards in the buffer to find the start of
the nearest HTML section, then looks forwards to find its end.

Returns a pair of beginning and end points. If no start is found,
returns nil."
  (lisp-markup-find-enclosing
   (lambda ()
     (search-backward-regexp "<[^/=[:space:]()]"))
   (lambda ()
     (lisp-markup-with-sgml-tag-table
      (sgml-skip-tag-forward 1)))
   nil))

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
