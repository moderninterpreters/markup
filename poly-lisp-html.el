;;; poly-lisp-html.el
;;; Charles Jackson
(require 'sgml-mode)
(require 'lisp-mode)
(require 'polymode)

(defconst html-mm 'html-mode)       ;in case you want to use mhtml-mode

(defclass pm-lisp-host-chunkmode (pm-host-chunkmode) () "Host chunkmode for lisp.")
(defvar poly-lisp-hostmode
  (pm-lisp-host-chunkmode
   :mode 'lisp-mode))
(defmethod pm-indent-line ((_chunkmode pm-lisp-host-chunkmode) span)
  (lisp-html-indent-line))

(defun html-head-matcher (direction)
  (let ((forward (= direction 1)))
    (if (funcall (if forward #'re-search-forward #'re-search-backward)
         "<[^=[:space:]]" nil t)
        (cons (match-beginning 0) (match-beginning 0)))))
(defun html-tail-matcher (_)
  (sgml-skip-tag-forward 1)
  (let ((end-whitespace (save-excursion
               (while (and (< (point) (point-max))
                           (or (char-equal (char-after) 32)
                               (char-equal (char-after) ?\t)
                               (char-equal (char-after) ?\r)
                               (char-equal (char-after) ?\n)))
                 (forward-char 1))
               (point))))
  (cons (point) end-whitespace)))
(defclass pm-html-inner-chunkmode (pm-inner-chunkmode) ()
  "Cunkmode for html in something else.")
(defvar poly-html-innermode
   (pm-html-inner-chunkmode
   :name "poly-html-innermode"
   :mode html-mm
   :can-nest t
   :allow-nested t
   :protect-indent nil
   :adjust-face nil
   :head-adjust-face nil
   :tail-adjust-face nil
   :head-matcher #'html-head-matcher
   :tail-matcher #'html-tail-matcher
   :head-mode 'body
   :tail-mode 'body))  
(defmethod pm-indent-line ((_chunkmode pm-html-inner-chunkmode) span)
  (lisp-html-indent-line))

(defun lisp-head-matcher (direction)
  (let ((forward (= direction 1)))
    (if (funcall (if forward #'re-search-forward #'re-search-backward)
         "\\(,\\|,@\\|=\\)(" nil t)
        (cons (match-beginning 1) (match-beginning 1)))))
(defun lisp-tail-matcher (_)
  (with-syntax-table lisp-mode-syntax-table
    (forward-char)
    (forward-sexp)
    (cons (point) (point))))
(defclass pm-lisp-inner-chunkmode (pm-inner-chunkmode) ()
  "Cunkmode for lisp inside something else, started by , ,@ or =.")
(defvar poly-lisp-innermode
  (pm-lisp-inner-chunkmode
   :name "poly-lisp-innermode"
   :mode 'lisp-mode
   :can-nest t
   :allow-nested t
   :protect-indent nil
   :adjust-face nil
   :head-adjust-face nil
   :tail-adjust-face nil
   :head-matcher #'lisp-head-matcher
   :tail-matcher #'lisp-tail-matcher
   :head-mode 'body
   :tail-mode 'body))
(defmethod pm-indent-line ((_chunkmode pm-lisp-inner-chunkmode) span)
  (lisp-html-indent-line))

(define-polymode poly-lisp-html-mode
  :hostmode 'poly-lisp-hostmode
  :innermodes '(poly-html-innermode poly-lisp-innermode))

(defun lisp-html-indent-line ()
  "Indent a line of lisp or html."
  (interactive)
  (save-excursion
    (widen)
    (back-to-indentation)
    (let ((prev-mm (save-excursion
                     (forward-line -1)
                     (end-of-line)
                     (pm-span-mode)))
          (curr-mm (pm-span-mode))
          (closing-p (looking-at "</")))
      (cond
       ;; sgml indent
       ((or (and (equal prev-mm html-mm)
                 (equal curr-mm html-mm))
            (and (equal prev-mm 'lisp-mode)
                 closing-p))
        (with-syntax-table html-mode-syntax-table
          (sgml-indent-line)))
       ;; lisp indent
       ((or (and (equal prev-mm 'lisp-mode)
                 (equal curr-mm html-mm))
            (equal curr-mm 'lisp-mode))
        (with-syntax-table lisp-mode-syntax-table
          (pm-set-buffer (if (= 1 (point)) 1 (- (point) 1)))
          (lisp-indent-line))))))
  (when (< (point) (save-excursion (back-to-indentation) (point)))
    (back-to-indentation)))

(provide 'poly-lisp-html)
;;; poly-lisp-html.el ends here
