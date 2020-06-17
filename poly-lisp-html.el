;;; poly-lisp-html.el
;;; Charles Jackson
(require 'sgml-mode)
(require 'lisp-mode)
(require 'polymode)

(defconst html-mm 'html-mode)       ;in case you want to use mhtml-mode

(defclass pm-lisp-host-chunkmode (pm-host-chunkmode) () "Chunkmode for lisp.")
(defvar poly-lisp-hostmode
  (pm-lisp-host-chunkmode
   :mode 'lisp-mode))
(defmethod pm-indent-line ((_chunkmode pm-lisp-host-chunkmode) span)
  (lisp-html-indent-line))

(defclass pm-lisp-html-inner-chunkmode (pm-inner-chunkmode) ()
  "Cunkmode for html inside lisp.")
(defvar poly-lisp-html-innermode
  (pm-lisp-html-inner-chunkmode
   :name "poly-lisp-html-innermode"
   :mode html-mm
   :protect-indent nil
   :adjust-face nil
   :head-adjust-face nil
   :tail-adjust-face nil
   :head-matcher (cons "\\(?1:<[^|=<> \t\r\n]+\\)\\|[) \t\r\n]/?\\(?1:>\\)" 1)
   :tail-matcher (cons "\\(?1:>[ \t\r\n]*\\))\\|\\(?1:,\\|,@\\|=\\)(" 1)
   :head-mode 'body
   :tail-mode 'body))
(defmethod pm-indent-line ((_chunkmode pm-lisp-html-inner-chunkmode) span)
  (lisp-html-indent-line))

(define-polymode poly-lisp-html-mode
  :hostmode 'poly-lisp-hostmode
  :innermodes '(poly-lisp-html-innermode))

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
