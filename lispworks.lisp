;;;; lispworks.lisp -- a LispWorks editor mode for markup
;;;;
;;;; This is a port of lisp-markup.el to the LispWorks IDE editor. It
;;;; provides a "Markup" minor mode that layers HTML awareness on top of
;;;; LispWorks' own Lisp mode, in the same way the Emacs mode layers on
;;;; top of lisp-mode.
;;;;
;;;; The LispWorks editor has no SGML/HTML mode to delegate to (unlike
;;;; Emacs, where lisp-markup.el leans on sgml-mode), so the tag-level
;;;; machinery is implemented here from scratch.

(defpackage :markup/lispworks
  (:use #:cl)
  (:export #:enable))

(in-package :markup/lispworks)

;;; Faces
;;; =====
;;;
;;; LispWorks' own font-lock faces live in internal EDITOR specials. We
;;; reuse them where the Emacs mode uses the equivalent Emacs face, and
;;; define our own for the two that LispWorks has no counterpart for
;;; (font-lock-constant-face and font-lock-warning-face).

(defun switchable-color (light dark)
  (editor::create-dark-background-switchable-color light dark))

(defvar *tag-face* editor::*font-lock-function-name-face*
  "Face for ordinary tag names, e.g. the \"div\" in <div>.")

(defvar *keyword-tag-face* editor::*font-lock-builtin-face*
  "Face for keyword tag names, e.g. the \":my-tag\" in <:my-tag>.")

(defvar *comment-face* editor::*font-lock-comment-face*
  "Face for <!-- HTML comments -->.")

(defvar *attribute-face*
  (editor:make-face 'markup-attribute-face
                    :foreground (switchable-color :darkgoldenrod :lightgoldenrod)
                    :if-exists :overwrite)
  "Face for attribute names, e.g. the \"class\" in <div class=\"x\">.")

(defvar *warning-face*
  (editor:make-face 'markup-warning-face
                    :foreground (switchable-color :red :orange)
                    :underline-p t
                    :if-exists :overwrite)
  "Face for the closing > or /> of a tag whose last attribute value is an
unquoted symbol. Markup's reader swallows the > or / into the symbol, so
this is nearly always a bug.")

;;; Scanning
;;; ========
;;;
;;; The scanner runs over a plain string rather than over buffer points:
;;; it is far easier to get right, and font-lock only ever hands us a
;;; region of a few hundred characters. It returns a list of
;;; (FROM TO FACE) triples with offsets relative to the start of the
;;; string, in increasing order of FROM.

(defparameter *whitespace* '(#\Space #\Tab #\Newline #\Return #\Page))

(defun whitespacep (ch)
  (member ch *whitespace*))

(defun tag-name-char-p (ch)
  "Characters that may appear in a tag name. Mirrors the Emacs mode's
[^!>/=[:space:]] character class, with < and \" excluded too so that
stray punctuation cannot start a tag."
  (and ch
       (not (whitespacep ch))
       (not (member ch '(#\> #\/ #\= #\< #\! #\")))))

(defun attribute-name-char-p (ch)
  (and ch (or (alpha-char-p ch) (char= ch #\-))))

(defun skip-lisp-string (text i len)
  "I is the index of the opening double quote. Returns the index just
after the closing quote."
  (incf i)
  (loop while (< i len)
        do (let ((ch (char text i)))
             (cond
               ((char= ch #\\) (incf i 2))
               ((char= ch #\") (return-from skip-lisp-string (1+ i)))
               (t (incf i)))))
  len)

(defun skip-lisp-form (text i len)
  "I is the index of an opening paren. Returns the index just after the
matching close paren, skipping over nested parens and strings."
  (let ((depth 0))
    (loop while (< i len)
          do (let ((ch (char text i)))
               (cond
                 ((char= ch #\") (setf i (skip-lisp-string text i len)))
                 ((char= ch #\\) (incf i 2))
                 ((char= ch #\() (incf depth) (incf i))
                 ((char= ch #\))
                  (incf i)
                  (when (zerop (decf depth))
                    (return-from skip-lisp-form i)))
                 (t (incf i)))))
    len))

(defun scan-attribute-value (text i len acc)
  "Scan the value following an = sign. Returns the index just past it,
pushing a warning highlight onto ACC if the value is an unquoted symbol
butting up against the end of the tag."
  (loop while (and (< i len) (whitespacep (char text i)))
        do (incf i))
  (when (>= i len)
    (return-from scan-attribute-value (values len acc)))
  (let ((ch (char text i)))
    (cond
      ((char= ch #\") (values (skip-lisp-string text i len) acc))
      ((char= ch #\() (values (skip-lisp-form text i len) acc))
      ((member ch '(#\, #\@ #\' #\`))
       ;; ,(foo) / ,@(foo) / '(foo) -- skip the prefix and recurse.
       (scan-attribute-value text (1+ i) len acc))
      (t
       ;; A bare token. Read to the end of it, then check whether it runs
       ;; straight into the tag terminator.
       (let ((start i))
         (loop while (and (< i len)
                          (not (whitespacep (char text i)))
                          (not (member (char text i) '(#\> #\/))))
               do (incf i))
         ;; Leave the terminator itself unconsumed so that SCAN-ATTRIBUTES
         ;; still sees the > that ends the tag.
         (if (and (> i start)
                  (< i len)
                  (member (char text i) '(#\> #\/)))
             (values i (cons (list i (1+ i) *warning-face*) acc))
             (values i acc)))))))

(defun scan-attributes (text i len acc)
  "Scan from just past a tag name to the end of the tag. Returns the
index just past the closing > and the accumulated highlights."
  (loop
    (when (>= i len)
      (return (values len acc)))
    (let ((ch (char text i)))
      (cond
        ((char= ch #\>) (return (values (1+ i) acc)))
        ((char= ch #\") (setf i (skip-lisp-string text i len)))
        ((char= ch #\() (setf i (skip-lisp-form text i len)))
        ((attribute-name-char-p ch)
         (let ((start i))
           (loop while (and (< i len) (attribute-name-char-p (char text i)))
                 do (incf i))
           (cond
             ((and (< i len) (char= (char text i) #\=))
              (push (list start i *attribute-face*) acc)
              (multiple-value-setq (i acc)
                (scan-attribute-value text (1+ i) len acc)))
             (t
              ;; A bare boolean attribute; the Emacs mode leaves these
              ;; unfontified, so we do too.
              nil))))
        (t (incf i))))))

(defstruct (tag (:constructor %make-tag (kind name start name-start end attrs-start)))
  "One <...> construct in the source. KIND is :OPEN, :CLOSE, :SELF (a tag
closed with />), :COMMENT (<!-- ... -->) or :DECL (<!DOCTYPE ...>).
START is the offset of the #\< and END the offset just past the #\>."
  kind
  name
  start
  name-start
  end
  attrs-start)

(defun scan-to-tag-end (text i len)
  "I is the offset just past a tag name. Returns the offset just past the
closing > and a second value that is true when the tag closed with />.
Strings and parenthesised Lisp forms are skipped, so a > inside either
does not end the tag."
  (let ((last-significant nil))
    (loop
      (when (>= i len)
        (return (values len nil)))
      (let ((ch (char text i)))
        (cond
          ((char= ch #\>)
           (return (values (1+ i)
                           (and last-significant
                                (char= (char text last-significant) #\/)))))
          ((char= ch #\")
           (setf i (skip-lisp-string text i len)
                 last-significant (1- i)))
          ((char= ch #\()
           (setf i (skip-lisp-form text i len)
                 last-significant (1- i)))
          ((whitespacep ch) (incf i))
          (t (setf last-significant i)
             (incf i)))))))

(defun tag-at (text j len)
  "If a tag begins at offset J -- which must hold a #\< -- return a TAG
describing it. Otherwise NIL, which is what a less-than operator such as
the one in (< a b) gets."
  (cond
    ((and (<= (+ j 4) len)
          (string= "!--" text :start2 (1+ j) :end2 (+ j 4)))
     (let* ((close (search "-->" text :start2 (+ j 4)))
            (end (if close (+ close 3) len)))
       (%make-tag :comment nil j nil end nil)))
    ((and (< (1+ j) len) (char= (char text (1+ j)) #\!))
     (let ((close (position #\> text :start (1+ j) :end len)))
       (%make-tag :decl nil j nil (if close (1+ close) len) nil)))
    (t
     (let ((k (1+ j))
           (closingp nil))
       (when (and (< k len) (char= (char text k) #\/))
         (setf closingp t)
         (incf k))
       (let ((name-start k))
         (loop while (and (< k len) (tag-name-char-p (char text k)))
               do (incf k))
         (when (> k name-start)
           (multiple-value-bind (end selfp) (scan-to-tag-end text k len)
             (%make-tag (cond (closingp :close)
                              (selfp :self)
                              (t :open))
                        (subseq text name-start k)
                        j name-start end k))))))))

(defun tokenize (text)
  "Return a vector of every TAG in TEXT, in order. A < that does not
begin a tag is skipped, and a < inside an HTML comment or inside a tag's
attributes never appears as a token of its own."
  (let ((len (length text))
        (tokens (make-array 0 :adjustable t :fill-pointer t))
        (i 0))
    (loop
      (let ((j (position #\< text :start i :end len)))
        (unless j (return))
        (let ((tag (tag-at text j len)))
          (cond
            (tag (vector-push-extend tag tokens)
                 (setf i (max (tag-end tag) (1+ j))))
            (t (setf i (1+ j)))))))
    tokens))

(defun tag-name-face (tag)
  (if (char= (char (tag-name tag) 0) #\:)
      *keyword-tag-face*
      *tag-face*))

(defun scan-deftags (text len acc)
  "Fontify (deftag name ...) forms, which the Emacs mode highlights
specially. LispWorks only fontifies the name of a def-form when it is at
top level, so this also covers nested ones."
  (let ((i 0))
    (loop
      (let ((j (search "(deftag" text :start2 i)))
        (unless j (return acc))
        (let ((after (+ j 7)))
          (cond
            ((and (< after len) (whitespacep (char text after)))
             (push (list (1+ j) after editor::*font-lock-keyword-face*) acc)
             (let ((s after))
               (loop while (and (< s len) (whitespacep (char text s)))
                     do (incf s))
               (let ((e s))
                 (loop while (and (< e len)
                                  (not (whitespacep (char text e)))
                                  (not (member (char text e) '(#\( #\)))))
                       do (incf e))
                 (when (> e s)
                   (push (list s e editor::*font-lock-function-name-face*) acc))
                 (setf i e))))
            (t (setf i after))))))))

(defun scan (text)
  "Return the list of (FROM TO FACE) highlights for TEXT, ordered by FROM."
  (let ((len (length text))
        (acc nil))
    (loop for tok across (tokenize text)
          do (case (tag-kind tok)
               (:comment (push (list (tag-start tok) (tag-end tok) *comment-face*) acc))
               (:decl nil)
               (t
                (let ((name-start (tag-name-start tok)))
                  (push (list name-start
                              (+ name-start (length (tag-name tok)))
                              (tag-name-face tok))
                        acc))
                (unless (eq (tag-kind tok) :close)
                  (setf acc (nth-value 1 (scan-attributes text
                                                          (tag-attrs-start tok)
                                                          (tag-end tok)
                                                          acc)))))))
    (setf acc (scan-deftags text len acc))
    (stable-sort (nreverse acc) #'< :key #'first)))

;;; Painting
;;; ========

(defun scan-region-start (start)
  "The point font-lock should really start scanning from: the start of
the enclosing top level form, so that a tag split across several lines
is still recognised when only one of those lines is refontified."
  (editor:with-point ((p start :temporary))
    (editor:line-start p)
    (if (editor::top-level-offset p -1)
        (editor:copy-point p :temporary)
        (editor:copy-point start :temporary))))

(defun apply-highlights (base highlights min-offset)
  "Paint HIGHLIGHTS, whose offsets are relative to BASE. Highlights that
end at or before MIN-OFFSET are skipped: font-lock only cleared the faces
from the region it asked us about, so painting before it could leave a
stale face behind."
  (editor:with-point ((p base :temporary)
                      (q base :temporary))
    (let ((pos 0))
      (dolist (highlight highlights)
        (destructuring-bind (from to face) highlight
          (when (> to min-offset)
            (when (editor:character-offset p (- from pos))
              (setf pos from)
              (editor:move-point q p)
              (when (editor:character-offset q (- to from))
                (editor::font-lock-apply-highlight p q face)))))))))

(defun fontify-keywords-region (start end)
  "The Markup mode's font-lock keyword pass. Paints the markup-specific
highlights first so they win over the Lisp ones on the rare overlap, then
delegates to LispWorks' normal Lisp keyword fontifier so that keywords,
lambda-list markers and def-forms still get their usual faces."
  (let ((buffer (editor:point-buffer start)))
    (editor:with-buffer-locked (buffer :for-modification nil
                                       :check-file-modification nil)
      (let* ((base (scan-region-start start))
             (min-offset (editor::count-characters base start))
             (text (editor:points-to-string base end)))
        (apply-highlights base (scan text) min-offset))
      (editor::lisp-font-lock-fontify-keywords-region start end))))

;;; The mode
;;; ========

(editor:defmode "Markup"
                :vars `((editor::font-lock-fontify-keywords-region-function
                         . fontify-keywords-region)))

(defun refontify (buffer)
  (when (editor::buffer-font-lock-mode-p buffer)
    (editor::font-lock-fontify-buffer buffer)))

(editor:defcommand "Markup Mode" (p)
     "Toggle Markup minor mode in the current buffer. With a positive
prefix argument turn it on, with a negative one turn it off."
     "Toggle Markup minor mode in the current buffer."
  (let* ((buffer (editor:current-buffer))
         (on (if p (plusp p) (not (editor:buffer-minor-mode buffer "Markup")))))
    (setf (editor:buffer-minor-mode buffer "Markup") on)
    (refontify buffer)
    (editor:message "Markup mode ~:[disabled~;enabled~]" on)))

(defun enable ()
  "Entry point for a LispWorks init file. Currently a no-op beyond
loading this file, which defines the \"Markup\" mode and the
\"Markup Mode\" command."
  (values))
