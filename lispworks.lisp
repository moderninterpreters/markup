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
  (:import-from #:markup/tags
                #:*void-tags*)
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

(defun void-tag-name-p (name)
  "True for the HTML elements that markup's reader closes implicitly, so
that the editor agrees with MARKUP::VOID-TAG?."
  (and name (member name *void-tags* :test #'string-equal)))

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

;;; Navigating elements
;;; ===================
;;;
;;; These are the counterparts of sgml-skip-tag-forward and friends, which
;;; the Emacs mode gets from sgml-mode. They all work on the token vector
;;; rather than on buffer points, which keeps them pure and testable.

(defun skip-element-forward (tokens index)
  "TOKENS[INDEX] opens an element. Return the offset just past the end of
that element -- past its closing tag when it has one -- and the index of
the token after it. Returns NIL when the element is never closed."
  (let* ((open (aref tokens index))
         (name (tag-name open)))
    (case (tag-kind open)
      ((:self :comment :decl) (values (tag-end open) (1+ index)))
      (:close nil)
      (t
       (if (void-tag-name-p name)
           (values (tag-end open) (1+ index))
           (let ((depth 1))
             (loop for i from (1+ index) below (length tokens)
                   for tok = (aref tokens i)
                   do (case (tag-kind tok)
                        (:open (when (and (string-equal name (tag-name tok))
                                          (not (void-tag-name-p (tag-name tok))))
                                 (incf depth)))
                        (:close (when (string-equal name (tag-name tok))
                                  (when (zerop (decf depth))
                                    (return (values (tag-end tok) (1+ i)))))))
                   finally (return (values nil nil))))))
      )))

(defun enclosing-element (tokens pos len)
  "Return (values START END NAME CLOSEDP) for the innermost element of
TOKENS that spans POS, or NIL. The element must begin strictly before
POS, so a position sitting on a tag's own < is outside that tag and
inside whatever contains it -- which is what the indenter needs when a
line begins with a tag. CLOSEDP is true when the element has a
closing tag of its own -- or needs none, being void or self-closing -- and
false when it is still open at POS. An element that is never closed is
taken to run to the end of the text, which is what lets the auto-close
command work while the tag is still being typed."
  (let ((stack '())
        (best nil))
    (flet ((consider (open end closedp &optional at-end-of-text)
             ;; A properly closed element does not enclose the position
             ;; just past it. One that is never closed does, so that the
             ;; auto-close command still works when the tag being typed is
             ;; the last thing in the form.
             (let ((start (tag-start open)))
               (when (and (< start pos)
                          (if at-end-of-text (<= pos end) (< pos end))
                          (or (null best) (> start (first best))))
                 (setf best (list start end (tag-name open) closedp))))))
      (loop for tok across tokens
            do (case (tag-kind tok)
                 (:open
                  (if (void-tag-name-p (tag-name tok))
                      (consider tok (tag-end tok) t)
                      (push tok stack)))
                 (:close
                  (let ((match (position (tag-name tok) stack
                                         :key #'tag-name :test #'string-equal)))
                    (when match
                      ;; Anything above the match was left unclosed; it is
                      ;; implicitly closed here, but is not CLOSEDP.
                      (loop for k from 0 to match
                            do (consider (pop stack) (tag-end tok) (= k match))))))
                 ((:self :comment :decl)
                  (consider tok (tag-end tok) t))))
      (dolist (open stack)
        (consider open len nil t)))
    (when best
      (values (first best) (second best) (third best) (fourth best)))))

(defun lisp-escape-extent (text start len)
  "START is the offset of a ,( ,@ or =( escape. Return (values START END)
where END is just past the escaping form."
  (let ((i start))
    (loop while (and (< i len) (member (char text i) '(#\= #\, #\@ #\' #\`)))
          do (incf i))
    (cond
      ((>= i len) (values start len))
      ((char= (char text i) #\() (values start (skip-lisp-form text i len)))
      (t
       (loop while (and (< i len)
                        (not (whitespacep (char text i)))
                        (not (member (char text i) '(#\< #\> #\)))))
             do (incf i))
       (values start i)))))

(defun enclosing-lisp-section (text pos &optional (len (length text)))
  "Return (values START END) for the innermost ,( ,@ or =( escape that
spans POS. With no enclosing escape the whole text is the section, which
mirrors the Emacs mode's use of point-min and point-max."
  (let ((best nil)
        (i 0))
    (loop
      (when (>= i (1- len)) (return))
      (let ((ch (char text i))
            (next (char text (1+ i))))
        (cond
          ((or (and (char= ch #\,) (or (char= next #\() (char= next #\@)))
               (and (char= ch #\=) (char= next #\()))
           (multiple-value-bind (start end) (lisp-escape-extent text i len)
             (when (and (< start pos) (< pos end)
                        (or (null best) (> start (first best))))
               (setf best (list start end))))
           (incf i))
          (t (incf i)))))
    (if best
        (values (first best) (second best))
        (values 0 len))))

(defun in-html-p (text pos &optional (tokens (tokenize text)) (len (length text)))
  "True when POS sits in HTML rather than in Lisp. As in the Emacs mode,
that means the innermost enclosing element is itself contained in the
innermost enclosing Lisp escape -- so the point after ,( inside a <div>
counts as Lisp, not HTML."
  (multiple-value-bind (html-start html-end) (enclosing-element tokens pos len)
    (when html-start
      (multiple-value-bind (lisp-start lisp-end) (enclosing-lisp-section text pos len)
        (and (<= lisp-start html-start)
             (<= html-end lisp-end))))))

(defun enclosing-tag-name (text pos &optional (tokens (tokenize text)) (len (length text)))
  "The name of the innermost element enclosing POS, and whether that
element is already closed. NIL when there is no enclosing element."
  (multiple-value-bind (start end name closedp) (enclosing-element tokens pos len)
    (declare (ignore start end))
    (values name closedp)))


;;; From buffer to text
;;; ===================
;;;
;;; Everything above works on strings. These bridge to the editor by
;;; pulling out the text of the top level form around a point, which is
;;; the same region the fontifier scans.

(defun enclosing-form-region (point)
  "Return two temporary points bounding the top level form around POINT.
While a tag is still being typed the form is unbalanced and its end
cannot be found, in which case the end of the buffer is used."
  (let ((buffer (editor:point-buffer point)))
    (editor:with-point ((start point :temporary))
      (editor:line-start start)
      (cond
        ((editor::top-level-offset start -1)
         (editor:with-point ((end start :temporary))
           (values (editor:copy-point start :temporary)
                   (if (editor:form-offset end 1)
                       (editor:copy-point end :temporary)
                       (editor:copy-point (editor:buffers-end buffer) :temporary)))))
        (t
         (values (editor:copy-point (editor:buffers-start buffer) :temporary)
                 (editor:copy-point (editor:buffers-end buffer) :temporary)))))))

(defun text-around (point)
  "Return the text of the top level form around POINT and the offset of
POINT within it."
  (multiple-value-bind (start end) (enclosing-form-region point)
    (values (editor:points-to-string start end)
            (editor::count-characters start point))))

;;; Indentation
;;; ===========
;;;
;;; The Emacs mode decides between Lisp and HTML indentation by looking at
;;; the end of the previous line, and then needs five special cases. Since
;;; containment here is strict -- an element must begin before the position
;;; to contain it -- the position at the start of a line already describes
;;; the context the line sits in, and one rule covers everything:
;;;
;;;   a line that closes its parent lines up with that parent;
;;;   any other HTML line sits one step in from its parent;
;;;   anything in Lisp is left to LispWorks' own Lisp indenter.

(defparameter *default-indent-offset* 2
  "Columns per level of HTML nesting, when the editor variable is unset.")

(defun column-of (text pos)
  "The column POS sits at, counting characters from the start of its line."
  (let ((bol (position #\Newline text :end pos :from-end t)))
    (if bol (- pos bol 1) pos)))

(defun line-content-offset (text bol)
  "The offset of the first non-blank character of the line starting at BOL,
or the end of the line when it is blank."
  (let ((len (length text))
        (pos bol))
    (loop while (and (< pos len) (member (char text pos) '(#\Space #\Tab)))
          do (incf pos))
    pos))

(defun closes-tag-p (text pos name)
  "True when a closing tag for NAME begins at POS."
  (and (< pos (length text))
       (char= (char text pos) #\<)
       (let ((tag (tag-at text pos (length text))))
         (and tag
              (eq (tag-kind tag) :close)
              (string-equal name (tag-name tag))))))

(defun tag-containing (tokens pos)
  "The tag whose own <...> POS falls strictly inside, or NIL. A position
here is on a continuation line of a tag whose attributes span lines."
  (loop for tok across tokens
        when (and (member (tag-kind tok) '(:open :close :self))
                  (< (tag-start tok) pos)
                  (< pos (tag-end tok)))
          do (return tok)))

(defun tag-attribute-column (text tag)
  "The column of TAG's first attribute, so that attributes continuing on
later lines line up under it rather than merely stepping in."
  (let ((pos (tag-attrs-start tag))
        (len (length text)))
    (loop while (and (< pos len) (member (char text pos) '(#\Space #\Tab)))
          do (incf pos))
    (column-of text pos)))

(defun markup-indent-column (text pos &optional (step *default-indent-offset*))
  "The column the line whose content begins at POS should be indented to,
or NIL when the line is Lisp and should be left to the Lisp indenter."
  (let* ((len (length text))
         (tokens (tokenize text)))
    (when (in-html-p text pos tokens len)
      (let ((tag (tag-containing tokens pos)))
        (if tag
            (tag-attribute-column text tag)
            (multiple-value-bind (start end name) (enclosing-element tokens pos len)
              (declare (ignore end))
              (when start
                (let ((parent-column (column-of text start)))
                  (if (closes-tag-p text pos name)
                      parent-column
                      (+ parent-column step))))))))))

(defun indent-offset (buffer)
  (or (editor:variable-value-if-bound 'markup-indent-offset :buffer buffer)
      *default-indent-offset*))

(defun markup-indent-line (point)
  "The Markup mode's Indent-Function."
  (let ((buffer (editor:point-buffer point)))
    (editor:with-point ((bol point :before-insert))
      (editor:line-start bol)
      (multiple-value-bind (text bol-offset) (text-around bol)
        (let ((column (markup-indent-column text
                                            (line-content-offset text bol-offset)
                                            (indent-offset buffer))))
          (if column
              ;; This is what region-indent-using-tabs does per line, minus
              ;; its undo wrapper: recording-for-undo-locking is a
              ;; compile-time-only macro inside the editor build and is not
              ;; available at runtime.
              (editor::insert-space-at-start bol column nil)
              (editor::indent-for-lisp point)))))))

;;; deftag defines a function, so it should indent like defun rather than
;;; like a call. Without this the body of a deftag lines up under the tag
;;; name, which is a long way to the right.
(editor::setup-indent "deftag" 2 2 7)

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
                :vars '((editor::font-lock-fontify-keywords-region-function
                         . fontify-keywords-region)
                        (editor::indent-function . markup-indent-line)
                        ;; Line by line, like the Emacs mode: less efficient
                        ;; than a bespoke region indenter, but correct.
                        (editor::indent-region-function
                         . editor::region-indent-using-tabs)
                        (markup-indent-offset . 2)))

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


(editor:defcommand "Markup Close Tag" (p)
     "Insert a closing tag for the innermost tag around point that has not
been closed yet."
     "Insert a closing tag for the innermost unclosed tag."
  (declare (ignore p))
  (let ((point (editor:current-point)))
    (multiple-value-bind (text offset) (text-around point)
      (multiple-value-bind (name closedp) (enclosing-tag-name text offset)
        (cond
          ((null name)
           (editor:editor-error "No enclosing tag to close"))
          (closedp
           ;; The Emacs mode happily inserts a second </span> here. Refusing
           ;; is more useful, and the auto-close case is unaffected: a tag
           ;; you are still typing has no closing tag yet.
           (editor:editor-error "<~a> is already closed" name))
          (t
           (editor:insert-string point (format nil "</~a>" name))))))))

(editor:defcommand "Markup Show Context" (p)
     "Report the innermost enclosing tag around point and whether point is
in HTML or in Lisp. Useful for checking what the mode thinks is going on."
     "Report the enclosing tag around point."
  (declare (ignore p))
  (let ((point (editor:current-point)))
    (multiple-value-bind (text offset) (text-around point)
      (multiple-value-bind (name closedp) (enclosing-tag-name text offset)
        (editor:message "~:[Lisp~;HTML~]~@[, inside <~a>~]~:[~; (closed)~]"
                        (in-html-p text offset)
                        name
                        (and name closedp))))))

(defun enable ()
  "Entry point for a LispWorks init file. Currently a no-op beyond
loading this file, which defines the \"Markup\" mode and the
\"Markup Mode\" command."
  (values))
