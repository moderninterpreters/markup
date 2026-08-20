;;;; test-lispworks.lisp -- tests for the LispWorks editor mode
;;;;
;;;; Part of the markup/tests system, but guarded with :if-feature
;;;; :lispworks in the .asd: the editor mode only exists on LispWorks, so
;;;; on SBCL this file is never even read.
;;;;
;;;; The interesting logic in markup/lispworks is the scanner, which is a
;;;; pure function from a string to a list of highlights. That is what is
;;;; tested here; the buffer/point plumbing around it is thin enough to
;;;; check by eye in the IDE.

(uiop:define-package #:markup/test-lispworks
    (:use #:cl
          #:fiveam)
  (:import-from #:markup/lispworks
                #:scan
                #:skip-lisp-string
                #:skip-lisp-form
                #:tag-name-char-p
                #:attribute-name-char-p
                #:tokenize
                #:tag-kind
                #:tag-name
                #:tag-start
                #:tag-end
                #:void-tag-name-p
                #:skip-element-forward
                #:enclosing-element
                #:enclosing-lisp-section
                #:in-html-p
                #:enclosing-tag-name
                #:markup-indent-column
                #:column-of
                #:line-content-offset
                #:closes-tag-p))
(in-package #:markup/test-lispworks)

(def-suite* :markup.test-lispworks)

(defun face-tag (face)
  "Map a face object back to a keyword, so tests can be written against
readable names. Note that *TAG-FACE* and the face used for a deftag's
name are the same object (LispWorks' font-lock-function-name-face), so
both report as :TAG."
  (cond
    ((eq face markup/lispworks::*keyword-tag-face*) :keyword-tag)
    ((eq face markup/lispworks::*attribute-face*) :attribute)
    ((eq face markup/lispworks::*warning-face*) :warning)
    ((eq face markup/lispworks::*comment-face*) :comment)
    ((eq face markup/lispworks::*tag-face*) :tag)
    ((eq face editor::*font-lock-keyword-face*) :lisp-keyword)
    (t face)))

(defun highlights (text)
  "Run SCAN over TEXT and return a list of (SUBSTRING TAG) pairs."
  (loop for (from to face) in (scan text)
        collect (list (subseq text from to) (face-tag face))))

;;; Tag names

(test scans-a-simple-tag
  (is (equal '(("div" :tag)) (highlights "<div>")))
  (is (equal '(("div" :tag) ("div" :tag)) (highlights "<div></div>"))))

(test scans-keyword-tags-with-the-builtin-face
  (is (equal '((":my-tag" :keyword-tag)) (highlights "<:my-tag>")))
  (is (equal '((":my-tag" :keyword-tag)) (highlights "</:my-tag>"))))

(test tag-names-may-contain-punctuation
  (is (equal '(("my-tag.v2" :tag)) (highlights "<my-tag.v2>"))))

(test self-closing-tags
  (is (equal '(("br" :tag)) (highlights "<br />")))
  (is (equal '(("br" :tag)) (highlights "<br/>"))))

;;; Things that look like tags but are not

(test a-less-than-operator-is-not-a-tag
  (is (equal '() (highlights "(when (< a b) 1)")))
  (is (equal '() (highlights "(< x 3)")))
  (is (equal '() (highlights "a < b"))))

(test a-bare-less-than-at-end-of-input-is-not-a-tag
  (is (equal '() (highlights "<")))
  (is (equal '() (highlights "foo <"))))

(test doctype-is-left-alone
  (is (equal '() (highlights "<!DOCTYPE html>"))))

;;; Attributes

(test attribute-names-are-highlighted
  (is (equal '(("div" :tag) ("class" :attribute))
             (highlights "<div class=\"a\">")))
  (is (equal '(("div" :tag) ("class" :attribute) ("id" :attribute))
             (highlights "<div class=\"a\" id=\"b\">"))))

(test hyphenated-attribute-names
  (is (equal '(("div" :tag) ("data-foo" :attribute))
             (highlights "<div data-foo=\"1\">"))))

(test boolean-attributes-are-not-highlighted
  ;; The Emacs mode requires a following = before it fontifies an
  ;; attribute name, and so do we.
  (is (equal '(("input" :tag)) (highlights "<input disabled />"))))

(test attribute-values-that-are-lisp-forms
  (is (equal '(("img" :tag) ("src" :attribute))
             (highlights "<img src=(logo-dark) />")))
  (is (equal '(("img" :tag) ("src" :attribute))
             (highlights "<img src= (logo-dark) />")))
  (is (equal '(("img" :tag) ("src" :attribute))
             (highlights "<img src=,(logo-dark) />"))))

(test a-paren-value-containing-a-string-with-a-gt-does-not-end-the-tag
  (is (equal '(("a" :tag) ("href" :attribute) ("title" :attribute))
             (highlights "<a href=(fmt \"a>b\") title=\"x\">"))))

(test attributes-spanning-several-lines
  ;; This is the case SCAN-REGION-START exists for: refontifying only the
  ;; second line would otherwise lose the highlight on ID.
  (is (equal '(("div" :tag) ("class" :attribute) ("id" :attribute))
             (highlights (format nil "<div class=\"a\"~%     id=\"b\">")))))

;;; The unquoted-value warning

(test unquoted-value-abutting-the-tag-end-is-flagged
  (is (equal '(("div" :tag) ("class" :attribute) (">" :warning))
             (highlights "<div class=foo>")))
  (is (equal '(("br" :tag) ("class" :attribute) ("/" :warning))
             (highlights "<br class=foo/>"))))

(test the-tag-still-ends-after-an-unquoted-value
  ;; Regression: the warning used to consume the > that ends the tag, so
  ;; everything after it was scanned as though it were still attributes
  ;; and the closing tag went unhighlighted.
  (is (equal '(("div" :tag) ("class" :attribute) (">" :warning) ("div" :tag))
             (highlights "<div class=foo>hello</div>"))))

(test a-well-formed-value-is-not-flagged
  (is (equal '(("div" :tag) ("class" :attribute))
             (highlights "<div class=\"foo\">")))
  (is (equal '(("div" :tag) ("class" :attribute))
             (highlights "<div class=(foo)>")))
  (is (equal '(("div" :tag) ("class" :attribute))
             (highlights "<div class=foo >"))))

;;; HTML comments

(test html-comments-are-highlighted-whole
  (is (equal '(("<!-- hi -->" :comment)) (highlights "<!-- hi -->")))
  (is (equal '(("<!-- <div> -->" :comment)) (highlights "<!-- <div> -->"))))

(test an-unterminated-html-comment-runs-to-the-end
  (is (equal '(("<!-- hi" :comment)) (highlights "<!-- hi"))))

(test scanning-continues-after-a-comment
  (is (equal '(("<!-- hi -->" :comment) ("p" :tag))
             (highlights "<!-- hi --><p>"))))

;;; deftag

(test deftag-is-highlighted
  (is (equal '(("deftag" :lisp-keyword) ("my-widget" :tag))
             (highlights "(deftag my-widget (children))"))))

(test deftag-without-a-following-space-is-ignored
  (is (equal '() (highlights "(deftagged x)"))))

;;; Highlight ordering

(test highlights-come-back-in-buffer-order
  (let ((result (scan "<div class=\"a\"><:x y=1></div>")))
    (is (equal (mapcar #'first result)
               (sort (mapcar #'first (copy-list result)) #'<)))))

;;; The string and form skippers

(test skip-lisp-string-handles-escapes
  ;; The text is  "a\"b" rest  -- the closing quote is at index 5.
  (let ((s "\"a\\\"b\" rest"))
    (is (= 6 (skip-lisp-string s 0 (length s))))))

(test skip-lisp-string-on-an-unterminated-string
  (let ((s "\"abc"))
    (is (= 4 (skip-lisp-string s 0 (length s))))))

(test skip-lisp-form-handles-nesting
  (let ((s "(a (b c) d) rest"))
    (is (= 11 (skip-lisp-form s 0 (length s))))))

(test skip-lisp-form-ignores-parens-inside-strings
  (let ((s "(a \")\" b) rest"))
    (is (= 9 (skip-lisp-form s 0 (length s))))))

(test skip-lisp-form-on-an-unbalanced-form
  (let ((s "(a (b"))
    (is (= 5 (skip-lisp-form s 0 (length s))))))

;;; Character predicates

(test tag-name-chars
  (is-true (tag-name-char-p #\a))
  (is-true (tag-name-char-p #\:))
  (is-true (tag-name-char-p #\-))
  (is-false (tag-name-char-p #\>))
  (is-false (tag-name-char-p #\/))
  (is-false (tag-name-char-p #\=))
  (is-false (tag-name-char-p #\Space))
  (is-false (tag-name-char-p #\!))
  (is-false (tag-name-char-p nil)))

(test attribute-name-chars
  (is-true (attribute-name-char-p #\a))
  (is-true (attribute-name-char-p #\-))
  (is-false (attribute-name-char-p #\=))
  (is-false (attribute-name-char-p #\Space))
  (is-false (attribute-name-char-p nil)))


;;; ==========================================================================
;;; Navigating elements
;;; ==========================================================================
;;;
;;; The helpers below take a string in which a single | marks the point of
;;; interest, which keeps the expectations readable.

(defun split-at-caret (text)
  "Return (values TEXT-WITHOUT-CARET POSITION-OF-CARET)."
  (let ((pos (position #\| text)))
    (assert pos () "Test string has no | marker: ~s" text)
    (values (remove #\| text) pos)))

(defun kinds (text)
  (loop for tok across (tokenize text) collect (tag-kind tok)))

(defun names (text)
  (loop for tok across (tokenize text) collect (tag-name tok)))

(defun spans (text)
  (loop for tok across (tokenize text) collect (list (tag-start tok) (tag-end tok))))

(defun element-span (text)
  "(list START END) of the innermost element around the caret, or NIL."
  (multiple-value-bind (s pos) (split-at-caret text)
    (multiple-value-bind (start end) (enclosing-element (tokenize s) pos (length s))
      (when start (list start end)))))

(defun element-name (text)
  (multiple-value-bind (s pos) (split-at-caret text)
    (nth-value 2 (enclosing-element (tokenize s) pos (length s)))))

(defun lisp-section (text)
  (multiple-value-bind (s pos) (split-at-caret text)
    (multiple-value-list (enclosing-lisp-section s pos))))

(defun html-at-caret-p (text)
  (multiple-value-bind (s pos) (split-at-caret text)
    (in-html-p s pos)))

(defun tag-to-close (text)
  (multiple-value-bind (s pos) (split-at-caret text)
    (enclosing-tag-name s pos)))

(defun tag-closed-p (text)
  (multiple-value-bind (s pos) (split-at-caret text)
    (nth-value 1 (enclosing-tag-name s pos))))

;;; Tokenizing

(test tokenize-classifies-each-kind
  (is (equal '(:open :close) (kinds "<div></div>")))
  (is (equal '(:self) (kinds "<br />")))
  (is (equal '(:open) (kinds "<br>")))
  (is (equal '(:comment) (kinds "<!-- hi -->")))
  (is (equal '(:decl) (kinds "<!DOCTYPE html>"))))

(test tokenize-records-names-and-spans
  (is (equal '("div" "div") (names "<div></div>")))
  (is (equal '((0 5) (5 11)) (spans "<div></div>"))))

(test tokenize-skips-non-tags
  (is (equal '() (kinds "(when (< a b) 1)")))
  ;; the < in (< a b) is not a tag; the two tokens are <span> and </span>
  (is (equal '("span" "span") (names "<span>,(if (< a b) 1 2)</span>")))
  (is (equal '(:open :close) (kinds "<span>,(if (< a b) 1 2)</span>"))))

(test tokenize-does-not-see-inside-comments-or-tags
  ;; A < inside an HTML comment or inside a tag's attributes must not
  ;; become a token of its own.
  (is (equal '(:comment) (kinds "<!-- <div> <span> -->")))
  (is (equal '(:open) (kinds "<div class=(if (< a b) \"x\" \"y\")>"))))

;;; Void tags

(test void-tag-names
  (is-true (void-tag-name-p "br"))
  (is-true (void-tag-name-p "BR"))
  (is-true (void-tag-name-p "img"))
  (is-true (void-tag-name-p "input"))
  (is-false (void-tag-name-p "div"))
  (is-false (void-tag-name-p nil)))

;;; skip-element-forward

(test skip-over-a-simple-element
  (is (= 11 (skip-element-forward (tokenize "<div></div>") 0)))
  (is (= 24 (skip-element-forward (tokenize "<div><span></span></div>") 0))))

(test skip-returns-the-index-of-the-following-token
  (is (= 2 (nth-value 1 (skip-element-forward (tokenize "<div></div>") 0))))
  (is (= 4 (nth-value 1 (skip-element-forward (tokenize "<div><span></span></div>") 0)))))

(test skip-counts-nesting-of-the-same-name
  (let ((text "<div><div></div></div>"))
    (is (= (length text) (skip-element-forward (tokenize text) 0)))))

(test skip-over-self-closing-and-void-elements
  (is (= 6 (skip-element-forward (tokenize "<br />") 0)))
  (is (= 4 (skip-element-forward (tokenize "<br>") 0)))
  ;; A self-closing element ends at its own >, not at whatever follows.
  (let ((tokens (tokenize "<img src=\"x\" /><p></p>")))
    (is (= 15 (skip-element-forward tokens 0)))
    (is (= 22 (skip-element-forward tokens 1)))))

(test skip-over-an-unterminated-element-fails
  (is (null (skip-element-forward (tokenize "<div>") 0)))
  (is (null (skip-element-forward (tokenize "<div><span></span>") 0))))

(test skip-tolerates-a-missing-inner-close
  ;; <span> is never closed, but </div> still ends the div.
  (let ((text "<div><span></div>"))
    (is (= (length text) (skip-element-forward (tokenize text) 0)))))

(test skip-from-a-closing-tag-is-not-an-element
  (is (null (skip-element-forward (tokenize "</div>") 0))))

;;; enclosing-element

(test innermost-enclosing-element
  (is (equal "div" (element-name "<div>|</div>")))
  (is (equal "span" (element-name "<div><span>|</span></div>")))
  (is (equal "div" (element-name "<div><span></span>|</div>"))))

(test enclosing-element-span
  (is (equal '(0 11) (element-span "<div>|</div>"))))

(test point-inside-a-tag-is-inside-that-element
  (is (equal "div" (element-name "<div |class=\"x\">y</div>")))
  (is (equal "br" (element-name "<br |/>")))
  (is (equal "img" (element-name "<img |src=\"x\">"))))

(test an-unclosed-element-runs-to-the-end-of-the-text
  ;; This is what makes the auto-close command work while you are still
  ;; typing the element.
  (is (equal "div" (element-name "<div>|")))
  (is (equal "span" (element-name "<div><span>|"))))

(test no-enclosing-element
  (is (null (element-name "(defun foo (|))")))
  (is (null (element-name "<div></div>|"))))

(test a-position-on-a-tags-own-bracket-is-outside-that-tag
  ;; Containment is strict, unlike the Emacs mode's (<= start point). This
  ;; is what lets the indenter ask "what encloses this line?" using the
  ;; position of the line's first character.
  (is (null (element-name "|<div></div>")))
  (is (equal "p" (element-name "<p>|<div></div></p>"))))

(test a-void-element-does-not-enclose-what-follows-it
  (is (equal "div" (element-name "<div><br>|</div>"))))

;;; enclosing-lisp-section

(test with-no-escape-the-whole-text-is-the-section
  (is (equal '(0 14) (lisp-section "(defun foo (|))"))))

(test innermost-lisp-escape
  ;; Offsets are into the text with the caret removed, so ,(foo ) spans 5..12.
  (is (equal '(5 12) (lisp-section "<div>,(foo |)</div>")))
  (is (equal '(5 12) (lisp-section "<div>=(foo |)</div>"))))

(test nested-lisp-escapes
  (is (equal '(9 14) (lisp-section "<div>,(a ,(b |) c)</div>"))))

(test a-point-outside-any-escape-gets-the-whole-text
  (let ((text "<div>,(foo)</div>"))
    (is (equal (list 0 (length text)) (lisp-section "<div>,(foo)</div>|")))))

;;; in-html-p

(test text-inside-an-element-is-html
  (is-true (html-at-caret-p "<div>|</div>"))
  (is-true (html-at-caret-p "<div>hello |world</div>")))

(test inside-a-lisp-escape-is-not-html
  (is-false (html-at-caret-p "<div>,(progn |)</div>"))
  (is-false (html-at-caret-p "<div>,(foo (bar |))</div>"))
  (is-false (html-at-caret-p "<img src=(logo |) />")))

(test plain-lisp-is-not-html
  (is-false (html-at-caret-p "(defun foo (|))"))
  (is-false (html-at-caret-p "(list 1 2 |3)")))

(test markup-nested-back-inside-a-lisp-escape-is-html-again
  (is-true (html-at-caret-p "<div>,(progn <span>|</span>)</div>")))

(test inside-a-tags-attributes-is-html
  (is-true (html-at-caret-p "<div |class=\"x\">y</div>")))

;;; unclosed-tag-name

(test the-tag-a-closing-tag-would-close
  (is (equal "div" (tag-to-close "<div>|")))
  (is (equal "span" (tag-to-close "<div><span>|"))))

(test a-closed-inner-element-is-skipped
  ;; The example from the Emacs mode's docstring: with point on the blank
  ;; line, </div> is what should be inserted.
  (is (equal "div" (tag-to-close (format nil "<div>~%  <span></span>~%  |~%")))))

(test keyword-tags-can-be-closed
  (is (equal ":my-tag" (tag-to-close "<:my-tag>|"))))

(test nothing-to-close
  (is (null (tag-to-close "(defun foo (|))")))
  (is (null (tag-to-close "<div></div>|"))))

;;; Whether the enclosing element is already closed

(test an-element-still-being-typed-is-not-closed
  (is-false (tag-closed-p "<div>|"))
  (is-false (tag-closed-p "<div><span>|"))
  (is-false (tag-closed-p (format nil "<div>~%  <span></span>~%  |~%"))))

(test an-element-with-its-own-closing-tag-is-closed
  (is-true (tag-closed-p "<div>|</div>"))
  (is-true (tag-closed-p "<div><span>|</span></div>")))

(test an-implicitly-closed-inner-element-is-not-closed
  ;; <span> has no </span> of its own; the </div> ends it. Closing it is
  ;; still the useful thing to offer.
  (is (equal "span" (tag-to-close "<div><span>|</div>")))
  (is-false (tag-closed-p "<div><span>|</div>")))

(test void-and-self-closing-elements-count-as-closed
  (is-true (tag-closed-p "<br |>"))
  (is-true (tag-closed-p "<br |/>")))


;;; ==========================================================================
;;; Indentation
;;; ==========================================================================

(defun split-lines (text)
  (with-input-from-string (in text)
    (loop for line = (read-line in nil) while line collect line)))

(defun join-lines (lines)
  (format nil "~{~a~%~}" (coerce lines 'list)))

(defun reindent (text &optional (step 2))
  "Strip every line's indentation and re-indent it with the mode's rule,
the way running Indent Region over the whole form would."
  (let ((lines (map 'vector
                    (lambda (line) (string-left-trim '(#\Space #\Tab) line))
                    (split-lines text))))
    (dotimes (i (length lines) (join-lines lines))
      (let* ((joined (join-lines lines))
             (bol (loop for k from 0 below i sum (1+ (length (aref lines k)))))
             (column (markup-indent-column joined bol step)))
        (when column
          (setf (aref lines i)
                (concatenate 'string
                             (make-string column :initial-element #\Space)
                             (aref lines i))))))))

;;; The helpers

(test column-of-counts-from-the-start-of-the-line
  (is (= 0 (column-of "abc" 0)))
  (is (= 2 (column-of "abc" 2)))
  (is (= 0 (column-of (format nil "ab~%cd") 3)))
  (is (= 2 (column-of (format nil "ab~%  cd") 5))))

(test line-content-offset-skips-blanks
  (is (= 0 (line-content-offset "abc" 0)))
  (is (= 2 (line-content-offset "  abc" 0)))
  ;; a blank line yields the position of its newline
  (is (= 2 (line-content-offset (format nil "  ~%x") 0)))
  (is (= 3 (line-content-offset (format nil "  ~%x") 3))))

(test closes-tag-p-matches-only-the-right-closing-tag
  (is-true (closes-tag-p "</div>" 0 "div"))
  (is-true (closes-tag-p "</DIV>" 0 "div"))
  (is-false (closes-tag-p "</span>" 0 "div"))
  (is-false (closes-tag-p "<div>" 0 "div"))
  (is-false (closes-tag-p "hello" 0 "div")))

;;; The rule

(test lisp-lines-are-left-to-the-lisp-indenter
  (is (null (markup-indent-column "(defun foo ()" 0)))
  (is (null (markup-indent-column (format nil "(defun foo ()~%(list 1)") 14))))

(test nested-elements-step-in-one-level
  ;; REINDENT exercises the markup rule alone, so Lisp lines such as a
  ;; leading (defun ...) come back untouched; in the editor those go to
  ;; LispWorks' own indenter instead.
  (is (string= (format nil "<div>~%  <p>hi</p>~%</div>~%")
               (reindent (format nil "<div>~%<p>hi</p>~%</div>~%")))))

(test a-closing-tag-lines-up-with-its-opening-tag
  (is (string= (format nil "<div>~%  <span>~%    x~%  </span>~%</div>~%")
               (reindent (format nil "<div>~%<span>~%x~%</span>~%</div>~%")))))

(test the-step-is-configurable
  (is (string= (format nil "<div>~%    <p>hi</p>~%</div>~%")
               (reindent (format nil "<div>~%<p>hi</p>~%</div>~%") 4))))

(test void-and-self-closing-elements-do-not-indent-what-follows
  (is (string= (format nil "<div>~%  <br />~%  <img src=\"x\">~%  <p>hi</p>~%</div>~%")
               (reindent (format nil "<div>~%<br />~%<img src=\"x\">~%<p>hi</p>~%</div>~%")))))

(test text-content-indents-like-an-element
  (is (string= (format nil "<p>~%  hello world~%</p>~%")
               (reindent (format nil "<p>~%hello world~%</p>~%")))))

(test a-lisp-escape-indents-as-html-but-its-body-does-not
  ;; The ,( line is HTML content of the <div>, so it steps in. The forms
  ;; inside it are Lisp, so markup-indent-column declines and LispWorks'
  ;; own indenter handles them -- which is why they come back unchanged.
  (let ((result (reindent (format nil "<div>~%,(progn~%(foo))~%<p>x</p>~%</div>~%"))))
    (is (string= (format nil "<div>~%  ,(progn~%(foo))~%  <p>x</p>~%</div>~%")
                 result))))

(test continuation-lines-align-under-the-first-attribute
  (is (string= (format nil "<div>~%  <a href=\"x\"~%     id=\"y\">~%    hi~%  </a>~%</div>~%")
               (reindent (format nil "<div>~%<a href=\"x\"~%     id=\"y\">~%hi~%</a>~%</div>~%")))))
