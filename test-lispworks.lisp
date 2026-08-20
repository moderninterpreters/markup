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
                #:attribute-name-char-p))
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
