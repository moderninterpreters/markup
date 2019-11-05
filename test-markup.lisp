;; Copyright 2019, Modern Interpreters Inc

(defpackage #:test-markup
  (:use #:markup
        #:fiveam
        #:cl))
(in-package #:test-markup)
(markup:enable-reader)

(test simple
  (is (equal 2 (+ 1 1))))

(test read-tag
  (is (equal "foo" (markup::read-tag (make-string-input-stream "foo>"))))
  (is (equal "foo" (markup::read-tag (make-string-input-stream "foo bar>")))))


(test read-xml
  (is (equal
       '(make-xml-tag :foo)
       (read-xml-from-string "<:foo />") )))

(test read-child
  (is (equal
       '(make-xml-tag :foo :children
         (list (make-xml-tag :bar)))
       (read-xml-from-string "<:foo><:bar /></:foo>")))
  (is (equal
       '(make-xml-tag :foo :children
         (list " " (make-xml-tag :bar)))
       (read-xml-from-string "<:foo> <:bar /></:foo>")))
  (is (equal
       '(make-xml-tag :foo :children
         (list " " (make-xml-tag :bar :children (list "  "))))
       (read-xml-from-string "<:foo   > <:bar>  </:bar></:foo>"))))

(test read-strings
  (is (equal
       '(make-xml-tag :foo :children
         (list "blah"))
       (read-xml-from-string "<:foo>blah</:foo>"))))

(test read-attributes
  (is (equal
       '(make-xml-tag :foo :attributes
         (list (cons "car" "bar")))
       (read-xml-from-string "<:foo car=\"bar\"></:foo>"))))


(test reader
  (is (equal
       '(make-xml-tag :foo :children
         (list (make-xml-tag :bar)))
       (quote <:foo><:bar></:bar></:foo>))))

(test reader-with-attr
  (is (equal
       '(make-xml-tag :foo :attributes (list (cons "car" "bar")))
       (quote <:foo car="bar"></:foo>))))

(test write-html
  (is (equal
       "<foo></foo>"
       (markup:write-html <:foo></:foo>)))
  (is (equal
       "<foo><bar></bar><car></car></foo>"
       (markup:write-html <:foo><:bar></:bar><:car></:car></:foo>))))

(test void-tag
  (is (equal
       "<br />"
       (markup:write-html <:br />)))
  (is (equal
       "<br />"
       (markup:write-html <br />))))

(test write-html-string
  (is (equal
       "<foo>car dar</foo>"
       (markup:write-html <:foo>car dar</:foo>))))

(test write-attributes
  (is (equal
       "<foo bar=\"car\"></foo>"
       (markup:write-html <:foo bar="car"></:foo>))))


(defun simple-wrapper (&key children attributes)
  (declare (ignore children attributes))
  <div></div>)

(test function-calls
  (is (equal
       (markup:write-html <div></div>)
       (markup:write-html <simple-wrapper></simple-wrapper>))))

(test escaping
  (let ((val "blah"))
    (is (equal
         (markup:write-html <div>blah</div>)
         (markup:write-html <div>,(progn val)</div>)))))

(test escaping-in-between
  (let ((val "blah"))
    (is (equal
         (markup:write-html <div>1blah2</div>)
         (markup:write-html <div>1,(progn val)2</div>)))))

(test escaping-with-space-in-between
  (let ((val "blah"))
    (is (equal
         (markup:write-html <div>blah car</div>)
         (markup:write-html <div>,(progn val) ,(progn "car")</div>)))))


(test escaping-attr-vals
  (let ((val "blah"))
    (is (equal
         (markup:write-html <div arg="blah"></div>)
         (markup:write-html <div arg=(progn val)></div>)))))

(test escaping-attr-vals
  (let ((val "blah"))
    (is (equal
         (markup:write-html <div arg="blah"></div>)
         (markup:write-html <div arg=val ></div>)))))

(markup:deftag wrapper (children &key foo)
  (declare (ignore children))
  <:h1>Foo was ,(progn foo)</:h1>)

(test function-calls
  (is (equal
       (markup:write-html <:h1>Foo was blah</:h1>)
       (markup:write-html <wrapper foo="blah"></wrapper>)))
  (is (equal
       (markup:write-html <:h1>Foo was </:h1>)
       (markup:write-html <wrapper></wrapper>))))

(markup:deftag wrapper2 (children &key (foo "defaultvalue"))
  (declare (ignore children))
  <:h1>Foo was ,(progn foo)</:h1>)

(test function-calls-2
  (is (equal
       (markup:write-html <:h1>Foo was blah</:h1>)
       (markup:write-html <wrapper2 foo="blah"></wrapper2>)))
  (is (equal
       (markup:write-html <:h1>Foo was defaultvalue</:h1>)
       (markup:write-html <wrapper2></wrapper2>))))


(markup:deftag wrapper-with-children (children &key (foo "defaultvalue"))
  <:h1>Foo was ,(progn foo),@(progn children)</:h1>)

(test function-calls-wrapper
  (is (equal
       (markup:write-html <:h1>Foo was blah</:h1>)
       (markup:write-html <wrapper-with-children foo="blah"></wrapper-with-children>)))
  (is (equal
       (markup:write-html <:h1>Foo was defaultvalue<:blah></:blah></:h1>)
       (markup:write-html <wrapper-with-children><:blah></:blah></wrapper-with-children>))))

(test commas
  (is (equal
       "<h1>Hello, world</h1>"
       (markup:write-html <:h1>Hello, world</:h1>))))

(markup:deftag without-key (children)
  (declare (ignore children))
  <h1>hello world</h1>)

(test without-key
  (is (equal "<h1>hello world</h1>"
             (markup:write-html <without-key />))))

(markup:deftag without-children ()
  <h1>hello world</h1>)

(test without-children
  (is (equal "<h1>hello world</h1>"
             (markup:write-html <without-children />))))

(markup:deftag with-only-key (&key att)
  <h1>hello ,(progn att)</h1>)

(test with-only-key
  (is (equal "<h1>hello zoidberg</h1>"
             (markup:write-html <with-only-key att="zoidberg" />))))

(test operators
  (is (equal t (< 1 2)))
  (is (equal nil (< 2 1)))
  (is (equal t (<= 1 2))))

(test escaping
  (let ((body "<body>"))
    (is (equal
         "<h1>&lt;body&gt;</h1>"
         (markup:write-html <:h1>,(progn body)</:h1>))))
  (let ((body "\"body\""))
    (is (equal
         "<h1 arg=\"&quot;body&quot;\">foo</h1>"
         (markup:write-html <:h1 arg=(progn body)>foo</:h1>)))))

(test unescaped-string
  (let ((body "<h1></h1>"))
    (is (equal
         "<body><h1></h1></body>"
         (markup:write-html <:body>,(progn (markup:unescaped body))</:body>)))))

(test reading-void-tags
  (is (equal
       "<img />"
       (markup:write-html <img>)))
  (is (equal
       "<p><img /></p>"
       (markup:write-html <p><img></p>))))

(test comments
  (is (equal
       "<body> <!-- this is a test --></body>"
       (markup:write-html <:body> <!-- this is a test --></:body>))))

(test />-without-space
  (is (equal
       "<body></body>"
       (markup:write-html <:body/>))))


(test default-escaping
  (is (equal
       "<body>News &amp; Events</body>"
       (markup:write-html <body>News &amp; Events</body>
                         ))))

(test but-escapes-inline-commas
  (let ((val "News & Events"))
    (is (equal
         "<body>News &amp; Events</body>"
         (markup:write-html <body>,(progn val)</body>)))))
