(in-package :test-markup)

(markup:enable-reader)

(test identity-walk
  (is (equal
       (write-html <h1>foobar</h1>)
       (write-html (walk <h1>foobar</h1> 'identity)))))

(test simple-replace
  (is (equal
       (write-html <h1 foo= "2">foobar</h1>)
       (write-html (walk  <h1>foobar</h1>
                         (lambda (x)
                           (add-attrs x :foo "2")))))))

(test check-name
  (is (equal 'h1 (xml-tag-name <h1>hello</h1>))))

(test replace-in-inner-place
  (is (equal
       (write-html <b>fdfd<h1 foo= "2" >foobar</h1></b>)
       (write-html (walk <b>fdfd<h1>foobar</h1></b>
                         (lambda (x)
                           (cond
                             ((equal :h1 (xml-tag-name x))
                              (add-attrs x :foo "2"))
                             (t
                              x))))))))
