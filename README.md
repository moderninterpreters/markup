# markup
### Arnold Noronha <arnold@tdrhq.com>

Markup let's you write XML/HTML code inside of common lisp, for instance

```lisp
(let ((x "hello"))
  <h1>,(progn x) <em>world</em>!</h1>)
```

## Motivation

There are several HTML generation libraries for Common Lisp, for
example CL-WHO. However, these libraries follow the Lisp style of
building up the structure in parenthesis.

For example, it might look like something like this:


```lisp
;; CL-WHO syntax, not markup's
(:p "hello" (:em "world") "!")
```

There are many advantages to this structure, but there are a few
prominent disadvantages.

First, there's all that double-quotes that becomes hard to track.

Second, and more importantly: There are hundreds of templates and HTML
snippeds on the internet that are hard to copy-paste into your project
if you have to transform them into CL-WHO structures. Time is money.

Finally, it's already hard to hire Lisp engineers. Don't you want to
be able to hire designers who might at least modify HTML they
recognize inside your lisp project?

## Performance

Performance is not a motivation for Markup. We're focussing on
developer productivity. For instance, compared to CL-WHO we generate
the entire tree of HTML tags before serializing it into the stream at
the last step. We haven't reached a situation where this is a
bottleneck for our use cases.

Building the tree also lets us build more complex components that can
go modify the tree.

It might be possible to build a streaming version of Markup, but
that's not on our radar.

## Full example with Hunchentoot

```lisp
(markup:enable-reader)

(markup:deftag template (children &key title)
  <html>
    <head>
     <title>,(progn title)</title>
    </head>
    <body>
      ,@(progn children)
    </body>
  </html>)

(hunchentoot:define-easy-handler (foobar :uri "/") ()
  (markup:write-xml
     <template title="Hello" >
        <h1>hello world!</h1>
     </template>))
```

## Installation

markup is available via quicklisp

```lisp
(ql:quickload "markup")
```

(If that doesn't load, make sure you update your dists, `(ql:update-all-dists)`)

## Editor support

We do not have editor support, even for Emacs, so indentation is going
to be done manually by the developer.

## See also

XHP for PHP, and Scala both support HTML/XML inside of code for very similar
motivations.

## License

Apache License, Version 2.0
