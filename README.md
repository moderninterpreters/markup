# markup
### Arnold Noronha <arnold@tdrhq.com>

Markup let's you write HTML code inside of common lisp, for instance

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

## FAQ

### What about expressions like `(< x 2)`?

Markdown requires tags to follow the `<` operator, otherwise (or if it's `<=`) treats it as a symbol.

To simplify editor support in the future, we recommend a style guide of using `(|<| x 2)` in markup enabled code.

### Are custom tags namespaced?

Of course, custom tags are just lisp symbols. So you can define a tag like `<admin:template>...</admin:template>`.

Certain tag names are treated as special (`<b>`, `<body>` etc.) since they're HTML elements.

If you want to output the equivalent HTML element for a tag that isn't
treated as special you can also specify the tag using keyword symbols `<:foo>..</:foo>`.

### How do you embed lisp code in markup?

You have already seen some examples in this README. Use `,(...)` to
escape some lisp code that returns a single element, or ,@sexp that
returns a list of elements. (Side note, we really don't need to have
both of these, but it matches the backquote syntax much better this
way).

You can also embed lisp code as attribute values.

```lisp
  <a href=(generate-url ...) >...</a>
```

That is, any expression after the an attribute is read using the
standard Lisp reader. A small caveat to this is that in some cases you need to have a space after the ending `>`. For instance the following will result in an error:

```lisp
   ;; bad code
   <a href=url-var>...</a>
   ;; correct code
   <a href=url-var >...</a>
```

### Is markup used in production?

Yes it is! Right now it's used on several websites we've built. Granted, they're not high-traffic (for now), but they've solved all of our use cases reliably.

## See also

XHP for PHP, and Scala both support HTML inside of code for very similar
motivations.

## License

Apache License, Version 2.0
