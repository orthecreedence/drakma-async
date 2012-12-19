drakma-async
============
This is a port of the wonderful [drakma](http://weitz.de/drakma/) library to run
on top of [cl-async](https://github.com/orthecreedence/cl-async).

*PLEASE* use the latest (git) versions of [cl-libevent2](/orthecreedence/cl-libevent2)
and [cl-async](/orthecreedence/cl-async) to run `drakma-async`. If you use the
quicklisp versions, at the very best SSL won't work, and at the very worst
nothing will work.

This library is now API-compatible with drakma.

Documentation
-------------
`drakma-async:http-request` takes the same arguments as [drakma:http-request](http://weitz.de/drakma/#http-request),
and does its absolute best to have the exact same behavior, except instead of
returning the values of the HTTP request made, it returns a [cl-async future](http://orthecreedence.github.com/cl-async/future)
that is finished with the values of the HTTP request.

Here's a simple usage example (using the [cl-async future macros](http://orthecreedence.github.com/cl-async/future#nicer-syntax)):

```common-lisp
(defun my-http-request ()
  (future-handler-case
    (multiple-future-bind (body status headers)
        (das:http-request "https://www.google.com/")
      (format t "Status: ~a~%" status)
      (format t "Headers: ~s~%" headers)
      (format t "Body: ~a~%" (if (stringp body) body (babel:octets-to-string body))))
    (http-eof ()
      (format t "Server hung up unexpectedly =[~%"))
    (t (e)
      (format t "Error: ~a~%" e))))

(as:start-event-loop #'my-http-request :catch-app-errors t)
```

### Tests
`drakma-async` comes with a test suite:

```common-lisp
(ql:quickload :drakma-async-test)

(drakma-async-test:run-tests)
```

### SSL
This library makes use of the `cl-async-ssl` package, which is an add-on package
for `cl-async` to provide SSL functionality. `drakma-async` will use SSL by
default, unless `:drakma-no-ssl` is present in `*features*` during load/compile
time.

Same goes for the test suite: if `:drakma-no-ssl` is present in `*features*`
when the tests are loaded, no SSL tests are performed.

### Notes
`drakma-async` provides a function in the `drakma` package called `http-request-async`
which mirrors `drakma:http-request`, except for that the final value returned is
a closure instead of the finished request values. This closure is to be called
when *all content in the response has been returned*. This is handled by the
[http-stream implementation](https://github.com/orthecreedence/drakma-async/blob/master/http-stream.lisp)
which is able to tell when a response has been completely downloaded. Once the
closure is called, it reads all the data from the http-stream the request was
sent on, and parses it via drakma's normal capabilities.

Obviously the downside to this is that the entire response has to be held in
memory. For this reason, this library would be best used for smaller requests
and *sending* large amounts of data as opposed to downloading large amounts of
data. Note that there is an [issue outlining this problem and a potential fix](/orthecreedence/drakma-async/issues/5),
but it hasn't been worked on YET.

Porting of `drakma:http-request` to `drakma:http-request-async` is now completely
automated through the use of the `rewrite-http-request` macro (along with some
tree search/replace functions in [util.lisp](https://github.com/orthecreedence/drakma-async/blob/master/util.lisp)).
This makes upgrading `drakma-async` to use the latest version of drakma as
simple as copying the `http-request` function from drakma's request.lisp and
pasting it inside the `(rewrite-http-request ...)` macro in drakma-aync's
hijack.lisp. This saves me a lot of time doing manual porting, and makes it
easy to use another version of drakma if that's required.
