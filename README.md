drakma-async
============
This is a port of the wonderful [drakma](http://weitz.de/drakma/) library to run
on top of [cl-async](https://github.com/orthecreedence/cl-async).

Although working and (nearly) API-compatible with drakma, I recommend *not using
drakma-async* until this notice is removed (unless you wish to help
building/testing).

Documentation
-------------
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

`drakma-async:http-request` takes the same arguments as [drakma:http-request](http://weitz.de/drakma/#http-request)
except for ones that `http-request` manages directly and aren't exposed:

 - `:close` We always close in this version, fix for this coming soon
 - `:want-stream` Will most likely be re-enabled when `:close` is implemented
 - `:stream` drakma-async manages the stream for us, at least until `:close` is
   implemented.

### Notes
`drakma-async` provides a function in the `drakma` package called `http-request-async`
which mirrors `drakma:http-request`, except for that the final value returned is
a closure instead of the finished request values. This closure is to be called
when *all content in the response has been returned*. This is handled by cl-async's
[http-stream implementation](https://github.com/orthecreedence/cl-async/blob/future%2Bstream/http-stream.lisp)
which is able to tell when a response has been completely downloaded. Once the
closure is called, it reads all the data from the http-stream the request was
sent on, and parses it via drakma's normal capabilities.

Obviously the downside to this is that the entire response has to be held in
memory. For this reason, this library would be best used for smaller requests
and *sending* large amounts of data as opposed to downloading large amounts of
data. I may eventually build an HTTP stream that would support
decoding/streaming asynchronously, but it would be outside the bounds of this
project (and probably included directly in cl-async's http-stream
implementation).

- Redirects are currently _supported_.
- SSL is currently _supported+ (via cl-async-ssl) but is *NOT* loaded if
`:drakma-no-ssl` is present in `*features*` (much like SSL for normal drakma).
