drakma-async
============
This is a port of the wonderful [drakma](http://weitz.de/drakma/) library to run
on top of [cl-async](https://github.com/orthecreedence/cl-async).

The goal is to support 100% of drakma's features, but provide an asynchronous
interface. A lofty goal, yes, but I believe attainable.

### Please don't use this yet
This has only been minimally tested. It requires cl-async's
[future+stream](https://github.com/orthecreedence/cl-async/tree/future+stream)
branch to operate correctly, as it uses both TCP/HTTP streams and futures.

__It is highly experimental.__

I plan to do a lot more testing and polishing on it before publishing, so
consider it alpha for now.

Documentation
-------------
Here's a simple usage example:

```common-lisp
(defun my-http-request ()
  (let* ((url "http://www.google.com/")
         (future (drakma-async:http-async url)))
    (as:set-event-handler future (lambda (ev) (format t "future ev: ~a~%" ev)))
    (as:attach future
	  ;; lambda list is the same as values returned by drakma:http-request
	  (lambda (body status headers uri stream must-close status-text)
        (format t "body:~%~a~%" body)))))

(as:start-event-loop #'my-http-request)
```

`http-async` takes the same arguments as [drakma:http-request](http://weitz.de/drakma/#http-request)
except for ones that `http-async` manages directly and aren't exposed:

 - `:close` We always close in this version, fix for this coming soon
 - `:want-stream` Will most likely be re-enabled when `:close` is implemented
 - `:stream` drakma-async manages the stream for us, at least until `:close` is
   implemented.

### Unimplemented
Some things haven't been put in that are worth mentioning

##### SSL
Since `http-client-stream` in cl-async needs to intercept the incoming request,
it must be able to decode the data before it receives it. If SSL is implemented
in drakma (like it is currently), http-stream will have no idea when the response
has finished.

In order to work around this, I may use SSL through libevent since it supports
it natively. Obviously, I will try to mimick/copy the parameters and
functionality that drakma provides as closely as possible.

##### content -> :continuation
Passing `:continuation` as the value to `:content` is not implemented. Doing so
is more than possible, but would change the interface slightly and I want to
think about the best way to do this.

### Notes
`http-async` provides a function in the `drakma` package called `http-request-async`
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

Redirects are currently _supported_.
