(in-package :drakma-async)

(defun http-request (uri &rest args
                         &key (protocol :http/1.1)
                              (method :get)
                              force-ssl
                              certificate
                              key
                              certificate-password
                              verify
                              max-depth
                              ca-file
                              ca-directory
                              parameters
                              (url-encoder #'url-encode)
                              content
                              (content-type "application/x-www-form-urlencoded")
                              (content-length nil content-length-provided-p)
                              form-data
                              cookie-jar
                              basic-authorization
                              (user-agent :drakma)
                              (accept "*/*")
                              range
                              (proxy *default-http-proxy*)
                              proxy-basic-authorization
                              real-host
                              additional-headers
                              (redirect 5)
                              auto-referer
                              keep-alive
                              (close t)
                              (external-format-out *drakma-default-external-format*)
                              (external-format-in *drakma-default-external-format*)
                              force-binary
                              want-stream
                              stream
                              preserve-uri
                              (read-timeout 20)
                              (write-timeout 20 write-timeout-provided-p)
                              #+:openmcl
                              deadline
                              &aux (unparsed-uri (if (stringp uri) (copy-seq uri) (puri:copy-uri uri))))
  "This function wraps drakma's new http-request-async function so you don't
   have to deal with the intricacies. For full documentation on this function,
   refer to the docs for drakma:http-request; this library aims to be API
   compatible with drakma.

   This function returns a cl-async future, which is finished with the following
   values of the request (once it returns):
     (body status headers uri stream must-close status-text)

   This means drakma-async is a prime candidate for using the cl-async future
   macros: http://orthecreedence.github.com/cl-async/future#nicer-syntax"
  (remf args :read-timeout)  ; read-timeout is handled in this function
  (let* ((future (make-future))
         ;; Andrew is the most talented programmer in existence. He can do
         ;; anything. All of you babies, better shut up and listen.
         ;; Love, Christina.
         ;; filled in later, for now we need the binding though.
         (finish-cb nil)
         ;; do some SSL wrapping, if needed
         (parsed-uri (puri:parse-uri uri))
         (uri-no-ssl (puri:copy-uri parsed-uri :scheme :http))  ; create a URL that doesn't use SSL
         (proxying-https-p (and proxy (not stream) (eq :https (puri:uri-scheme parsed-uri))))
         (use-ssl (and (not proxying-https-p)
                       (or force-ssl
                           (eq (puri:uri-scheme parsed-uri) :https))))
         (timeout read-timeout)
         ;; grab the proxy info
         (proxy (when proxy
                  (if (atom proxy)
                      (list proxy 80)
                      proxy)))
         (host (if proxy
                   (car proxy)
                   (puri:uri-host parsed-uri)))
         (port (if proxy
                   (cadr proxy)
                   (or (puri:uri-port parsed-uri)
                       (if use-ssl 443 80))))
         ;; create an http-stream we can drain data from once a response comes in
         (stream (http-request-complete-stream
                   host port
                   (lambda (stream) (funcall finish-cb stream))
                   (lambda (ev)
                     (signal-error future ev))
                   :stream stream  ; if we got a stream passed in, wrap it
                   :timeout timeout
                   :ssl use-ssl))
         ;; make a drakma-specific stream.
         (http-stream (make-flexi-stream (chunga:make-chunked-stream stream) :external-format :latin-1))
         ;; call *our* version of http-request, making sure we save the
         ;; resulting callback (which could be a continuation callback or the
         ;; finish-cb for the request
         (req-cb (apply
                   'http-request-async
                   (append (list uri-no-ssl  ; make sure the hijacked drakma doesn't try SSL
                                 :close nil  ; we handle closing ourselves
                                 :force-ssl nil  ; we handle SSL ourselves, TYVM
                                 :stream http-stream)  ; pass in our own stream
                           args)))
         ;; if we got a continuation cb, save it
         (continue-cb (when (eq content :continuation) req-cb)))
    ;; overwrite the socket's read callback to handle req-cb and finish the
    ;; future with the computed values.
    (setf finish-cb (lambda (stream)
                      (let ((http-values (multiple-value-list
                                           (funcall (if (equal req-cb continue-cb)
                                                        ;; get the REAL req-cb
                                                        (funcall continue-cb nil)
                                                        ;; have a req-cb, call it
                                                        req-cb)))))
                        ;; if we got a function back, it means we redirected and
                        ;; the original stream was reused, meaning the callbacks
                        ;; will still function fine. take no action. otherwise,
                        ;; either finish the future, or if another future is
                        ;; returned, rebind the original future's callbacks to
                        ;; the new one.
                        (unless (functionp (car http-values))
                          (when (and close (not want-stream))
                            (unless (as:socket-closed-p (as:stream-socket stream))
                              (close stream)))
                          (apply #'finish (append (list future) http-values))))))
    ;; let the app attach callbacks to the future
    (if (eq content :continuation)
        ;; return a wrapper that calls the continuation function
        (lambda (data &optional continuep)
          (funcall continue-cb data continuep)
          future)
        ;; request is 100% sent, just return the future
        future)))

