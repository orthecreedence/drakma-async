(defpackage :drakma-async
  (:use :cl :drakma :flexi-streams)
  (:export #:http-async)
  (:nicknames :das))

(in-package :drakma)
(defun http-request-async (uri &rest args
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
                                    content
                                    (content-type "application/x-www-form-urlencoded")
                                    (content-length nil content-length-provided-p)
                                    form-data
                                    cookie-jar
                                    basic-authorization
                                    (user-agent :drakma)
                                    (accept "*/*")
                                    range
                                    proxy
                                    proxy-basic-authorization
                                    additional-headers
                                    (redirect 5)
                                    (redirect-methods '(:get :head))
                                    auto-referer
                                    keep-alive
                                    (close t)
                                    (external-format-out *drakma-default-external-format*)
                                    (external-format-in *drakma-default-external-format*)
                                    force-binary
                                    want-stream
                                    stream
                                    preserve-uri
                                    #+(or abcl clisp lispworks mcl openmcl sbcl)
                                    (connection-timeout 20)
                                    #+:lispworks (read-timeout 20)
                                    #+(and :lispworks (not :lw-does-not-have-write-timeout))
                                    (write-timeout 20 write-timeout-provided-p)
                                    #+:openmcl
                                    deadline
                                    &aux (unparsed-uri (if (stringp uri) (copy-seq uri) (puri:copy-uri uri))))
  "This function mirrors drakma:http-request. The difference is that in the
   finish-request sub-function, it returns a function instead of parsed HTTP
   values. This function is to be called with no arguments when the stream
   passed to http-request-async has a completed request on it.
   
   Note that you are *required* by law to pass :stream with a flexi-stream as
   a value which wraps around an async-io-stream, which wraps around a cl-async
   TCP socket.
   
   This function is meant to be wrapped by drakma-async:http-async, which takes
   similar arguments to this one and functions much like the original drakma."
  (unless (member protocol '(:http/1.0 :http/1.1) :test #'eq)
    (parameter-error "Don't know how to handle protocol ~S." protocol))
  (setq uri (cond ((uri-p uri) (copy-uri uri))
                  (t (parse-uri uri))))
  (unless (member method +known-methods+ :test #'eq)
    (parameter-error "Don't know how to handle method ~S." method))
  (unless (member (uri-scheme uri) '(:http :https) :test #'eq)
    (parameter-error "Don't know how to handle scheme ~S." (uri-scheme uri)))
  (when (and close keep-alive)
    (parameter-error "CLOSE and KEEP-ALIVE must not be both true."))
  (when (and form-data (not (member method '(:post :report) :test #'eq)))
    (parameter-error "FORM-DATA makes only sense with POST requests."))
  (when range
    (unless (and (listp range)
                 (integerp (first range))
                 (integerp (second range))
                 (<= (first range) (second range)))
      (parameter-error "RANGE parameter must be specified as list of two integers, with the second larger or equal to the first")))
  ;; convert PROXY argument to canonical form
  (when proxy
    (when (atom proxy)
      (setq proxy (list proxy 80))))
  ;; make sure we don't get :CRLF on Windows
  (let ((*default-eol-style* :lf)
        (file-parameters-p (find-if-not (lambda (thing)
                                          (or (stringp thing)
                                              (null thing)))
                                        parameters :key #'cdr))
        parameters-used-p)
    (when (and file-parameters-p (not (eq method :post)))
      (parameter-error "Don't know how to handle parameters in ~S, as this is not a POST request."
                       parameters))
    (when (eq method :post)
      ;; create content body for POST unless it was provided
      (unless content
        ;; mark PARAMETERS argument as used up, so we don't use it
        ;; again below
        (setq parameters-used-p t)
        (cond ((or form-data file-parameters-p)
               (let ((boundary (format nil "----------~A" (make-random-string))))
                 (setq content (make-form-data-function parameters boundary)
                       content-type (format nil "multipart/form-data; boundary=~A" boundary)))
               (unless (or file-parameters-p content-length-provided-p)
                 (setq content-length (or content-length t))))
              (t
               (setq content (alist-to-url-encoded-string parameters external-format-out)
                     content-type "application/x-www-form-urlencoded")))))
    (let ((proxying-https-p (and proxy (not stream) (eq :https (puri:uri-scheme uri))))
           http-stream raw-http-stream must-close done)
      (unwind-protect
          (progn
            (let ((host (or (and proxy (first proxy))
                            (uri-host uri)))
                  (port (cond (proxy (second proxy))
                              ((uri-port uri))
                              (t (default-port uri))))
                  (use-ssl (and (not proxying-https-p)
                                (or force-ssl
                                    (eq (uri-scheme uri) :https)))))
              #+(and :lispworks5.0 :mswindows
                     (not :lw-does-not-have-write-timeout))
              (when use-ssl
                (when (and write-timeout write-timeout-provided-p)
                  (drakma-warn "Disabling WRITE-TIMEOUT because it doesn't mix well with SSL."))
                (setq write-timeout nil))
              (setq http-stream (or stream
                                    (error "Stream not passed into http-request-async (required)."))
                    raw-http-stream http-stream)
              #+:openmcl
              (when deadline
                ;; it is correct to set the deadline here even though
                ;; it may have been initialized by SOCKET-CONNECT
                ;; already - the stream may have been passed in by the
                ;; user and the user may want to adjust the deadline
                ;; for every request
                (setf (ccl:stream-deadline http-stream) deadline))
            (labels ((write-http-line (fmt &rest args)
                       (when *header-stream*
                         (format *header-stream* "~?~%" fmt args))
                       (format http-stream "~?~C~C" fmt args #\Return #\Linefeed))
                     (write-header (name value-fmt &rest value-args)
                       (write-http-line "~A: ~?" name value-fmt value-args))
                     (wrap-stream (http-stream)
                       (make-flexi-stream (make-chunked-stream http-stream)
                                          :external-format +latin-1+)))
              (when (and use-ssl
                         ;; don't attach SSL to existing streams
                         (not stream))
                #+:lispworks
                (comm:attach-ssl http-stream :ssl-side :client)
                #-:lispworks
                (setq http-stream (make-ssl-stream http-stream
                                                   :certificate certificate
                                                   :key key
                                                   :certificate-password certificate-password
                                                   :verify verify
                                                   :max-depth max-depth
                                                   :ca-file ca-file
                                                   :ca-directory ca-directory)))
              (cond (stream
                     (setf (flexi-stream-element-type http-stream)
                           #+:lispworks 'lw:simple-char #-:lispworks 'character
                           (flexi-stream-external-format http-stream) +latin-1+))
                    (t
                     (setq http-stream (wrap-stream http-stream))))
              (when proxying-https-p
                ;; set up a tunnel through the proxy server to the
                ;; final destination
                (write-http-line "CONNECT ~A:~:[443~;~:*~A~] HTTP/1.1"
                                 (uri-host uri) (uri-port uri))
                (write-http-line "Host: ~A:~:[443~;~:*~A~]"
                                 (uri-host uri) (uri-port uri))
                (write-http-line "")
                (force-output http-stream)
                ;; check we get a 200 response before proceeding
                (unless (eql (second (read-status-line http-stream *header-stream*)) 200)
                  (error "Unable to establish HTTPS tunnel through proxy."))
                ;; got a connection; we have to read a blank line,
                ;; turn on SSL, and then we can transmit
                (read-line* http-stream)
                #+:lispworks
                (comm:attach-ssl raw-http-stream :ssl-side :client)
                #-:lispworks
                (setq http-stream (wrap-stream (make-ssl-stream raw-http-stream))))
              (when-let (all-get-parameters
                         (and (not preserve-uri)
                              (append (dissect-query (uri-query uri))
                                      (and (not parameters-used-p) parameters))))
                (setf (uri-query uri)
                      (alist-to-url-encoded-string all-get-parameters external-format-out)))
              (when (eq method :options*)
                ;; special pseudo-method
                (setf method :options
                      (uri-path uri) "*"
                      (uri-query uri) nil))
              (write-http-line "~A ~A ~A"
                               (string-upcase method)
                               (if (and preserve-uri
                                        (stringp unparsed-uri))
                                   (trivial-uri-path unparsed-uri)
                                   (render-uri (cond
                                                 ((and proxy
                                                       (null stream)
                                                       (not proxying-https-p))
                                                  uri)
                                                 (t
                                                  (make-instance 'uri
                                                                 :path (or (uri-path uri) "/")
                                                                 :query (uri-query uri))))
                                               nil))
                               (string-upcase protocol))
              (write-header "Host" "~A~@[:~A~]" (uri-host uri) (non-default-port uri))
              (when user-agent
                (write-header "User-Agent" "~A" (user-agent-string user-agent)))
              (when basic-authorization
                (write-header "Authorization" "Basic ~A"
                              (base64:string-to-base64-string
                               (format nil "~A:~A"
                                       (first basic-authorization)
                                       (second basic-authorization)))))
              (when (and proxy proxy-basic-authorization)
                (write-header "Proxy-Authorization" "Basic ~A"
                              (base64:string-to-base64-string
                               (format nil "~A:~A"
                                       (first proxy-basic-authorization)
                                       (second proxy-basic-authorization)))))
              (when accept
                (write-header "Accept" "~A" accept))
              (when range
                (write-header "Range" "bytes=~A-~A" (first range) (second range)))
              (when cookie-jar
                ;; write all cookies in one fell swoop, so even Sun's
                ;; web server has a chance to get it
                (when-let (cookies (loop for cookie in (cookie-jar-cookies cookie-jar)
                                         when (send-cookie-p cookie uri force-ssl)
                                         collect (cookie-name cookie) and
                                         collect (cookie-value cookie)))
                  (write-header "Cookie" "~{~A=~A~^; ~}" cookies)))
              (when keep-alive
                (write-header "Connection" "Keep-Alive"))
              (when close
                (setq must-close close)
                (write-header "Connection" "close"))
              (loop for (name . value) in additional-headers
                    do (write-header name "~A"
                                     (cond ((or (functionp value)
                                                (and (symbolp value)
                                                     (fboundp value)))
                                            (funcall value))
                                           (t value))))
              (when content
                (when content-type
                  (write-header "Content-Type" "~A" content-type))
                (when (and content-length
                           (not (or (and (integerp content-length)
                                         (not (minusp content-length)))
                                    (typep content '(or (vector octet) list))
                                    (eq content :continuation))))
                  ;; CONTENT-LENGTH forces us to compute request body
                  ;; in RAM
                  (setq content
                        (with-output-to-sequence (bin-out)
                          (let ((out (make-flexi-stream bin-out :external-format +latin-1+)))
                            (send-content content out external-format-out)))))
                (when (and (or (not content-length-provided-p)
                               (eq content-length t))
                           (typep content '(or (vector octet) list)))
                  (setq content-length (length content)))
                (cond (content-length
                       (write-header "Content-Length" "~D" content-length))
                      (t
                       (write-header "Transfer-Encoding" "chunked"))))
              ;; end of request headers
              (when *header-stream*
                (terpri *header-stream*))
              (format http-stream "~C~C" #\Return #\Linefeed)
              (force-output http-stream)
              (when (and content (null content-length))
                (setf (chunked-stream-output-chunking-p
                       (flexi-stream-stream http-stream)) t))         
              (labels ((finish-request (content &optional continuep)
                         (send-content content http-stream external-format-out)
                         (when continuep
                           (force-output http-stream)
                           (return-from finish-request))
                         (setf (chunked-stream-output-chunking-p
                                (flexi-stream-stream http-stream)) nil)
                         (finish-output http-stream)
                         (lambda ()
                           (block http-request
                             (with-character-stream-semantics
                               (multiple-value-bind (server-protocol status-code status-text)
                                   ;; loop until status is NOT 100
                                   (loop for (server-protocol status-code status-text)
                                         = (read-status-line http-stream *header-stream*)
                                         when (= status-code 100)
                                         ;; ignore headers sent until non-100 status is seen
                                         do (read-http-headers http-stream *header-stream*)
                                         until (/= status-code 100)
                                         finally (return (values server-protocol status-code status-text)))
                                 (let ((headers (read-http-headers http-stream *header-stream*))
                                       body external-format-body)
                                   (let ((connections (header-value :connection headers)))
                                     (when connections
                                       (setq connections (split-tokens connections)))
                                     (when (or (member "close" connections :test #'string-equal)
                                               (not (or (and (eq protocol :http/1.1)
                                                             (eq server-protocol :http/1.1))
                                                        (member "Keep-Alive" connections
                                                                :test #'string-equal))))
                                       (setq must-close t)))
                                   (when cookie-jar
                                     (update-cookies (get-cookies headers uri) cookie-jar))
                                   (when (and redirect
                                              (member status-code +redirect-codes+)
                                              (member method redirect-methods))
                                     (unless (or (eq redirect t)
                                                 (and (integerp redirect)
                                                      (plusp redirect)))
                                       (cerror "Continue anyway."
                                               'drakma-simple-error
                                               :format-control "Status code was ~A, but ~
~:[REDIRECT is ~S~;redirection limit has been exceeded~]."
                                               :format-arguments (list status-code (integerp redirect) redirect)))
                                     (when auto-referer
                                       (setq additional-headers (set-referer uri additional-headers)))
                                     (let* ((location (header-value :location headers))
                                            (new-uri (merge-uris
                                                      (cond ((or (null location)
                                                                 (zerop (length location)))
                                                             (drakma-warn
                                                              "Empty `Location' header, assuming \"/\".")
                                                             "/")
                                                            (t location))
                                                      uri))
                                            ;; can we re-use the stream?
                                            (old-server-p (and (string= (uri-host new-uri)
                                                                        (uri-host uri))
                                                               (eql (uri-port new-uri)
                                                                    (uri-port uri))
                                                               (eq (uri-scheme new-uri)
                                                                   (uri-scheme uri)))))
                                       (unless old-server-p
                                         (setq must-close t
                                               want-stream nil))
                                       ;; try to re-use the stream, but only
                                       ;; if the user hasn't opted for a
                                       ;; connection which is always secure
                                       (let ((re-use-stream (and old-server-p
                                                                 (not must-close)
                                                                 (not force-ssl))))
                                         ;; close stream if we can't re-use it
                                         (unless re-use-stream
                                           (remf args :close)
                                           (remf args :want-stream)
                                           (remf args :stream)
                                           (ignore-errors (close http-stream)))
                                         (setq done t)
                                         (return-from http-request
                                           (apply (if re-use-stream
                                                      #'http-request-async
                                                      #'drakma-async:http-async)
                                                  (append
                                                    (list
                                                      new-uri
                                                      :redirect (cond ((integerp redirect) (1- redirect))
                                                                      (t redirect)))
                                                    (when re-use-stream
                                                      (list :stream http-stream))
                                                    (list
                                                      :additional-headers additional-headers
                                                      ;; don't send GET parameters again in redirect
                                                      :parameters (and (not (eq method :get)) parameters)
                                                      :preserve-uri t)
                                                    args))))))
                                   (let ((transfer-encodings (header-value :transfer-encoding headers)))
                                     (when transfer-encodings
                                       (setq transfer-encodings (split-tokens transfer-encodings)))
                                     (when (member "chunked" transfer-encodings :test #'equalp)
                                       (setf (chunked-stream-input-chunking-p
                                              (flexi-stream-stream http-stream)) t)))
                                   (when (setq external-format-body
                                               (and (not force-binary)
                                                    (funcall *body-format-function*
                                                             headers external-format-in)))
                                     (setf (flexi-stream-external-format http-stream)
                                           external-format-body))
                                   (when force-binary
                                     (setf (flexi-stream-element-type http-stream) 'octet))
                                   (unless (or want-stream (eq method :head))
                                     (let (trailers)
                                       (multiple-value-setq (body trailers)
                                           (read-body http-stream headers must-close external-format-body))
                                       (when trailers
                                         (drakma-warn "Adding trailers from chunked encoding to HTTP headers.")
                                         (setq headers (nconc headers trailers)))))
                                   (setq done t)
                                   (values (cond (want-stream http-stream)
                                                 (t body))
                                           status-code
                                           headers
                                           uri
                                           http-stream
                                           must-close
                                           status-text))))))))
                ;; disable continuations FOR NOW
                ;(when (eq content :continuation)
                ;  (return-from http-request (values #'finish-request t)))
                (finish-request content)))))
        ;; the cleanup form of the UNWIND-PROTECT above
        (when (and http-stream
                   (or ;(not done)
                       (and must-close
                            (not want-stream)))
                   (not (eq content :continuation)))
          (ignore-errors (close http-stream)))))))


(in-package :drakma-async)
(defun http-async (uri &rest args
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
                            content
                            (content-type "application/x-www-form-urlencoded")
                            (content-length nil content-length-provided-p)
                            form-data
                            cookie-jar
                            basic-authorization
                            (user-agent :drakma)
                            (accept "*/*")
                            range
                            proxy
                            proxy-basic-authorization
                            additional-headers
                            (redirect 5)
                            (redirect-methods '(:get :head))
                            auto-referer
                            keep-alive
                            (close t)
                            (external-format-out *drakma-default-external-format*)
                            (external-format-in *drakma-default-external-format*)
                            force-binary
                            want-stream
                            stream
                            preserve-uri
                            #+(or abcl clisp lispworks mcl openmcl sbcl)
                            (connection-timeout 20)
                            #+:lispworks (read-timeout 20)
                            #+(and :lispworks (not :lw-does-not-have-write-timeout))
                            (write-timeout 20 write-timeout-provided-p)
                            #+:openmcl
                            deadline)
  "This function wraps drakma's new http-request-async function so you don't
   have to deal with the intricacies. For full documentation on this function,
   refer to the docs for drakma:http-request, since most parameters are the
   same. There are a few parameters that are controlled by this function fully,
   in particular:
     :close
       Always set to nil. This function will handling closing.
     :want-stream
       Always set to nil. We'll use our own stream.
     :stream
       We pass in our own stream, so you are not allowed to.

   This function returns a cl-async future, to which one or more callbacks can
   be attached. The callbacks must take the same arguments as the return values
   of drakma:http-request:
     (body status headers uri stream must-close status-text)

   The callbacks will be called when the request completes."
  ;; TODO: allow passing in of TCP stream so more than one request can happen on
  ;; a socket
  (let* ((future (as:make-future))
         ;; filled in later
         (finish-cb nil)
         ;; create an http-stream we can drain data from once a response comes in
         (stream (as:http-client-stream
                   uri
                   (lambda (stream)
                     (funcall finish-cb stream))
                   (lambda (ev)
                     (as:signal-event future ev))
                   :timeout (if (boundp 'connection-timeout)
                                connection-timeout
                                20)))
         ;; make a drakma-specific stream.
         (http-stream (make-flexi-stream (chunga:make-chunked-stream stream) :external-format :latin-1))
         ;; call *our* version of http-request which we defined above, making
         ;; sure we save the resulting callback.
         (req-cb (apply
                   #'drakma::http-request-async
                   (append
                     (list uri
                           :close nil
                           :want-stream nil
                           :stream http-stream)
                     args))))
    ;; overwrite the socket's read callback to handle req-cb and finish the
    ;; future with the computed values.
    (setf finish-cb (lambda (stream)
                      (let ((http-values (multiple-value-list (funcall req-cb))))
                        ;; if we got a function back, it means we redirected and
                        ;; the original stream was reused, meaning the callbacks
                        ;; will still function fine. take no action. otherwise,
                        ;; either finish the future, or if another future is
                        ;; returned, rebind the original future's callbacks to
                        ;; the new one.
                        (unless (functionp (car http-values))
                          (unless (as:socket-closed-p (as:stream-socket stream))
                            (close stream))
                          (apply #'as:finish (append (list future) http-values))))))
    ;; let the app attach callbacks to the future
    future))
