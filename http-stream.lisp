;;; This file holds an HTTP stream implementation such that data can be written
;;; to the stream, but its request-cb callback will *only* be invoked when an
;;; entire response has been returned. If a chunked response is returned, the
;;; same rule applies: until the last empty chunk is sent, the request-cb isn't
;;; triggered.

(in-package :drakma-async)

(define-condition http-eof (as:event-error) ()
  (:report (lambda (c s) (format s "HTTP connection EOF: ~a: ~a" (as:event-errcode c) (as:event-errmsg c))))
  (:documentation "Triggered when an HTTP peer closes the connection."))

(define-condition http-timeout (as:event-error) ()
  (:report (lambda (c s) (format s "HTTP connection timeout: ~a: ~a" (as:event-errcode c) (as:event-errmsg c))))
  (:documentation "Triggered when an HTTP connection times out."))

(defun get-underlying-socket (stream)
  "Given a stream (of type flexi, chunga, or async-stream), grab the underlying
   socket (or return nil)."
  (let ((tmp-stream stream))
    ;; try to drill down and find the underlying socket
    ;; in the passed in stream
    (when (subtypep (type-of tmp-stream) 'flexi-streams:flexi-stream)
      (setf tmp-stream (flexi-streams:flexi-stream-stream stream)))
    (when (subtypep (type-of tmp-stream) 'chunga:chunked-stream)
      (setf tmp-stream (chunga:chunked-stream-stream tmp-stream)))
    (when (subtypep (type-of tmp-stream) 'as:async-stream)
      (as:stream-socket tmp-stream))))

(defun http-request-complete-stream (host port request-cb event-cb &key ssl stream read-timeout write-timeout)
  "Open a TCP stream to the given uri, determine when a full response has been
   returned from the host, and then fire the complete callback, at which point
   the response can be read from the stream."
  (let* ((http-stream nil)
         (http-sock nil)
         (http nil)
         (http-bytes (fast-io:make-output-buffer))
         (parser nil)
         (make-parser nil))
    (flet ((finish-request ()
             
             (let ((evbuf (le:bufferevent-get-input (as::socket-c http-sock)))
                   (remaining-buffer-data nil))
               ;; if the evbuffer has a length > 0, grab any data left on it
               (unless (eq (le:evbuffer-get-length evbuf) 0)
                 (setf remaining-buffer-data (as::drain-evbuffer evbuf)))
               ;; write the response + any extra data back into the evbuffer
               (le:evbuffer-unfreeze evbuf 0)  ; input buffers by default disable writing
               (as::write-to-evbuffer evbuf (fast-io:finish-output-buffer http-bytes))
               (when remaining-buffer-data
                 ;; write existing data back onto end of evbuffer
                 (as::write-to-evbuffer evbuf remaining-buffer-data))
               (le:evbuffer-freeze evbuf 0)  ; re-enable write freeze
               ;; send the finalized stream to the request-cb
               (funcall request-cb http-stream)
               ;; parsing should be done now (it's synchronous), so clear the
               ;; evbuffer out in case we got a redirect and are re-using the
               ;; stream
               (le:evbuffer-drain evbuf 65536)
               ;; reset the parser (in case we get a redirect on the same stream)
               (funcall make-parser))))
      (setf make-parser (lambda ()
                          (setf http (fast-http:make-http-response))
                          (setf parser (fast-http:make-parser http
                                                              :finish-callback #'finish-request))))
      (funcall make-parser)
      (let* ((existing-socket (get-underlying-socket stream))
             (buffer (make-array 65536 :element-type '(unsigned-byte 8)))
             (read-cb (lambda (sock stream)
                        ;; store the http-stream so the other functions can access it
                        (unless http-stream (setf http-stream stream))
                        (unless http-sock (setf http-sock sock))
                        (loop for num-bytes = (read-sequence buffer http-stream :end 8096)
                              while (< 0 num-bytes) do
                          (fast-io:fast-write-sequence buffer http-bytes 0 num-bytes)
                          (funcall parser buffer :start 0 :end num-bytes))))
             (event-cb (lambda (ev)
                         (handler-case (error ev)
                           (as:tcp-eof ()
                             (let ((sock (as:tcp-socket ev)))
                               (unless http-sock (setf http-sock sock))
                               (funcall parser :eof))
                             (funcall event-cb (make-instance 'http-eof
                                                              :code -1
                                                              :msg "HTTP stream client peer closed connection.")))
                           (as:tcp-timeout ()
                             (funcall event-cb (make-instance 'http-timeout
                                                              :code -1
                                                              :msg "HTTP stream client timed out.")))
                           (t ()
                             (funcall event-cb ev))))))
        (if (and existing-socket
                 (not (as:socket-closed-p existing-socket)))
            ;; we have an existing socket, rewrite its callbacks
            (progn
              (as:write-socket-data existing-socket nil
                :read-cb read-cb
                :event-cb event-cb)
              (make-instance 'as:async-io-stream :socket existing-socket))
            ;; new socket/stream
            (apply (if ssl
                       #'as-ssl:tcp-ssl-connect
                       #'as:tcp-connect)
                   (list 
                     host port
                     read-cb event-cb
                     :read-timeout read-timeout
                     :write-timeout write-timeout
                     :stream t)))))))

