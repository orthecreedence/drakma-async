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

(defparameter *test-data* nil)
(defun http-request-complete-stream (host port request-cb event-cb &key ssl stream read-timeout write-timeout)
  "Open a TCP stream to the given uri, determine when a full response has been
   returned from the host, and then fire the complete callback, at which point
   the response can be read from the stream."
  (let* ((http-stream nil)
         (http-sock nil)
         (http nil)
         (http-bytes (cl-async-util:make-buffer))
         (finished nil)
         (parser nil)
         (make-parser nil))
    (flet ((finish-request ()
             (setf finished t)
             (clear-input http-stream)
             (let ((bytes (cl-async-util:buffer-output http-bytes)))
               (cl-async::stream-append-bytes http-stream bytes))
             ;; send the finalized stream to the request-cb
             (funcall request-cb http-stream)
             (funcall make-parser)))
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
                        (unless finished
                          (loop for num-bytes = (read-sequence buffer http-stream :end 8096)
                                while (< 0 num-bytes) do
                            (cl-async-util:write-to-buffer buffer http-bytes 0 num-bytes)
                            (push (subseq buffer 0 num-bytes) *test-data*)
                            (funcall parser buffer :start 0 :end num-bytes)
                            (when finished (return))))))
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
                       #-drakma-no-ssl #'as-ssl:tcp-ssl-connect
                       #+drakma-no-ssl (error "Drakma SSL disabled; cannot proceed with HTTPS request.")
                       #'as:tcp-connect)
                   (list 
                     host port
                     read-cb
                     :event-cb event-cb
                     :read-timeout read-timeout
                     :write-timeout write-timeout
                     :stream t)))))))

