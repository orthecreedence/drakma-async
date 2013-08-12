(defpackage :drakma-async-test
  (:use :cl :eos :cl-async-base :cl-async-util :drakma-async :cl-async-future)
  (:export #:run-tests))
(in-package :drakma-async-test)

(defmacro async-let ((&rest bindings) &body body)
  "Wrap an async op inside of a let/start-event-loop block to mimick a blocking
   action. Bindings must be set from withing the block via setf."
  `(let ,bindings
     (as:start-event-loop
       (lambda ()
         ,@body)
       :catch-app-errors t)
     (values ,@(loop for (binding . nil) in bindings collect binding))))

(defun test-timeout (seconds)
  "Make sure a test times out after the given num seconds. An event loop can't
   do this itself without skewing results. Uses event base IDs to make sure it
   doesn't cancel an event loop that test-timeout wasn't called inside of."
  (let ((event-base (event-base-c *event-base*))
        (base-id (event-base-id *event-base*)))
    (let ((cancel nil))
      ;; if the event loop exits naturally, cancel the break
      (as:add-event-loop-exit-callback
        (lambda () (setf cancel t)))
      ;; spawn the thread to kill the event loop
      (handler-case
        (bt:make-thread (lambda ()
                         (sleep seconds)
                         (when (and *event-base*
                                    (eql (event-base-id *event-base*) base-id)
                                    (not cancel))
                           (le:event-base-loopexit event-base (cffi:null-pointer)))))
        (bt::bordeaux-mp-condition ()
          nil)))))

(def-suite drakma-async-test :description "cl-async test suite")
(in-suite drakma-async-test)

(test simple-request
  "Runs a simple http request against google"
  (multiple-value-bind (http-status)
      (async-let ((http-status nil))
        (test-timeout 5)
        (multiple-future-bind (nil status)
            (das:http-request "http://www.google.com/")
          (setf http-status status)))
    (is (= http-status 200))))

(test redirect
  "Runs a redirect test"
  (multiple-value-bind (http-status)
      (async-let ((http-status nil))
        (test-timeout 5)
        (multiple-future-bind (nil status)
            (das:http-request "http://google.com/") ; google.com redirects to www.google.com
          (setf http-status status)))
    (is (= http-status 200))))

(test redirect-fail
  "Runs a redirect test"
  (signals drakma::drakma-simple-error
    (async-let ((http-status nil))
      (test-timeout 2)
      (future-handler-case
        (multiple-future-bind (nil status)
            (das:http-request "http://google.com/"
                              :read-timeout 2
                              :redirect 0) ; google.com redirects to www.google.com
          (format t "STATUS: ~a~%" status)
          (setf http-status status))
        (t (e)
          (error e))))))

(test want-stream
  "Makes sure :want-stream returns a stream that can be operated on again, and
   also makes sure that :want-stream keeps the stream open"
  (multiple-value-bind (status1 status2 socket-equal-p)
      (async-let ((status1 nil)
                  (status2 nil)
                  (socket-equal-p)
                  (socket1 nil))
        (test-timeout 4)
        (multiple-future-bind (stream status)
            (das:http-request "http://www.google.com/"
                              :want-stream t)
          (setf status1 status
                socket1 (das::get-underlying-socket stream))
          (multiple-future-bind (stream status)
              (das:http-request "http://www.google.com/"
                                :stream stream
                                :want-stream t)
            (setf status2 status)
            (setf socket-equal-p (equal socket1
                                        (das::get-underlying-socket stream)))
            (close stream))))
    (is (= status1 200))
    (is (= status2 200))
    (is-true socket-equal-p)))

(test stream-stays-open
  "Test that a stream stays open when :close is nil"
  (multiple-value-bind (is-open)
      (async-let ((is-open nil))
        (test-timeout 3)
        (multiple-future-bind (nil nil nil nil stream)
            (das:http-request "http://www.google.com/"
                              :close nil)
          (setf is-open (open-stream-p stream))
          (ignore-errors (close stream))))
    (is-true is-open)))

#-(or :drakma-no-ssl)
(test ssl
  "Test that https functions properly (set :drakma-no-ssl into *features* to
   disable)"
  (multiple-value-bind (http-status)
      (async-let ((http-status))
        (test-timeout 5)
        (multiple-future-bind (nil status)
            (das:http-request "http://www.google.com")
          (setf http-status status)))
    (is (= http-status 200))))

(test proxy
  "Test that proxying works. Since this relies on a potentially non-reliable
   proxy (I picked the first one off a list), feel free to update the tests to
   use a more reliable proxy."
  (multiple-value-bind (http-status)
      (async-let ((http-status))
        (test-timeout 5)
        (multiple-future-bind (nil status)
            (das:http-request "http://www.google.com"
                              :proxy '("108.178.2.70" 7080))
          (setf http-status status)))
    (is (= http-status 200))))

(test proxy-redirect
  "Test that redirecting works over a proxy."
  (multiple-value-bind (http-status)
      (async-let ((http-status))
        (test-timeout 5)
        (multiple-future-bind (nil status)
            (das:http-request "http://google.com"
                              :proxy '("108.178.2.70" 7080))
          (setf http-status status)))
    (is (= http-status 200))))

