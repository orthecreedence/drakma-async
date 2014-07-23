(defpackage :drakma-async
  (:use :cl :alexandria :flexi-streams :cl-async-future :alexandria :drakma :puri :flexi-streams :chunga)
  (:shadow http-request)
  (:shadowing-import-from :drakma
                          syntax-error)
  (:export #:http-request
           #:http-eof
           #:http-timeout)
  (:import-from :drakma
                +known-methods+
                +latin-1+
                +redirect-codes+
                +redirect-to-get-codes+
                +redirect-to-get-methods+

                *drakma-default-external-format*
                *header-stream*

                default-port
                non-default-port
                user-agent-string

                alist-to-url-encoded-string

                make-random-string
                make-form-data-function
                make-ssl-stream

                send-content

                read-status-line
                read-body

                dissect-query
                trivial-uri-path

                drakma-simple-error
                drakma-warn
                
                update-cookies
                send-cookie-p
                get-cookies
                set-referer)
  (:nicknames :das))

