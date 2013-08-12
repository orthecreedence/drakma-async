(defpackage :drakma-async
  (:use :cl :alexandria :flexi-streams :cl-async-future :alexandria :drakma :puri :flexi-streams :chunga)
  (:shadow http-request)
  (:shadowing-import-from :drakma
                          syntax-error)
  (:export #:http-request
           #:http-eof)
  (:import-from :drakma
                +known-methods+
                +latin-1+
                +redirect-codes+

                *drakma-default-external-format*
                *header-stream*

                default-port
                non-default-port
                user-agent-string

                send-content

                read-status-line
                read-body

                dissect-query

                drakma-simple-error
                drakma-warn
                
                update-cookies
                get-cookies
                set-referer)
  (:nicknames :das))

  
