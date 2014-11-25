(asdf:defsystem drakma-async
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.5"
  :description "An asynchronous port of the Drakma HTTP client."
  :depends-on (#-(or :drakma-no-ssl) #:cl-async-ssl
               #+(or :drakma-no-ssl) #:cl-async
               #:cl-async-future
               #:alexandria
               #:flexi-streams
               #:fast-http
               #:fast-io
               #:drakma)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "http-stream" :depends-on ("util"))
   (:file "rewrite" :depends-on ("http-stream"))
   (:file "drakma" :depends-on ("rewrite"))))
