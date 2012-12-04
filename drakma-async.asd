(asdf:defsystem drakma-async
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An asynchronous port of the Drakma HTTP client."
  :depends-on (#-(or :drakma-no-ssl) #:cl-async-ssl
               #+(or :drakma-no-ssl) #:cl-async
			   #:flexi-streams #:drakma)
  :components
  ((:file "package")
   (:file "http-stream" :depends-on ("package"))
   (:file "hijack" :depends-on ("http-stream"))
   (:file "drakma" :depends-on ("hijack"))))
