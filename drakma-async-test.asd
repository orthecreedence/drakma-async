(asdf:defsystem drakma-async-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1"
  :description "TESTS FOR An asynchronous port of the Drakma HTTP client."
  :depends-on (#:cffi #:drakma-async #:eos #:bordeaux-threads)
  :components
  ((:module test
	:components ((:file "drakma")
	             (:file "run" :depends-on ("drakma"))))))
