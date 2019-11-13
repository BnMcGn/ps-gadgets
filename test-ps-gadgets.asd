;;;; test-ps-gadgets.asd

(asdf:defsystem #:test-ps-gadgets
  :description "Tests for ps-gadgets"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache License, version 2.0"
  :depends-on (#:parenscript
               #:ps-gadgets
               #:ps-react-gadgets
               #:external-program)
  :serial t
  :components ((:module :test
                        :serial t
                        :components ((:file "test-ps-gadgets")))))


