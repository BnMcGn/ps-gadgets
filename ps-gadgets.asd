;;;; ps-gadgets.asd

(asdf:defsystem #:ps-gadgets
  :description "Describe ps-gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:parenscript
               #:gadgets
               #:cl-utilities
               #:alexandria
               #:trivial-types
               #:ps-lib-tool)
  :serial t
  :components ((:file "package")
               (:file "ps-macros")
               (:file "as-ps-data")
               (:file "ps-gadgets")))

