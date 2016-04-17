;;;; ps-gadgets.asd

(asdf:defsystem #:ps-gadgets
  :description "Describe ps-gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:parenscript
               #:gadgets
               #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "ps-macros")
               (:file "ps-gadgets")))

