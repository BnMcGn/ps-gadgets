;;;; ps-react-gadgets.asd

(asdf:defsystem #:ps-react-gadgets
  :description "Describe ps-react-gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:parenscript
               #:gadgets
               #:ps-gadgets
               #:alexandria)
  :serial t
  :components ((:file "ps-react-gadgets")))

