;;;; ps-react-gadgets.lisp

(defpackage #:ps-react-gadgets
  (:use #:cl #:parenscript #:ps-gadgets))

(in-package #:ps-react-gadgets)

(def-ps-lib ps-react-gadgets ()
  (ps

    (def-component update-notify
        ;;FIXME: Consider implementing multi-child.
        (let ((children
               (if (atom (prop children))
                   (prop children)
                   (raise "updateNotify should only be used with one child"))))
          (set-state 'existing-dispatch (@ children props dispatch))
          (chain -react
                 (clone-element children
                                (create :dispatch (@ self new-dispatch)))))
      get-initial-state
      (lambda ()
        (create :callbacks [] 'existing-dispatch nil))
      new-dispatch
      (lambda (action)
        (if (eq (@ action type) 'set-callback)
            (set-state callbacks
                       (chain (state callbacks)
                              (append (list (@ action 'callback)))))
            (when (state 'existing-dispatch)
              (funcall (state 'existing-dispatch action)))))
      component-did-update
      (lambda ()
        (dolist (x (state callbacks))
          (funcall x))))

    ))
