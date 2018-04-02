;;;; ps-react-gadgets.lisp

(defpackage #:ps-react-gadgets
  (:use #:cl #:parenscript #:ps-gadgets #:cl-react)
  (:export
   #:ps-react-gadgets))

(in-package #:ps-react-gadgets)

(def-ps-lib ps-react-gadgets ()
  (ps

    (def-component update-notify
        ;;FIXME: Consider implementing multi-child.
        (let ((children
               (if (atom (prop children))
                   (prop children)
                   (throw "updateNotify should only be used with one child"))))
          (chain -react
                 (clone-element children
                                (create :dispatch (@ this new-dispatch)))))
      get-initial-state
      (lambda ()
        (create :callbacks []
                :olddispatch (prop children props dispatch)))
      new-dispatch
      (lambda (action)
        ;;FIXME: Review callback storage. Maybe should delete old.
        (if (eq (@ action type) 'set-callback)
            (set-state callbacks
                       (list (@ action 'callback)))
                      ; (chain (state callbacks)
                      ;        (append (list (@ action 'callback)))))
            (when (state :olddispatch)
              (funcall (state :olddispatch) action))))
      component-will-receive-props
      (lambda ()
        (dolist (x (state callbacks))
          (funcall x))))

    (def-component json-loader
        (let ((child
               (if (atom (prop children))
                   (prop children)
                   (throw "jsonLoader should only be used with one child"))))
          (clone-element
           child
           (if (prop store-name)
               (create-from-list (list (prop store-name) (@ this state)))
               (@ this state))))
      component-did-mount
      (lambda ()
        (do-keyvalue (key url (prop sources))
          (json-bind (res url)
              (set-state key res)))))

    (def-component display-if
        (if (if (prop predicate)
                  (funcall (prop predicate) (@ this props))
                  (prop test))
            (prop children)
            null))

    ))
