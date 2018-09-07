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

    ;;FIXME: This is not operational. Caused mysterious problems for descendants. Not
    ;; Debugged
    (def-component where-in-parent
        (psx
         (:|React.Fragment|
           (children-map
            (prop children)
            (lambda (child)
              (if (is-valid-element child)
                  (clone-element child (@ this state))
                  child)))))
      get-initial-state
      (lambda ()
        (create where (create) where-p nil))
      component-did-mount
      (lambda ()
        (let ((par (chain document (get-element-by-id (prop parent))))
              (chld (chain document (get-element-by-id (prop child)))))
          (when (and par chld)
            (set-state
             :where-p t
             :where (position-difference par chld))))))

    (def-component json-loader
        (let ((child
               (if (atom (prop children))
                   (prop children)
                   (throw "jsonLoader should only be used with one child"))))
          (clone-element child (state storage)))
      component-did-mount
      (lambda ()
        (let ((dispatch (or (prop dispatch) (@ this default-dispatch))))
          (if (arrayp (prop sources))
             (dolist (url (prop sources))
               (json-bind (res url)
                   (set-state
                    storage
                    (funcall
                     dispatch
                     (state storage)
                     (if (prop store-name)
                         (create-from-list (prop store-name) res)
                         res)))))
             (do-keyvalue (key url (prop sources))
               (json-bind (res url)
                   (let ((stor (create-from-list (list key res))))
                     (set-state
                      :storage
                      (funcall
                       dispatch
                       (state storage)
                       (if (prop store-name)
                           (create-from-list (list (prop store-name) stor))
                           stor)))))))))
      default-dispatch
      (lambda (existing incoming)
        incoming)
      get-initial-state (lambda () (create storage (create))))

    (def-component display-if
        (if (if (prop predicate)
                  (funcall (prop predicate) (@ this props))
                  (prop test))
            (prop children)
            null))

    ))
