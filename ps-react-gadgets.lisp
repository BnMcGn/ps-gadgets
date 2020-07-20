;;;; ps-react-gadgets.lisp

(defpackage #:ps-react-gadgets
  (:use #:cl #:parenscript #:ps-gadgets #:cl-react)
  (:export
   #:ps-react-gadgets))

(in-package #:ps-react-gadgets)

(define-ps-lib ps-react-gadgets ()
  (ps

    (def-component update-notify
        ;;FIXME: Consider implementing multi-child.
        (let ((children
               (if (atom (prop children))
                   (prop children)
                   (throw "updateNotify should only be used with one child"))))
          (clone-element children
                         (create :dispatch (@ this new-dispatch))))
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

    ;;Props: sources, reducer, store-name
    (def-component json-loader
        (collecting
          (dolist (child (ensure-array (prop children)))
            (collect
                (clone-element child (if (prop store-name)
                                         (create-from-list (list (prop store-name) (state storage)))
                                         (state storage))))))
      component-did-mount
      (lambda ()
        (let ((reducer (or (prop reducer) (@ this default-reducer))))
          (if (arrayp (prop sources))
             (dolist (url (prop sources))
               (json-bind (res url () :error-func (lambda () (@ this status)))
                   (set-state
                    storage
                    (funcall
                     reducer
                     (state storage)
                     res)))))
             (do-keyvalue (key url (prop sources))
               (let ((currkey key))
                 (json-bind (res url ())
                    (let ((stor (create-from-list (list currkey res))))
                      (set-state
                       :storage
                       (funcall
                        reducer
                        (state storage)
                        stor))))))))
      default-reducer
      (lambda (existing incoming)
        incoming)
      get-initial-state (lambda () (create storage (create))))

    (def-component display-if
        (if (if (prop predicate)
                  (funcall (prop predicate) (@ this props))
                  (prop test))
            (prop children)
            null))

    (def-component error-boundary
        (if (state has-error)
            (psx (:h1 :style (create :text-color "red") "Wrongness has occurred!"))
            (prop children))
      get-initial-state
      (lambda ()
        (create 'has-error nil))
      component-did-catch
      (lambda (err errinfo)
        (say "Wrongness warning:")
        (say err)
        (say errinfo)
        (set-state has-error t)))

    ))
