;;;; ps-react-gadgets.lisp

(defpackage #:ps-react-gadgets
  (:use #:cl #:parenscript #:reacl #:ps-lib-tool #:ps-gadgets))

(in-package #:ps-react-gadgets)

(def-ps-package ps-react-gadgets
  :ps-requirements '(reacl)
  :code
  (ps

    (defun clone-children (children props)
      (collecting
        (dolist (child (ensure-array children))
          (collect
              (react clone-element child props)))))

    (def-component update-notify
        ((set-state :callbacks []
                    :olddispatch (@ props children props dispatch)))
      (defun render ()
        (clone-children (prop children) (create :dispatch (@ this new-dispatch))))
      (defun new-dispatch (action)
        ;;FIXME: Review callback storage. Maybe should delete old.
        (if (eq (@ action type) 'set-callback)
            (set-state callbacks
                       (list (@ action 'callback)))
                                        ; (chain (state callbacks)
                                        ;        (append (list (@ action 'callback)))))
            (when (state :olddispatch)
              (funcall (state :olddispatch) action))))
      (defun component-will-receive-props ()
        (dolist (x (state callbacks))
          (funcall x))))

    ;;Props: sources, reducer, store-name
    (def-component json-loader
        ((set-state :storage (create) :done nil))
      (defun render ()
        (if (or (not (prop wait)) (and (prop wait) (state done)))
            (clone-children (prop children)
                            (if
                             (prop store-name)
                             (create-from-list (list (prop store-name) (state storage)))
                             (state storage)))
            null))
      (defun component-did-mount ()
        (let ((reducer (or (prop reducer) (@ this default-reducer))))
          (if (arrayp (prop sources))
              (dolist (url (prop sources))
                (json-bind (res url () :error-func (lambda () (@ this status)))
                           (set-state
                            storage
                            (funcall
                             reducer
                             (state storage)
                             res)
                            :done t)))
              (do-keyvalue (key url (prop sources))
                (let ((currkey key))
                  ;;FIXME: done/wait not implemented for multiple source loader
                  (json-bind (res url ())
                             (let ((stor (create-from-list (list currkey res))))
                               (set-state
                                :storage
                                (funcall
                                 reducer
                                 (state storage)
                                 stor)))))))))
      (defun default-reducer (existing incoming)
        incoming))

    (def-component prop-to-context
      nil
      (defun render ()
        (let ((provider (@ (prop context) -provider)))
          (psx (:provider
                :value (getprop (propsref) (prop propname))
                (collecting
                  (dolist (child (ensure-array (prop children)))
                    (collect
                        (react clone-element child
                               (copy-remove-keys (propsref) '(:context :propname)))))))))))

    (def-component display-if
      nil
      (defun render ()
        (if (if (prop predicate)
                (funcall (prop predicate) (@ this props))
                (prop test))
            (prop children)
            null)))

    (def-component error-boundary
        ((set-state :has-error nil))
      (defun render ()
        (if (state has-error)
            (psx (:h1 :style (create :text-color "red") "Wrongness has occurred!"))
            (prop children)))
      (defun component-did-catch (err errinfo)
        (say "Wrongness warning:")
        (say err)
        (say errinfo)
        (set-state has-error t)))

    ))
