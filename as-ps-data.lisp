;;;; as-ps-data.lisp

(in-package #:ps-gadgets)

(defparameter *assume-list* nil
  "Tell as-ps-data to not interpret lists as alists even if they look like them.")

(defgeneric as-ps-data (item)
  (:documentation
   "Convert a tree of lisp data into the equivalent tree of Parenscript/Javascript data."))

;;It might be safer to not have this: That way we find out if something odd is being sent.
;;(defmethod as-ps-data ((item t)) item)

(defmethod as-ps-data ((item string)) item)
(defmethod as-ps-data ((item number)) item)
(defmethod as-ps-data ((item symbol)) (ps:symbol-to-js-string item))
(defmethod as-ps-data ((item null)) nil)

(defmethod as-ps-data ((item list))
  (cond
    ((not item) nil)
    ((and (not *assume-list*) (trivial-types:association-list-p item))
      (list* 'create (mapcan (lambda (x) (list (as-ps-data (car x)) (as-ps-data (cdr x)))) item)))
    ;;Plists are too vague. Convert to hash or alist.
    ;;((trivial-types:property-list-p item)
    ;;(list* 'create (mapcar #'as-ps-data item))))
    (t (list* 'list (mapcar #'as-ps-data item)))))

(defmethod as-ps-data ((item hash-table))
  (list* 'create (gadgets:collecting
                   (maphash (lambda (k v)
                              (gadgets:collect (as-ps-data k))
                              (gadgets:collect (as-ps-data v))) item))))

