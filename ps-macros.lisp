;;;; ps-macros.lisp

(in-package #:ps-gadgets)

;;; "webhax" goes here. Hacks and glory await!

(defpsmacro do-keyvalue ((key val obj) &body body)
  (let ((obj-v (gensym)))
    `(let ((,obj-v ,obj))
       (funcall (lambda ()
                  (for-in (,key ,obj-v)
                          (if (chain ,obj-v (has-own-property ,key))
                              (let ((,val (getprop ,obj-v ,key)))
                                ,@body))))))))

(defpsmacro collecting-string (&body body)
  (let ((res (gensym)))
    `(let ((,res ""))
       (labels ((collect (itm)
                  (setf ,res (+ ,res itm))))
         ,@body)
       ,res)))

(defpsmacro collecting (&body body)
  (let ((res (gensym)))
    `(let ((,res (array)))
       (labels ((collect (itm)
                  (chain ,res (push itm))))
         ,@body)
       ,res)))

(defpsmacro collecting-set (&body body)
  (let ((res (gensym)))
    `(let ((,res (create)))
       (labels ((collect (itm)
                  (setf (getprop ,res itm) itm)))
         ,@body)
       (collecting (dolist (x (chain -object (keys ,res)))
                     (collect (getprop ,res x)))))))

(defpsmacro do-window ((var/s source
                              &key (size 2) (step 1)
                              (start-padding '(array))) &body body)
  (let ((size (if (listp var/s) (length var/s) size))
        (data (gensym))
        (i (gensym)))
    `(let ((,data (chain ,start-padding (concat ,source))))
       (dolist (,i (range 0 (1+ (- (length ,data) ,size)) ,step))
         ,(if (listp var/s)
              `(destructuring-bind ,var/s (chain ,data (slice ,i (+ ,i ,size)))
                 ,@body)
              `(let ((,var/s (chain ,data (slice ,i (+ ,i ,size)))))
                 ,@body))))))

(defpsmacro strcat (first &rest rest)
  `(chain ,first (concat ,@rest)))

(defpsmacro threeway (test minus-clause zero-clause plus-clause)
  (let ((val (gensym)))
    `(let ((,val ,test))
       (cond ((> 0 ,val) ,minus-clause)
             ((equal 0 ,val) ,zero-clause)
             (t ,plus-clause)))));end eval-always

(defpsmacro json-bind ((results url &rest params) &body body)
  `(chain $ (get-j-s-o-n ,url (create ,@params) (lambda (,results) ,@body))))

(defpsmacro json-post-bind ((results url data &rest params) &body body)
  `(chain $ (ajax (create data-type "json" :url ,url :data ,data :type "POST"
                          :success (lambda (,results) ,@body) ,@params))))
