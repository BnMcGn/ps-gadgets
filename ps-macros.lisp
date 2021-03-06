;;;; ps-macros.lisp

(in-package #:ps-gadgets)

;;; "ps-gadgets" goes here. Hacks and glory await!

;;;FIXME: Should handle docstring
;;FIXME: Should include (ps ...) ?
;;DEPRECATED
(defmacro define-ps-lib (name (&rest params) &body body)
  "Define a function whose body will contain parenscript"
  `(defun ,name ,params
    ,@body))


(defmacro define-dual-code (name &body body)
  `(progn
     (defun ,name () (ps ,@body))
     ,@body))

(defmacro if-ps (ps-clause lisp-clause)
  (declare (ignore ps-clause)) lisp-clause)

(defpsmacro if-ps (ps-clause lisp-clause)
  (declare (ignore lisp-clause)) ps-clause)

(defmacro when-ps (&body code) (declare (ignore code)) nil)

(defpsmacro when-ps (&body code) code)

(defmacro unless-ps (&body code) code)

(defpsmacro unless-ps (&body code) (declare (ignore code)) nil)

(defpsmacro do-keyvalue ((key val obj) &body body)
  (let ((obj-v (gensym)))
    `(let ((,obj-v ,obj))
       (funcall (lambda ()
                  (unless (eql "object" (typeof ,obj-v))
                    (throw "Do-keyvalue needs an object"))
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

(defpsmacro dotree ((var-for-leaf/branch
                   tree
                   &key
                   result
                   (order :depth)
                   (proc-branch t)
                   (proc-leaf nil)
                   branch-filter
                   leaf-test)
                  &body body)
  `(progn
     (call-with-tree
      (lambda (,var-for-leaf/branch) ,@body)
      ,tree
      :order ,order :proc-branch ,proc-branch :proc-leaf ,proc-leaf :branch-filter
      ,branch-filter :leaf-test ,leaf-test)
     ,result))

(defpsmacro strcat (first &rest rest)
  `(chain ,first (concat ,@rest)))

(defpsmacro threeway (test minus-clause zero-clause plus-clause)
  (let ((val (gensym)))
    `(let ((,val ,test))
       (cond ((> 0 ,val) ,minus-clause)
             ((equal 0 ,val) ,zero-clause)
             (t ,plus-clause)))));end eval-always

(defpsmacro json-bind ((results url (&rest params) &key error-func) &body body)
  (let ((reqsym (gensym "request"))
        (urlsym (gensym "url")))
    `(let ((,reqsym (new -x-m-l-http-request))
           (,urlsym (set-url-parameters ,url (create ,@params))))
       (chain ,reqsym (open "GET" ,urlsym true))
       (setf (@ ,reqsym onload)
             (lambda ()
               (if (< 199 (@ this status) 400)
                   (let ((,results
                           (try (chain -j-s-o-n (parse (@ this response)))
                                (:catch (err) (say "JSON parse error")
                                        (say err)
                                        (say ,urlsym)) )))
                     ,@body)
                   ;;FIXME: This might need some touching up
                   ,@(when error-func `((funcall ,error-func))))))
       (setf (@ ,reqsym onerror) ,error-func)
       (chain ,reqsym (send)))))

(defpsmacro json-post-bind ((results url data &rest params) &body body)
  `(chain $ (ajax
             (create data-type "json" :url ,url :data ,data :type "POST"
                     :success (lambda (,results)
                                (let ((,results
                                       (if (equal (typeof ,results) "string")
                                           (chain -j-s-o-n (parse ,results))
                                           ,results)))
                                  ,@body))
                     :error (lambda (data status error)
                              (say (strcat "Ajax failure: "
                                           status
                                           " Error: "
                                           error)))
                     ,@params))))

(defpsmacro text-bind ((results url &rest params) &body body)
  `(chain $ (ajax
             (create data-type "text" :url ,url :type "GET"
                     :success (lambda (,results)
                                ,@body)
                     :error (lambda (data status error)
                              (say (strcat "Ajax failure: "
                                           status
                                           " Error: "
                                           error)))
                     ,@params))))

(defpsmacro json-if ((results url (&rest data) &key post (parse t)) success-clause error-clause)
  (let ((reqsym (gensym "request"))
        (urlsym (gensym "url"))
        (errsym (gensym "error_func")))
    `(let ((,reqsym (new -x-m-l-http-request))
           (,urlsym (set-url-parameters ,url (create ,@data)))
           (,errsym (lambda (,results) ,error-clause)))
       (chain ,reqsym (open ,(if post "POST" "GET") ,urlsym true))
       (setf (@ ,reqsym onload)
             (lambda ()
               (if (< 199 (@ this status) 400)
                   ,(if parse
                        `(try (let ((,results (chain -j-s-o-n (parse (@ this response)))))
                                ,success-clause)
                              (:catch (err) (funcall ,errsym err)))
                        `(let ((,results (@ this response)))
                           ,success-clause))
                   ;;FIXME: kind of crude
                   (funcall ,errsym this))))
       (setf (@ ,reqsym onerror) ,errsym)
       (chain ,reqsym (send)))))

(defpsmacro def-class (name (&optional extends) &body body)
  (multiple-value-bind (constructor others)
      (gadgets:splitfilter (lambda (x) (string-equal (car x) 'constructor)) body)
    (let ((constructor (case (length constructor)
                         (0 nil)
                         (1 (car constructor))
                         (otherwise
                          (error "Class can't have more than one constructor"))))
          (const-lambda-list nil)
          (const-body nil))
      (when constructor
        (setf const-lambda-list (second constructor))
        (setf const-body (cddr constructor)))
      `(progn
         (defun ,name ,const-lambda-list
           ,@const-body)
         ,@(mapcar
            (lambda (item)
              `(setf (@ ,name prototype ,(car item))
                     (lambda ,(second item) ,@(cddr item))))
            others)
         ,@(when extends
                 `((setf (@ ,name prototype) (chain -object (create (@ ,name prototype))))
                   (setf (@ ,name prototype constructor) ,name)))))))

(defpsmacro def-class-es6 (name (&optional extends) &body body)
  (let ((output nil))
    (push (format nil "class ~a~a {~&"
                  (symbol-to-js-string name)
                  (if extends
                      (format nil " extends ~a" (symbol-to-js-string extends))
                      ""))
          output)
    (dolist (itm body)
      (when (atom itm)
        (error "Non-list item found in class body."))
      (let ((staticp nil))
        (when (eq (car itm) :static)
          (setf itm (if (listp (cadr itm)) (cadr itm) (cdr itm)))
          (setf staticp t))
        (destructuring-bind (mname lambda-list . statements) itm
          (push
           (format
            nil "~a~a (~{~a~^ ~}) {~a}~&"
            (if staticp "static " "")
            (symbol-to-js-string mname)
            (mapcar #'symbol-to-js-string lambda-list)
            ;; Use parenscript to express a method body, then peel the
            ;; block contents out and put them in our own block.
            (let ((fbody (eval `(ps (lambda ,lambda-list ,@statements)))))
              (subseq fbody
                      (1+ (position #\{ fbody))
                      (position #\} fbody :from-end t))))
           output))))
    (push "}" output)
    `(lisp-raw ,(apply #'concatenate 'string (nreverse output)))))

