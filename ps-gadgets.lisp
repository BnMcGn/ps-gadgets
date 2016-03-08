;;;; ps-gadgets.lisp

(in-package #:ps-gadgets)

;;; "webhax" goes here. Hacks and glory await!

(defparameter *js-second* 1000)
(defparameter *js-minute* (* 60 *js-second*))
(defparameter *js-hour* (* 60 *js-minute*))
(defparameter *js-day* (* 24 *js-hour*))
(defparameter *js-week* (* 7 *js-day*))
(defparameter *js-month* (* 30 *js-day*)) ;Ok, things start to get wierd.
(defparameter *js-year* (* 365 *js-day*))

(defun ps-gadgets ()
  (strcat
   (compile-script *ps-lisp-library*)
   (ps
     (defun say (thing)
       (chain console (log thing))
       thing)

     (defun atom (itm)
       (chain -array (is-array itm)))

     (defun ensure-array (arr)
       (cond
         ((or (equal (typeof arr) "undefined") (null arr))
          ([]))
         ((chain arr (has-own-property 'length))
          arr)
         (t ([] arr))))

     (defun remove-if-not (test arr)
       (collecting
         (dolist (itm (ensure-array arr))
           (when (funcall test itm)
             (collect itm)))))

     (defun range (start &optional stop (step 1))
       (let ((start (if stop start 0))
             (stop (if stop stop start)))
         (let ((stop-p (if (> step 0) (lambda (x) (< x stop))
                           (lambda (x) (> x stop)))))
           (if (or (and (> step 0) (>= start stop)
                        (and (< step 0) (<= start stop))))
             ([])
             (collecting
               (while (funcall stop-p start)
                 (collect start)
                 (incf start step)))))))

     (defun position-difference (element1 element2)
       (let ((pos1 (chain element1 (get-bounding-client-rect)))
             (pos2 (chain element2 (get-bounding-client-rect))))
         (create top (- (@ pos1 top) (@ pos2 top))
                 left (- (@ pos1 left) (@ pos2 left)))))

     (defun mapleaves (fn tree &key test)
       "Map a one-argument function FN over each leaf node of the TREE
   in depth-first order, returning a new tree with the same structure."
       (labels ((rec (node)
                  (if (and (atom node) (or (not test) (funcall test node)))
                      (funcall fn node)
                      (mapcar #'rec node))))
         (when tree
           (rec tree))))

     (defun ago (date-obj)
       (let ((diff (- (chain -date (now)) date-obj)))
         (create
          get-years (lambda () (parse-int (/ diff (lisp *js-year*))))
          get-months (lambda () (parse-int (/ diff (lisp *js-month*))))
          get-weeks (lambda () (parse-int (/ diff (lisp *js-week*))))
          get-days (lambda () (parse-int (/ diff (lisp *js-day*))))
          get-hours (lambda () (parse-int (/ diff (lisp *js-hour*))))
          get-minutes (lambda () (parse-int (/ diff (lisp *js-minute*))))
          get-seconds (lambda () (parse-int (/ diff (lisp *js-second*)))))))

     (defun not-empty (itm)
       (and itm (< 0 (@ itm length))))

     (defun shallow-copy (obj)
       (let ((res (create)))
         (do-keyvalue (k v obj)
           (setf (aref res k) v))
         res))

         ))); End ps-gadgets

(defun alist->ps-object-code (alist &key (wrap t))
  (let ((res
   (collecting
       (dolist (item alist)
         (collect (car item))
         (collect (cdr item))))))
    (if wrap (cons 'ps:create res) res)))

