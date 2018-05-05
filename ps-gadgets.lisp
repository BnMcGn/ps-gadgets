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

(def-ps-lib ps-gadgets ()
  (concatenate 'string
   (ps* *ps-lisp-library*)
   (ps

     (setf *whitespace-characters*
       (lisp (list* 'list gadgets:*whitespace-characters*)))

     (defun say (thing)
       (chain console (log thing))
       thing)

     (defun grab (thing)
       (if (@ window grabbed)
           (push thing (@ window grabbed))
           (setf (@ window grabbed) (list thing)))
       thing)

     ;;FIXME: Should check that isn't some other compound type. Else check if is
     ;; known atom type.
     (defun atom (itm)
       (not (arrayp itm))

     (defun arrayp (itm)
       (chain -array (is-array itm)))

     (defun ensure-array (arr)
       (cond
         ((or (equal (typeof arr) "undefined") (null arr))
          ([]))
         ((chain arr (has-own-property 'length))
          arr)
         (t ([] arr))))

     (defun ensure-string (str)
       (if (equal (typeof str) "string")
           str
           ""))

     (defun capitalize-first (str)
       (chain str (replace (regex "^[a-z]")
                           (lambda (first-char)
                             (chain first-char (to-upper-case))))))

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

     (defun flatten (tree)
       (collecting
           (mapleaves #'collect tree)))

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
       (and itm (< 0 (if (eq undefined (@ itm length))
                         (chain -object (keys itm) length)
                         (@ itm length)))))

     (defun first-match (predicate input-list)
       (let ((res nil)
             (sig nil))
         (dolist (x input-list)
           (when (funcall predicate x)
             (setf res x)
             (setf sig t)
             (break)))
         (values res sig)))

     (defun boolify (val)
       ;;FIXME: Should be using case insensitive test
       (when val
         (if (member val '(0 "false" "False" "NIL" "nil" "null" "Null" "No" "no"))
             nil
             t)))

     (defun identity (x) x)

     (defun funcall (func &rest params)
       (apply func params))

     (defun relative-to-range (start end num)
       "Returns a value indicating where num is positioned relative to start and end. If num lies between start and end, the return value will be between 0.0 and 1.0."
       (/ (- num start) (- end start)))

     (defun as-in-range (start end num)
       "Complement of relative-of-range function. Treats num as if it were a fraction of the range specified by start and end. Returns the absolute number that results."
       (+ start (* num (- end start))))

     (let ((counter 0))
       (defun unique-id ()
         (incf counter)))

     (defun list-last (list-obj)
       (when (and (chain list-obj (has-own-property 'length))
                  (< 0 (@ list-obj length)))
         (@ list-obj (- (@ list-obj length) 1))))

     (defun create-from-list (keys-and-values)
       (let ((res (create)))
         (do-window ((k v) keys-and-values :step 2)
           (setf (aref res k) v))
         res))

     ;;Tools for non-destructive editing. Useful for redux.

     (defun shallow-copy (obj)
       (let ((res (create)))
         (do-keyvalue (k v obj)
           (setf (aref res k) v))
         res))

     (defun copy-merge-objects (base addition)
       "Makes a copy of base, overwrites key values in base with those found
        in addition. Silently ignores extra keys in addition."
       (let ((res (create)))
         (do-keyvalue (k v base)
           (setf (getprop res k)
                 (if (chain addition (has-own-property k))
                     (getprop addition k)
                     (getprop base k))))
         res))

     (defun copy-merge-all (&rest objects)
       (let ((res (create)))
         (dolist (ob objects)
           (do-keyvalue (k v ob)
             (setf (getprop res k) v)))
         res))

     (defun copy-remove-keys (obj keys)
       (let ((res (shallow-copy obj)))
         (dolist (k keys)
           (delete (getprop res k)))
         res))

     (defun safe-set-copy (arr &rest key/values)
       "Creates copy of arr, setting keys to values in the copy only if they
        exist in the original."
       (let ((res (create)))
         (do-window ((k v) key/values :step 2)
           (unless (chain arr (has-own-property k))
             (throw "Tried to set non-existent key in safe-set-copy"))
           (setf (getprop res k) v))
         (copy-merge-objects arr res)))

     (defun set-copy (arr &rest key/values)
       "Creates copy of arr, setting key to value in the copy regardless of
        whether it exists in the original."
       (let ((res (shallow-copy arr)))
         (do-window ((k v) key/values :step 2)
           (setf (getprop res k) v))
         res))

     (defun copy-updates (orig changes)
       "Creates a copy of changes that only contains things that are
       different from orig."
       (let ((res (create)))
         (do-keyvalue (k v changes)
           (if (chain orig (has-own-property k))
               (unless (equal (getprop orig k) v)
                 (setf (getprop res k) v))
               (setf (getprop res k) v)))
         res))


     (defun incf-copy (arr key &optional (value 1))
       (safe-set-copy arr key (+ (getprop arr key) value)))

     (defun decf-copy (arr key &optional (value 1))
       (safe-set-copy arr key (- (getprop arr key) value)))

     (defun inc-or-set-copy (arr key &optional (value 1))
       (if (chain arr (has-own-property key))
           (incf-copy arr key value)
           (set-copy arr key value)))

     (defun get-path (arr path)
       (if (> 1 (chain path (length)))
           (get-path (getprop arr (car path)) (cdr path))
           (getprop arr (car path))))

     (defun deep-set-copy (arr path value)
       (case (@ path length)
         (0
          (throw "Shouldn't have zero length path"))
         (1
          (safe-set-copy arr (elt path 0) value))
         (otherwise
          (safe-set-copy
           arr (elt path 0)
           (deep-set-copy (getprop arr (elt path 0))
                          (chain path (slice 1)) value)))))

     (defun push-copy (alist val)
       (collecting
           (collect val)
         (dolist (itm (ensure-array alist))
           (collect itm))))

     (defvar *tree-leaf-p* nil "Is the node being processed a leaf or a branch?")
     (defvar *tree-stack* nil "Pointers to the parentage of the current node. Includes current node.")
     (defvar *tree-index-stack* nil "Indices that lead to the address of the current node.")
     (defvar *tree-process-branches* t)
     (defvar *tree-process-leaves* t)
     (defvar *tree-breadth-first* nil)
     (defvar *tree-leaf-test* nil)
     (defvar *tree-branch-filter* nil)

     (defun %proc-branch (branch exec)
       (collecting
           (let ((stor nil))
             (loop
                for item-tmp in branch
                for i from 0
                do
                  (let* ((*tree-stack* (push-copy *tree-stack* item-tmp))
                         (*tree-index-stack* (push-copy *tree-index-stack* i))
                         (item item-tmp)
                         (ts *tree-stack*)
                         (tis *tree-index-stack*))
                    (if (funcall *tree-leaf-test* item)
                        (when *tree-process-leaves*
                          (collect
                              (let ((*tree-leaf-p* t))
                                (lambda ()
                                  (let ((*tree-stack* ts)
                                        (*tree-index-stack* tis))
                                    (funcall exec item)
                                    nil)))))
                        (progn
                          (when *tree-process-branches*
                            (collect
                                (let ((*tree-leaf-p* nil))
                                  (lambda ()
                                    (let ((*tree-stack* ts)
                                          (*tree-index-stack* tis))
                                      (funcall exec item)
                                      nil)))))
                          ;;FIXME: prev will want to be able to effect how and if of
                          ;; execution of following.
                          (let ((sub
                                 (lambda ()
                                   (let ((*tree-stack* ts)
                                         (*tree-index-stack* tis))
                                     (let ((res (%proc-branch
                                                 (funcall
                                                  (or *tree-branch-filter* #'identity)
                                                  item)
                                                 exec)))
                                       (mapcar #'funcall (chain res (slice 0 -1)))
                                       (getprop res -1))))))
                            (if *tree-breadth-first*
                                (chain stor (push sub)) ;; Store to execute at end
                                (collect sub))))))) ;; Execute as found
             (collect stor))))

     ;;FIXME: Doesn't support switching between depth and breadth first mid-tree.
     (defun %handle-proc-branch-tail (tail)
       (collecting
         (dolist (item tail)
           (dolist (res (funcall item))
             (when (functionp res) (collect res))))))

     (defun call-with-tree (func
                            tree
                            &key
                              (order :depth)
                              (proc-branch t)
                              proc-leaf
                              branch-filter
                              leaf-test)
       (unless (member order '(:depth :breadth))
         (error "Order must be :depth or :breadth"))
       (let ((*tree-process-branches* proc-branch)
             (*tree-process-leaves* proc-leaf)
             (*tree-breadth-first* (eq order :breadth))
             (*tree-leaf-test* (or leaf-test #'atom))
             (*tree-branch-filter* branch-filter))
         (let ((res (%proc-branch
                     (funcall (or *tree-branch-filter* #'identity) tree) func)))
           (mapcar #'funcall (chain res (slice 0 -1)))
           (loop
              with items = (getprop res -1)
              while items
              do (setf items (%handle-proc-branch-tail items))))))

         ))); End ps-gadgets

(defun alist->ps-object-code (alist &key (wrap t))
  (let ((res
   (gadgets:collecting
       (dolist (item alist)
         (gadgets:collect (car item))
         (gadgets:collect (cdr item))))))
    (if wrap (cons 'ps:create res) res)))

