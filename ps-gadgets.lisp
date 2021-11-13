;;;; ps-gadgets.lisp

(in-package #:ps-gadgets)

;;; "ps-gadgets" goes here. Hacks and glory await!

(defparameter *js-second* 1000)
(defparameter *js-minute* (* 60 *js-second*))
(defparameter *js-hour* (* 60 *js-minute*))
(defparameter *js-day* (* 24 *js-hour*))
(defparameter *js-week* (* 7 *js-day*))
(defparameter *js-month* (* 30 *js-day*)) ;Ok, things start to get weird.
(defparameter *js-year* (* 365 *js-day*))

(def-ps-package ps-lisp-library
  :code (ps* *ps-lisp-library*)
  :export '(#:mapcar #:map-into #:map #:member #:set-difference #:reduce #:nconc))

(def-ps-package ps-gadgets
  :ps-requirements '(ps-lisp-library)
  :code
  (ps

    (setf *whitespace-characters*
          (lisp (list* 'list gadgets:*whitespace-characters*)))

    (defun say (thing)
      (chain console (log thing))
      thing)

    (defun grab (thing)
      (if (@ window grabbed)
          (chain window grabbed (push thing))
          (setf (@ window grabbed) (list thing)))
      thing)

    ;;FIXME: Should check that isn't some other compound type. Else check if is
    ;; known atom type.
    (defun atom (itm)
      (not (arrayp itm)))

    (defun arrayp (itm)
      (chain -array (is-array itm)))

    (defun numberp (itm)
      (equal "number" (typeof itm)))

    (defun integerp (itm)
      (chain -number (is-integer itm)))

    (defun string-object-p (itm)
      (and (equal (typeof itm) "object")
           (instanceof itm -string)))

    (defun string-object-to-literal (obj)
      (chain obj (to-string)))

     (defun string-literal-p (itm)
       (equal "string" (typeof itm)))

    (defun stringishp (itm)
      (or (string-literal-p itm) (string-object-p itm)))

    (defun ensure-string-literal (itm)
      (cond
        ((stringp itm) itm)
        ((string-object-p itm) (string-object-to-literal itm))
        (t (throw "Not a string type"))))

     (defun ensure-array (arr)
       (cond
         ((or (equal (typeof arr) "undefined") (null arr))
          ([]))
         ((chain arr (has-own-property 'length))
          arr)
         (t ([] arr))))

    (defun js-string-equal (a b)
      (cond
        ((eq a b) t)
        ((or (not (stringishp a)) (not (stringishp b))) nil)
        ((equal (ensure-string-literal a) (ensure-string-literal b)) t)))

    (defun has-property (obj key)
      (chain obj (has-own-property key)))

    ;;FIXME: maybe should use version from lodash
    (defun array-equal (a b)
      (cond
        ((eq a b) t)
        ((or (not a) (not b)) false)
        ((not (eq (@ a length) (@ b length))) false)
        (t (progn
             (dotimes (i (@ a length))
               (when (not (equal (getprop a i) (getprop b i)))
                 (return false)))
             t))))

    (defun get/d (obj key value)
      (if (eq (typeof (getprop obj key)) "undefined")
          value
          (getprop obj key)))

    (defun array-cdr (arr)
      (chain (ensure-array arr) (slice 1)))

    (defun unique (arr &key (test (lambda (a b) (eql a b))))
      (chain arr (filter (lambda (val ind self)
                           (eq (chain self (find-index (lambda (itm) (test val itm)))) ind)))))

    (defun ensure-string (str)
      (if (equal (typeof str) "string")
          str
          ""))

    (defun copy-string (str)
      (chain (+ " " str) (slice 1)))

    (defun capitalize-first (str)
      (chain str (replace (regex "^[a-z]")
                          (lambda (first-char)
                            (chain first-char (to-upper-case))))))

    (defun newline ()
      "Because parenscript doesn't allow \n"
      (chain -string (from-char-code 10)))

    (defun remove-if (test arr)
      (collecting
        (dolist (itm (ensure-array arr))
          (unless (funcall test itm)
            (collect itm)))))

    (defun remove-if-not (test arr)
      (collecting
        (dolist (itm (ensure-array arr))
          (when (funcall test itm)
            (collect itm)))))

     (defun some (pred first-seq &rest more-seqs)
       (loop for i from 0
             for itm in first-seq
             for res = (apply pred itm (mapcar (lambda (x) (elt x i)) more-seqs))
             until res
             finally (return res)))

     (defun range (start &optional stop (step 1))
       (let ((start (if stop start 0))
             (stop (if stop stop start)))
         (let ((stop-p (if (> step 0) (lambda (x) (< x stop))
                           (lambda (x) (> x stop)))))
           (if (or (and (> step 0) (>= start stop)
                        (and (< step 0) (<= start stop))))
             ([])
             (collecting
               (loop while (funcall stop-p start)
                 do (collect start)
                 do (incf start step)))))))

     (defun position-difference (element1 element2)
       (let ((pos1 (chain element1 (get-bounding-client-rect)))
             (pos2 (chain element2 (get-bounding-client-rect))))
         (create top (- (@ pos1 top) (@ pos2 top))
                 left (- (@ pos1 left) (@ pos2 left))
                 bottom (- (@ pos1 bottom) (@ pos2 bottom))
                 right (- (@ pos1 right) (@ pos2 right)))))

     (defun mapleaves (fn tree &key test)
       "Map a one-argument function FN over each leaf node of the TREE
   in depth-first order, returning a new tree with the same structure."
       (labels ((rec (node)
                  (if (and (atom node) (or (not test) (funcall test node)))
                      (funcall fn node)
                      (mapcar #'rec node))))
         (when tree
           (rec tree))))

     (defun try-awhile (predicate &key (sleep 1) (wait 1000) on-success on-fail)
       "Will continue to call predicate until either it returns success or a given amount of time elapses. Duration can be set with the :wait keyword. It defaults to 1000 milliseconds. Try-awhile will sleep between predicate calls unless the :sleep keyword is set to nil. Default sleep is 100 milliseconds.

Try-awhile will return the predicate value on success or nil on failure. If a function is supplied to the :on-success argument, it will be executed if the predicate succeeds and its result will be returned instead of the predicate result. The :on-fail keyword may be used to supply a function that will be run if the time elapses without a predicate success. It's result will be returned instead of the default nil."
       ;;FIXME: Needs to return? a promise
       (let ((start-time (chain -date (now))))
         (defun func ()
           (let ((result (funcall predicate)))
             (if result
                 (if on-success
                     (funcall on-success)
                     result)
                 (if (< (+ start-time wait) (chain -date (now)))
                     (if on-fail
                         (funcall on-fail)
                         nil)
                     (set-timeout func sleep)))))
         (func)))

     (defun random-element (arr)
       (getprop arr (chain -math (floor (* (chain -math (random)) (@ arr length))))))

    (defun flatten (tree)
      (collecting
        (labels ((proc (itm)
                   (if (atom itm)
                       (collect itm)
                       (dolist (i itm)
                         (proc i)))))
          (proc tree))))

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

     (defun url-domain (url)
       ;;FIXME: Maybe we should error out if we get a bad URL
       (let* ((part1 (or (chain url (split "//") 1) ""))
              (part2 (chain part1 (split "/") 0)))
         (if (equal "www." (chain part2 (substring 0 4)))
             (chain part2 (slice 4))
             part2)))

     (defun url-parameters-location (url)
       (let ((start (chain url (index-of "?")))
             (end (chain url (index-of "#"))))
         (when (eq -1 start) (setf start undefined))
         (when (eq -1 end) (setf end undefined))
         (when (and end start (> start end)) (setf start undefined))
         (when start (incf start))
         (list start end)))

     (defun url-parameters (url)
       (let* ((res (create))
              (loc (url-parameters-location url))
              (parstring (when (or (@ loc 0) (@ loc 1))
                             (chain url (slice (@ loc 0) (@ loc 1))))))
         (when parstring
           (dolist (par (chain parstring (split "&")))
            (let ((pair (chain par (split "="))))
              (setf (getprop res (decode-u-r-i-component (@ pair 0)))
                    (decode-u-r-i-component (@ pair 1))))))
         res))

     (defun url-parameter-string-from-object (obj)
       (chain
        (collecting
          (do-keyvalue (k v obj)
            (collect (+ (encode-u-r-i-component k) "=" (encode-u-r-i-component v)))))
        (join "&")))

     (defun replace-url-parameters (url new-param-string)
       (if (< 0 (@ new-param-string length))
           (let* ((loc (url-parameters-location url))
                  (tail (if (@ loc 1)
                            (chain url (slice (@ loc 1)))
                            ""))
                  (head (chain url (slice undefined (if (@ loc 0) (1- (@ loc 0)) (@ loc 1))))))
             (+ head (if (@ loc 0) "" "?") new-param-string tail))
           url))

     (defun set-url-parameters (url obj)
       "Return a copy of url with the keys in obj set as parameters. Overwrite parameters in url when names are shared, but retain other parameters from url."
       (let ((existing (url-parameters url)))
         (do-keyvalue (k v obj)
           (setf (getprop existing k) v))
         (replace-url-parameters url (url-parameter-string-from-object existing))))

     (defun update-url-parameter (url param value)
       (let* ((parts (chain url (split "#")))
              (base (getprop parts 0))
              (anchor (getprop parts 1))
              (parts (chain base (split "?")))
              (base (getprop parts 0))
              (params (if (getprop parts 1)
                          (chain (getprop parts 1) (split "&"))
                          (list)))
              (parstring (+ param "=" value))
              (found nil)
              (params
               (mapcar
                (lambda (par)
                  (let ((kv (chain par (split "="))))
                    (if (equal (getprop kv 0) param)
                        (progn
                          (setf found t)
                          parstring)
                        par)))
                params))
              (params
               (if found
                   params
                   (chain params (concat (list parstring))))))
         (+
          base
          "?"
          (chain params (join "&"))
          (if anchor (+ "#" anchor) ""))))

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

     (defun array-equal (arr1 arr2)
       (when (eq (@ arr1 length) (@ arr2 length))
         (dotimes (i (@ arr1 length))
           (unless (eq (getprop arr1 i) (getprop arr2 i))
             (return-from array-equal nil)))
         t))

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
           (setf (getprop res k) v))
         res))

     ;;Based on code from https://developers.google.com/web/fundamentals/primers/promises
     (defun json-promise (url)
       (new (-promise
             (lambda (resolve reject)
               (let ((req (new (-x-m-l-http-request))))
                 (chain req (open "GET" url))
                 (setf (@ req onload)
                       (lambda ()
                         (if (eql 200 (@ req status))
                             (funcall resolve
                                      (chain -j-s-o-n (parse (@ req response))))
                             (funcall reject (-error (@ req status-text))))))
                 (setf (@ req onerror)
                       (lambda ()
                         (funcall reject (-error "Network Error"))))
                 (chain req (send)))))))

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
           (when ob
             (do-keyvalue (k v ob)
               (setf (getprop res k) v))))
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
             (dotimes (i (@ branch length))
               (let* ((*tree-stack* (push-copy *tree-stack* (getprop branch i)))
                      (*tree-index-stack* (push-copy *tree-index-stack* i))
                      (item (getprop branch i))
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
       (cl-utilities:collecting
         (dolist (item tail)
           (dolist (res (funcall item))
             (when (functionp res) (cl-utilities:collect res))))))

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

         )); End ps-gadgets

(defun alist->ps-object-code (alist &key (wrap t))
  (let ((res
   (cl-utilities:collecting
       (dolist (item alist)
         (cl-utilities:collect (car item))
         (cl-utilities:collect (cdr item))))))
    (if wrap (cons 'ps:create res) res)))

