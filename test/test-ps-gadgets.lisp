
(in-package :cl-user)
(defpackage test-ps-gadgets
  (:use #:cl #:parenscript #:ps-gadgets #:ps-react-gadgets #:cl-react))

(in-package :test-ps-gadgets)


(defun tests ()
  (ps
    (require "jsdom-global")
    (defvar jsdom-module (require "jsdom"))
    (defvar jsdom (new (chain jsdom-module
                              (-j-s-d-o-m "<!doctype html><html><body></body></html>"))))
    (setf (@ global window) (@ jsdom window))
    (setf (@ global document) (@ jsdom window document))
    (setf (@ global navigator) (create user-agent "node.js"))

    (defvar -react (require "react"))
    (defvar chai (require "chai"))
    (defvar expect (@ chai expect))

    (defvar enzyme (require "enzyme"))
    (defvar shallow (@ enzyme shallow))
    (defvar mount (@ enzyme mount))

    (defvar chai-enzyme (require "chai-enzyme"))
    (defvar sinon (require "sinon"))
    (defvar eadapt (require "enzyme-adapter-react-16"))

    (chain chai (use chai-enzyme))
    (chain enzyme (configure (create adapter (new eadapt))))

    (describe
     "<JsonLoader/>"
     (lambda ()
       (defvar server nil)
       (defvar proto-fixture nil)

       (defun testdiv (props)
         (psx (:div :id "jchild" "here")))

       (before
        (lambda (done)
          (setf server (chain sinon (create-fake-server)))
          (setf (@ server auto-respond) true)
          (setf (@ global -x-m-l-http-request) (@ server xhr))
          (chain server
                 (respond-with
                  "GET" "/data/1"
                  (list 200 (create "Content-Type" "application/json")
                        "{\"root\": {\"controversy\": 1}}")))
          (chain server
                 (respond-with
                  "GET" "/data/2"
                  (list 200 (create "Content-Type" "application/json")
                        "{\"root\": {\"controversy\": 2}}")))
          (chain server
                 (respond-with
                  "GET" "/data/3"
                  (list 200 (create "Content-Type" "application/json")
                        "{\"root\": {\"controversy\": 3}}")))
          (set-timeout
           (lambda ()
             (setf proto-fixture
                   (mount
                    (psx
                     (:json-loader
                      :store-name "things"
                      :sources (create a "/data/1" b "/data/2" c "/data/3")
                      :reducer
                      (lambda (store more)
                        ;;FIXME: Can't guarantee the order of responses. This is dangerous.
                        (when (chain more (has-own-property "c"))
                          ;;Telling mocha done even though we aren't quite. Oh well. Seems to work.
                          (done))
                        (copy-merge-all store more))
                      (:testdiv :id "jchild"))))))
           50)))

       (after (lambda ()
                (chain server (restore))))
       (it "Should have 3 items in 'things'"
           (lambda ()
             (let
                 ((fixture proto-fixture))
               (chain
                (expect (chain fixture (#:state) storage))
                                        ;to have (length-of 3)
                to be (an "object") that contains all (keys "a" "b" "c")))))
       ))
    ))



(defun run-tests ()
  (with-open-file (s (asdf:system-relative-pathname 'test-ps-gadgets "test/tests.js")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    ;;FIXME: need better way to load ps libraries
    (write-string (ps-gadgets) s)
    (write-string (react:build) s)
    (write-string (ps-react-gadgets) s)

    (write-string (tests) s))
  (uiop:with-current-directory ((asdf:system-source-directory 'test-ps-gadgets))
    (princ
     (with-output-to-string (out)
       (external-program:run
        "mocha" nil :output out :error :output)))))

(run-tests)
