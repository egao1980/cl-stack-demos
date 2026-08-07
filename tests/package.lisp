(defpackage #:cl-stack-demos/tests
  (:use #:cl #:rove #:cl-stack-demos))

(in-package #:cl-stack-demos/tests)

(deftest apps-registered
  (ok (>= (length (list-apps)) 10)))

(deftest smoke-object-tape
  (ok (eq :ok (run-app "object-tape"))))
