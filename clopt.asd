;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes -*-

(defsystem "clopt"
  :description "Declared, documented, typed options for Common Lisp systems:
DEFOPTION defines a special variable and registers it, so a front end (a web
page, Emacs Customize) can list a system's options, show their documentation,
and set them safely."
  :author "Bruce Seely"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "clopt")
               (:file "settings"))
  :in-order-to ((test-op (test-op "clopt/tests"))))

(defsystem "clopt/tests"
  :depends-on ("clopt")
  :components ((:file "test/clopt-test"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :clopt-test :run-tests)
               (error "clopt tests failed"))))
