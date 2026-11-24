;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes -*-
;;;
;;;   sbcl --non-interactive --eval '(asdf:test-system "clopt")'

(defpackage #:clopt-test
  (:use #:cl #:clopt)
  (:export #:run-tests))

(in-package #:clopt-test)

(defoption *test-mode* :fast
  :type (member :fast :careful)
  :doc "How carefully to work.")

(defoption *test-verbose* nil
  :type boolean
  :doc "Talk while working.")

(defoption *test-count* 3 :type (integer 0) :system :other-system)

(defvar *slot* 10)

(defun run-tests ()
  (let ((checks '()))
    (flet ((check (label ok) (push (cons label (and ok t)) checks)))
      (check "defoption defines the variable" (eq *test-mode* :fast))
      (check "  with its documentation"
             (equal (documentation '*test-mode* 'variable) "How carefully to work."))
      (check "the system defaults to the symbol's package"
             (eq :clopt-test (option-system (find-option '*test-mode*))))
      (check "  or is what :system says"
             (eq :other-system (option-system (find-option '*test-count*))))
      (check "list-options by system"
             (equal (mapcar #'option-name (list-options :system :clopt-test))
                    '(*test-mode* *test-verbose*)))
      (check "option-systems lists both"
             (subsetp '(:clopt-test :other-system) (option-systems)))
      (check "option-doc" (equal (option-doc '*test-verbose*) "Talk while working."))
      (check "member choices" (equal (option-choices '*test-mode*) '(:fast :careful)))
      (check "boolean choices" (equal (option-choices '*test-verbose*) '(nil t)))
      (check "an open type has no choices" (null (option-choices '*test-count*)))
      ;; setting
      (setf (option-value '*test-mode*) :careful)
      (check "setf option-value sets the variable" (eq *test-mode* :careful))
      (check "  and it is no longer the default" (not (option-default-p '*test-mode*)))
      (check "a value of the wrong type is refused, and nothing changes"
             (and (typep (nth-value 1 (ignore-errors (setf (option-value '*test-mode*) :sloppy)))
                         'invalid-option-value)
                  (eq *test-mode* :careful)))
      (reset-option '*test-mode*)
      (check "reset-option restores the default"
             (and (eq *test-mode* :fast) (option-default-p '*test-mode*)))
      ;; defvar semantics: re-evaluating keeps a set value
      (setf *test-count* 7)
      (eval '(defoption *test-count* 3 :type (integer 0) :system :other-system))
      (check "re-declaring keeps the value that was set" (= *test-count* 7))
      (check "  and registers it once"
             (= 1 (count '*test-count* (list-options) :key #'option-name)))
      ;; a value that is not a variable
      (register-option 'slot-option :type 'integer :default 10 :doc "In a slot."
                       :reader (lambda () *slot*)
                       :writer (lambda (v) (setf *slot* v)))
      (setf (option-value 'slot-option) 12)
      (check "a reader/writer option" (and (= *slot* 12) (= 12 (option-value 'slot-option))))
      (unregister-option 'slot-option)
      (check "unregister-option" (null (find-option 'slot-option nil)))
      (check "an unknown option is an error"
             (typep (nth-value 1 (ignore-errors (option-value 'no-such-option)))
                    'unknown-option))
      (check "a default not of the type is refused"
             (nth-value 1 (ignore-errors (register-option 'bad :type 'integer :default :x)))))
    (setf checks (reverse checks))
    (dolist (c checks)
      (format t "~&  ~:[FAIL <<<~;pass~]  ~a~%" (cdr c) (car c)))
    (let ((ok (every #'cdr checks)))
      (format t "~&clopt tests: ~:[FAILED~;passed~]~%" ok)
      ok)))
