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
    (settings-checks (lambda (label ok) (push (cons label (and ok t)) checks)))
    (setf checks (reverse checks))
    (dolist (c checks)
      (format t "~&  ~:[FAIL <<<~;pass~]  ~a~%" (cdr c) (car c)))
    (let ((ok (every #'cdr checks)))
      (format t "~&clopt tests: ~:[FAILED~;passed~]~%" ok)
      ok)))


;;; --- settings: saving and loading values ------------------------------------

(defvar *test-holder* nil)
(register-option '*test-holder* :type t :default nil
                 :system :clopt-test-holder)

(defun %fresh-settings-directory ()
  (merge-pathnames (format nil "clopt-test-~d-~d/" (get-universal-time) (random 100000))
                   (uiop:temporary-directory)))

(defun %write-file (path &rest lines)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dolist (l lines) (write-line l out))))

(defun settings-checks (check)
  (let ((*settings-directory* (%fresh-settings-directory)))
    (unwind-protect
         (progn
           (funcall check "settings names: letters, digits, space, - and _"
                    (and (settings-name-p "work") (settings-name-p "my set-2_b")
                         (not (settings-name-p "../x")) (not (settings-name-p "a/b"))
                         (not (settings-name-p "")) (not (settings-name-p " a"))
                         (not (settings-name-p "a.b"))))
           (funcall check "a bad name never becomes a path"
                    (typep (nth-value 1 (ignore-errors (settings-path "../../etc"))) 'settings-error))
           (mapc #'reset-option (list-options))
           ;; save what differs from the defaults
           (setf (option-value '*test-mode*) :careful
                 (option-value '*test-count*) 9)
           (save-settings "one")
           (funcall check "save lists only what differs from the defaults"
                    (equal (read-settings "one")
                           '(("CLOPT-TEST" "*TEST-MODE*" :careful)
                             ("CLOPT-TEST" "*TEST-COUNT*" 9))))
           (funcall check "  as plain text: (\"CLOPT-TEST\" \"*TEST-MODE*\" :careful)"
                    (let ((text (with-open-file (in (settings-path "one"))
                                  (let ((s (make-string (file-length in))))
                                    (subseq s 0 (read-sequence s in))))))
                      (and (search "(\"CLOPT-TEST\" \"*TEST-MODE*\" :careful)" text)
                           (not (search "#A" text)))))
           (funcall check "settings-names finds it" (equal (settings-names) '("one")))
           (funcall check "saving over it needs :replace"
                    (typep (nth-value 1 (ignore-errors (save-settings "one" :replace nil)))
                           'settings-error))
           ;; load: listed options set, the rest back to their defaults
           (mapc #'reset-option (list-options))
           (setf (option-value '*test-verbose*) t)
           (funcall check "the plan says what loading would change, and changes nothing"
                    (and (= 3 (length (settings-plan "one")))
                         (eq *test-mode* :fast)))
           (load-settings "one")
           (funcall check "load sets what the file lists"
                    (and (eq *test-mode* :careful) (= *test-count* 9)))
           (funcall check "  and puts the rest back to their defaults" (null *test-verbose*))
           (setf (option-value '*test-verbose*) t)
           (load-settings "one" :reset nil)
           (funcall check ":reset nil leaves the rest alone" (eq *test-verbose* t))
           (funcall check "loading what is already set changes nothing"
                    (null (settings-plan "one" :reset nil)))
           ;; entries a file may hold
           (%write-file (settings-path "odd")
                        "(:clopt-settings 1)"
                        "(\"NO-SUCH-SYSTEM\" \"*THING*\" 1)"
                        "(\"CLOPT-TEST\" \"*TEST-MODE*\" :sloppy)")
           (multiple-value-bind (changes unknown refused) (load-settings "odd" :reset nil)
             (funcall check "an option of a system not loaded is skipped and reported"
                      (equal unknown '(("NO-SUCH-SYSTEM" "*THING*" 1))))
             (funcall check "a value not of the type is refused, the option left as it is"
                      (and (= 1 (length refused)) (null changes) (eq *test-mode* :careful))))
           (save-settings "odd")
           (funcall check "saving over a file keeps the entries of systems not loaded"
                    (member '("NO-SUCH-SYSTEM" "*THING*" 1) (read-settings "odd") :test #'equal))
           (%write-file (settings-path "evil")
                        "(:clopt-settings 1)"
                        "(\"CLOPT-TEST\" \"*TEST-COUNT*\" #.(setf clopt-test::*test-holder* :evaluated))")
           (funcall check "a file is data: #. is refused and nothing is evaluated"
                    (and (typep (nth-value 1 (ignore-errors (load-settings "evil"))) 'settings-error)
                         (null *test-holder*)))
           (%write-file (settings-path "notes") "(defun hello () 1)")
           (funcall check "a file that is not a settings file is refused"
                    (typep (nth-value 1 (ignore-errors (read-settings "notes"))) 'settings-error))
           (setf (option-value '*test-holder*) (make-hash-table))
           (funcall check "a value that does not print readably is refused, nothing written"
                    (and (typep (nth-value 1 (ignore-errors (save-settings "table"))) 'settings-error)
                         (not (settings-exist-p "table"))))
           (setf (option-value '*test-holder*) nil)
           (delete-settings "one")
           (funcall check "delete-settings" (not (settings-exist-p "one")))
           (let ((*settings-directory* nil))
             (funcall check "no settings directory: no names, and saving is an error"
                      (and (null (settings-names))
                           (typep (nth-value 1 (ignore-errors (save-settings "x"))) 'settings-error)))))
      (mapc #'reset-option (list-options))
      (uiop:delete-directory-tree (pathname *settings-directory*)
                                  :validate t :if-does-not-exist :ignore))))
