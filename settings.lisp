;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes -*-
;;;
;;; SETTINGS -- the values of a system's options, saved to a file and loaded
;;; back.
;;;
;;; A settings file records how a configuration DIFFERS from the defaults: it
;;; holds only the options that are not at their default value. Loading it sets
;;; those, and (by default) puts every other registered option back to its
;;; default -- so loading a file always gives the same configuration, whatever
;;; was set before, and a default that changes in the code reaches every file
;;; that did not set that option.
;;;
;;; THE FILE is Lisp DATA, never code. It is read with *READ-EVAL* off, one
;;; entry per form:
;;;
;;;   ;;; clopt settings, saved 2026-11-24 16:40
;;;   (:clopt-settings 1)
;;;   ("CONCEPTUAL-GRAPHS" "*ALIGNMENT-STRATEGY*" :individuals)
;;;   ("CONCEPTUAL-GRAPHS" "*GRAPH-REFERENT-INDENT*" 6)
;;;
;;; An option is named by package and symbol name as strings, so reading a file
;;; interns nothing in any system's package, and an entry for a system that is
;;; not loaded is simply skipped (and kept, if the file is saved over). A value
;;; that is not of its option's type is refused and the option left as it was.
;;;
;;; NAMED SETS live in *SETTINGS-DIRECTORY*, one NAME.settings file each. The
;;; functions here take either a name (a string: letters, digits, space, - and
;;; _) or a pathname.

(in-package #:clopt)

(defvar *settings-directory* nil
  "The directory named settings sets live in, as a pathname or a namestring
ending in /; NIL until a host system sets it. Not created until a set is saved
there.")

(define-condition settings-error (error)
  ((message :initarg :message :reader settings-error-message))
  (:report (lambda (c s) (write-string (settings-error-message c) s))))

(defun %settings-error (fmt &rest args)
  (error 'settings-error :message (apply #'format nil fmt args)))


;;; --- names and files -------------------------------------------------------

(defun settings-name-p (name)
  "True when NAME can name a settings set: a non-empty string of letters,
digits, space, - and _, not beginning or ending with a space. Nothing else, so
a name can never reach outside *SETTINGS-DIRECTORY*."
  (and (stringp name)
       (plusp (length name))
       (char/= #\Space (char name 0))
       (char/= #\Space (char name (1- (length name))))
       (every (lambda (c) (or (alphanumericp c) (find c " -_"))) name)))

(defun %settings-directory ()
  (let ((dir *settings-directory*))
    (unless dir
      (%settings-error "No settings directory: set CLOPT:*SETTINGS-DIRECTORY* first."))
    (let ((s (if (pathnamep dir) (namestring dir) dir)))
      (pathname (if (and (plusp (length s)) (char= #\/ (char s (1- (length s)))))
                    s
                    (concatenate 'string s "/"))))))

(defun settings-path (name)
  "The file of the settings set NAME in *SETTINGS-DIRECTORY*."
  (unless (settings-name-p name)
    (%settings-error "~s is not a settings name: use letters, digits, space, - and _." name))
  (merge-pathnames (make-pathname :name name :type "settings") (%settings-directory)))

(defun %settings-file (designator)
  (etypecase designator
    (pathname designator)
    (string (settings-path designator))))

(defun settings-names ()
  "The names of the sets in *SETTINGS-DIRECTORY*, sorted; NIL when there is no
directory or no set."
  (when *settings-directory*
    (sort (remove-if-not #'settings-name-p
                         (mapcar #'pathname-name
                                 (directory (merge-pathnames
                                             (make-pathname :name :wild :type "settings")
                                             (%settings-directory)))))
          #'string<)))

(defun settings-exist-p (designator)
  "True when the settings set or file DESIGNATOR exists."
  (and (probe-file (%settings-file designator)) t))

(defun delete-settings (designator)
  "Delete the settings set or file DESIGNATOR. The values it holds are lost."
  (let ((path (%settings-file designator)))
    (unless (probe-file path)
      (%settings-error "There is no settings file ~a." (namestring path)))
    (delete-file path)
    t))


;;; --- reading and writing ---------------------------------------------------

(defmacro %with-settings-io (&body body)
  "BODY with the reader and printer set for settings files: keywords print with
their colon, symbols are read into CLOPT-IO and nowhere else, #. is refused."
  `(with-standard-io-syntax
     (let ((*package* (find-package '#:clopt-io))
           (*read-eval* nil)
           (*print-case* :downcase)
           (*print-pretty* nil))
       ,@body)))

(defun %plain-string (string)
  "STRING as an ordinary string of CHARACTERs. Package and symbol names may be
BASE-STRINGs, which SBCL prints readably as #A((17) base-char . \"...\") -- it
reads back, but a settings file is meant to be read by people too."
  (let ((plain (make-string (length string) :element-type 'character)))
    (replace plain string)))

(defun %option-key (option)
  (let ((name (option-name option)))
    (list (%plain-string (package-name (symbol-package name)))
          (%plain-string (symbol-name name)))))

(defun %entry-option (entry)
  "The registered option ENTRY (package name value) sets, or NIL."
  (find-if (lambda (o) (equal (%option-key o) (list (first entry) (second entry))))
           *options*))

(defun read-settings (designator)
  "The entries of the settings set or file DESIGNATOR: a list of
(package-name symbol-name value). Signals SETTINGS-ERROR for a file that is
not a settings file or does not read."
  (let ((path (%settings-file designator)))
    (unless (probe-file path)
      (%settings-error "There is no settings file ~a." (namestring path)))
    (with-open-file (in path)
      (%with-settings-io
        (handler-case
            (let ((header (read in nil :eof)))
              (unless (and (consp header) (eq (first header) :clopt-settings))
                (%settings-error "~a is not a settings file." (namestring path)))
              (unless (eql (second header) 1)
                (%settings-error "~a is settings version ~s; this clopt reads version 1."
                                 (namestring path) (second header)))
              (loop for entry = (read in nil :eof)
                    until (eq entry :eof)
                    unless (and (consp entry) (= 3 (length entry))
                                (stringp (first entry)) (stringp (second entry)))
                      do (%settings-error "~a: ~s is not a settings entry."
                                          (namestring path) entry)
                    collect entry))
          (settings-error (e) (error e))
          (error (e)
            (%settings-error "~a does not read as settings: ~a" (namestring path) e)))))))

(defun save-settings (designator &key all (replace t))
  "Save the current values of the registered options to the settings set or
file DESIGNATOR and return its pathname. Only options that differ from their
default are saved, unless ALL. Entries already in the file for options that
are not registered now (a system not loaded) are kept. With REPLACE NIL, an
existing file is a SETTINGS-ERROR rather than overwritten. A value that cannot
be printed readably is a SETTINGS-ERROR, and nothing is written."
  (let* ((path (%settings-file designator))
         (exists (probe-file path)))
    (when (and exists (not replace))
      (%settings-error "A settings set ~a already exists." (namestring path)))
    (let* ((current (loop for o in (list-options)
                          when (or all (not (option-default-p o)))
                            collect (append (%option-key o) (list (option-value o)))))
           (kept (when exists
                   (remove-if #'%entry-option (ignore-errors (read-settings path)))))
           (texts (%with-settings-io
                    (let ((*print-readably* t))
                      (mapcar (lambda (entry)
                                (handler-case (prin1-to-string entry)
                                  (print-not-readable ()
                                    (%settings-error "~a::~a holds ~a, which cannot be saved: it does not print readably."
                                                     (first entry) (second entry)
                                                     (let ((*print-readably* nil))
                                                       (prin1-to-string (third entry)))))))
                              (append current kept))))))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede)
        (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
          (format out ";;; -*- Mode: LISP; Syntax: Common-lisp -*-~%~
                       ;;; clopt settings, saved ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d.~%~
                       ;;; Lisp DATA, read with *read-eval* off: (package option value).~%~
                       ;;; Only options that differ from their defaults are listed; loading~%~
                       ;;; puts every other option back to its default.~%"
                  y mo d h m s))
        (format out "(:clopt-settings 1)~%~{~a~%~}" texts))
      path)))


;;; --- what loading does -----------------------------------------------------

(defun settings-plan (designator &key (reset t))
  "What loading the settings set or file DESIGNATOR would do, without doing it.
Three values:
  CHANGES  ((option current-value new-value) ...) -- every option whose value
           would change. With RESET, an option the file does not list goes back
           to its default; without, it is left alone.
  UNKNOWN  ((package-name symbol-name value) ...) -- entries for options that
           are not registered (their system is not loaded); skipped.
  REFUSED  ((option value) ...) -- entries whose value is not of the option's
           type; the option is left as it is."
  (let ((entries (read-settings designator))
        (changes '()) (unknown '()) (refused '()) (listed '()))
    (dolist (entry entries)
      (let ((option (%entry-option entry)))
        (cond ((null option) (push entry unknown))
              ((not (typep (third entry) (option-type option)))
               (push (list option (third entry)) refused)
               (push option listed))
              (t
               (push option listed)
               (unless (equal (third entry) (option-value option))
                 (push (list option (option-value option) (third entry)) changes))))))
    (when reset
      (dolist (option (list-options))
        (unless (or (member option listed) (option-default-p option))
          (push (list option (option-value option) (option-default option)) changes))))
    (values (sort changes #'string< :key (lambda (c) (string (option-name (first c)))))
            (nreverse unknown)
            (nreverse refused))))

(defun load-settings (designator &key (reset t))
  "Load the settings set or file DESIGNATOR: set every option it lists and,
with RESET (the default), put every other registered option back to its
default. Returns the three values of SETTINGS-PLAN, describing what was done."
  (multiple-value-bind (changes unknown refused) (settings-plan designator :reset reset)
    (dolist (change changes)
      (setf (option-value (first change)) (third change)))
    (values changes unknown refused)))
