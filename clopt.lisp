;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes -*-
;;;
;;; CLOPT -- options a person may set, declared once.
;;;
;;; A Lisp system has many special variables and only a few of them are
;;; options. cgraph has about ninety earmuffed variables; a dozen are things a
;;; person should set, and the rest is internal state that breaks the system if
;;; edited. Nothing in the language tells the two apart, so a settings window
;;; built from the variables would show all ninety. DEFOPTION is the line
;;; between them:
;;;
;;;   (clopt:defoption *alignment-strategy* :automatic
;;;     :type (member :automatic :individuals)
;;;     :doc "How two graphs being combined decide which concepts are ...")
;;;
;;; defines the variable (DEFVAR, so reloading keeps a value you set) and
;;; registers it with its type and documentation. A front end -- a web page,
;;; Emacs Customize -- then lists a system's options, shows each docstring,
;;; offers the allowed values, and sets them through OPTION-VALUE, which refuses
;;; a value that is not of the option's type.
;;;
;;; WHAT IS NOT AN OPTION. A variable a function binds for its own purposes is
;;; not one, however settable it looks: setting it globally does nothing to the
;;; function that binds it, and a settings window that let you would mislead.
;;;
;;; A VALUE THAT IS NOT A VARIABLE. An option whose value lives elsewhere (a
;;; slot of some object, a symbol macro over one) is registered with REGISTER-
;;; OPTION and a :READER / :WRITER pair instead of DEFOPTION, which would try
;;; to DEFVAR it.
;;;
;;; Portable Common Lisp: no implementation-specific code.

(in-package #:clopt)

(defclass option ()
  ((name    :initarg :name    :reader option-name
            :documentation "The option's symbol -- the variable it names.")
   (system  :initarg :system  :reader option-system
            :documentation "A keyword naming the Lisp system it belongs to.")
   (type    :initarg :type    :reader option-type
            :documentation "A type specifier every value must satisfy.")
   (default :initarg :default :reader option-default
            :documentation "The value RESET-OPTION restores.")
   (doc     :initarg :doc     :initform nil
            :documentation "Documentation; NIL means the variable's own.")
   (reader  :initarg :reader  :initform nil
            :documentation "Function of no arguments returning the value, or
NIL to read the symbol's global value.")
   (writer  :initarg :writer  :initform nil
            :documentation "Function of one argument setting the value, or
NIL to set the symbol's global value.")))

(defmethod print-object ((option option) stream)
  (print-unreadable-object (option stream :type t)
    (format stream "~s ~s" (option-system option) (option-name option))))

(defvar *options* '()
  "Every registered option, most recently registered first.")


;;; --- conditions ------------------------------------------------------------

(define-condition unknown-option (error)
  ((name :initarg :name :reader unknown-option-name))
  (:report (lambda (c s)
             (format s "~s is not a registered option." (unknown-option-name c)))))

(define-condition invalid-option-value (error)
  ((option :initarg :option :reader invalid-option-value-option)
   (value  :initarg :value  :reader invalid-option-value-value))
  (:report (lambda (c s)
             (let ((o (invalid-option-value-option c)))
               (format s "~s is not a valid value for ~s, which must be of type ~s."
                       (invalid-option-value-value c) (option-name o) (option-type o))))))


;;; --- declaring -------------------------------------------------------------

(defun %system-of (symbol)
  "The default system of an option named SYMBOL: its package's name, as a keyword."
  (intern (package-name (symbol-package symbol)) :keyword))

(defun register-option (name &key (system (%system-of name)) (type t) default
                                  doc reader writer)
  "Register NAME as an option and return the OPTION. Registering a name again
replaces the earlier registration, so reloading a file is harmless. Use this
directly only for an option that is not a variable (give :READER and :WRITER);
DEFOPTION is the usual way."
  (check-type name symbol)
  (unless (typep default type)
    (error "The default ~s of option ~s is not of its type ~s." default name type))
  (let ((option (make-instance 'option :name name :system system :type type
                                       :default default :doc doc
                                       :reader reader :writer writer)))
    (setf *options* (cons option (remove name *options* :key #'option-name)))
    option))

(defun unregister-option (name)
  "Forget option NAME. The variable, if any, is left as it is."
  (setf *options* (remove name *options* :key #'option-name))
  nil)

(defmacro defoption (name default &key (type t) doc system)
  "Define the special variable NAME (as DEFVAR: an existing value is kept) and
register it as an option of SYSTEM (by default, NAME's package as a keyword).
TYPE is a type specifier every value must satisfy -- (MEMBER ...) lists the
choices, BOOLEAN a switch. DOC becomes the variable's documentation too.
DEFAULT is evaluated once more, at registration, to record what RESET-OPTION
restores, so it should be a constant or a side-effect-free form."
  `(progn
     (defvar ,name ,default ,@(when doc (list doc)))
     (register-option ',name :type ',type :default ,default
                             ,@(when system (list :system system)))
     ',name))


;;; --- finding ---------------------------------------------------------------

(defun find-option (name &optional (errorp t))
  "The OPTION registered as NAME. When there is none: an UNKNOWN-OPTION error,
or NIL if ERRORP is false."
  (or (find name *options* :key #'option-name)
      (and errorp (error 'unknown-option :name name))))

(defun %option (name-or-option)
  (if (typep name-or-option 'option) name-or-option (find-option name-or-option)))

(defun list-options (&key system)
  "The registered options, ordered by system and then by name; only SYSTEM's
when it is given."
  (sort (remove-if-not (lambda (o) (or (null system) (eq system (option-system o))))
                       (copy-list *options*))
        (lambda (a b)
          (let ((sa (string (option-system a))) (sb (string (option-system b))))
            (if (string= sa sb)
                (string< (string (option-name a)) (string (option-name b)))
                (string< sa sb))))))

(defun option-systems ()
  "The systems that have registered options, sorted by name."
  (sort (remove-duplicates (mapcar #'option-system *options*))
        #'string< :key #'string))


;;; --- describing ------------------------------------------------------------

(defun option-doc (option)
  "OPTION's documentation: its own, or else the variable's."
  (let ((o (%option option)))
    (or (slot-value o 'doc)
        (documentation (option-name o) 'variable))))

(defun option-choices (option)
  "The values OPTION allows when its type lists them -- (MEMBER a b ...), or
BOOLEAN as (NIL T) -- else NIL, meaning \"any value of its type\"."
  (let ((type (option-type (%option option))))
    (cond ((eq type 'boolean) (list nil t))
          ((and (consp type) (eq (first type) 'member)) (copy-list (rest type)))
          ((and (consp type) (eq (first type) 'or))
           ;; (or null (member ...)) and the like: choices only if every arm has them
           (let ((arms (mapcar (lambda (arm)
                                 (cond ((eq arm 'null) (list nil))
                                       ((and (consp arm) (eq (first arm) 'member)) (rest arm))
                                       ((eq arm 'boolean) (list nil t))
                                       (t :open)))
                               (rest type))))
             (unless (member :open arms)
               (remove-duplicates (apply #'append arms) :from-end t))))
          (t nil))))


;;; --- reading and writing ---------------------------------------------------

(defun option-value (option)
  "OPTION's current value (the variable's global value, or its :READER's)."
  (let ((o (%option option)))
    (if (slot-value o 'reader)
        (funcall (slot-value o 'reader))
        (symbol-value (option-name o)))))

(defun (setf option-value) (value option)
  "Set OPTION to VALUE, which must be of its type; INVALID-OPTION-VALUE if not,
and nothing is changed. Sets the GLOBAL value: a function that binds the
variable itself does not see it, which is why such a variable is not an option."
  (let ((o (%option option)))
    (unless (typep value (option-type o))
      (error 'invalid-option-value :option o :value value))
    (if (slot-value o 'writer)
        (funcall (slot-value o 'writer) value)
        (setf (symbol-value (option-name o)) value))
    value))

(defun reset-option (option)
  "Set OPTION back to its default and return it."
  (let ((o (%option option)))
    (setf (option-value o) (option-default o))))

(defun option-default-p (option)
  "True when OPTION holds its default value (compared with EQUAL)."
  (let ((o (%option option)))
    (equal (option-value o) (option-default o))))
