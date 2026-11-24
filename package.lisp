;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes -*-

(defpackage #:clopt
  (:use #:cl)
  (:export
   ;; declaring
   #:defoption #:register-option #:unregister-option
   ;; finding
   #:find-option #:list-options #:option-systems
   ;; describing
   #:option #:option-name #:option-system #:option-type #:option-default
   #:option-doc #:option-choices
   ;; reading and writing
   #:option-value #:reset-option #:option-default-p
   #:invalid-option-value #:invalid-option-value-option
   #:invalid-option-value-value
   #:unknown-option #:unknown-option-name
   ;; settings: saving and loading values
   #:*settings-directory* #:settings-name-p #:settings-path #:settings-names
   #:settings-exist-p #:delete-settings #:read-settings
   #:save-settings #:load-settings #:settings-plan
   #:settings-error #:settings-error-message))

(defpackage #:clopt-io
  (:use #:cl)
  (:documentation "Where settings files are read and printed: a symbol in a
file is interned here and nowhere else, and keywords print with their colon."))
