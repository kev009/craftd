(defpackage :craftd
  (:use :common-lisp)
  (:export :string :create-string :destroy-string :string-content))

(in-package craftd)

(uffi:def-struct string (length :int))

(uffi:def-function ("CD_CreateStringFromCStringCopy" c-create-string) ((data (* :char)))
    :returning :pointer)

(uffi:def-function ("CD_DestroyString" c-destroy-string) ((self :pointer)))

(uffi:def-function ("CD_StringContent" c-string-content) ((self :pointer))
    :returning (* :char))

(defun create-string (data) (c-create-string data))

(defun destroy-string (self) (c-destroy-string self))

(defun string-content (self) (c-string-content self))
