(in-package :craftd)

(export '(create-string destroy-string string-content))

(uffi:def-function ("CD_CreateStringFromCStringCopy" c-create-string) ((data :cstring))
    :returning (* :void))

(uffi:def-function ("CD_DestroyString" c-destroy-string) ((self (* :void))))

(uffi:def-function ("CD_StringContent" c-string-content) ((self (* :void)))
    :returning :cstring)

(uffi:def-function ("CD_StringLength" c-string-length) ((self (* :void)))
  :returning :int)

(uffi:def-function ("CD_StringSize" c-string-size) ((self (* :void)))
  :returning :int)

(defun create-string (data)
  (c-create-string data))

(defun destroy-string (self)
  (c-destroy-string self))

(defun string-content (self)
  (uffi:convert-from-cstring (c-string-content self)))

(defun string-length (self)
  (c-string-length self))

(defun string-size (self)
  (c-string-size self))
