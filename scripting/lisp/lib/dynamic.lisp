(in-package :craftd)

(export '(dynamic-get dynamic-put dynamic-delete))

(uffi:def-function ("CD_HashGet" c-hash-get) ((self (* :void)) (name :cstring))
                   :returning (* :void))

(uffi:def-function ("CD_HashPut" c-hash-put) ((self (* :void)) (name :cstring) (value (* :void)))
                   :returning (* :void))

(uffi:def-function ("CD_HashDelete" c-hash-delete) ((self (* :void)) (name :cstring))
                   :returning (+ :void))

(defun dynamic-get (object name)
  (uffi:with-cstring (c-name name)
    (c-hash-get (get-wrapped-value object 'dynamic) name)))

(defun dynamic-set (object name value)
  (uffi:with-cstring (c-name name)
    (c-hash-set (get-wrapped-value object 'dynamic) name value)))

(defun dynamic-delete (object name)
  (uffi:with-cstring (c-name name)
    (c-hash-delete (get-wrapped-value object 'dynamic) name)))
