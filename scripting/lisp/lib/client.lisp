(defpackage :craftd
  (:use :common-lisp)
  (:export :register :unregister))

(in-package craftd)

(uffi:def-struct client
    (server  :pointer)
    (ip      (:array :char 128))
    (socket  :int)
    (buffers :pointer)
    (status  :int)
    (jobs    :char))

(defun client-ip (client)
  (convert-from-cstring (get-slot-value client 'client 'ip)))
