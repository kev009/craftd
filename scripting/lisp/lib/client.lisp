(in-package craftd)

(export '(client-ip))

(uffi:def-struct client
    (server  (* :void))
    (ip      (:array :char 128))
    (socket  :int)
    (buffers (* :void))
    (status  :int)
    (jobs    :char))

(defun client-ip (client)
  (convert-from-cstring (get-wrapped-slot client 'ip)))
