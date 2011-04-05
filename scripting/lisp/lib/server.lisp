(in-package :craftd)

(export '(server-name kick))

(uffi:def-struct timeloop)
(uffi:def-struct config)
(uffi:def-struct plugins)
(uffi:def-struct logger)

(uffi:def-struct server
    (timeloop (:pointer))
    (config (:pointer)))

(uffi:def-function ("CD_ServerToString" c-server-name) ((self :pointer))
                   :returning :cstring)

(uffi:def-function ("CD_ServerKick" c-server-kick) ((self :pointer) (client :pointer) (reason :pointer)))

(defun server-name ()
  (c-server-name *server*))

(defun kick (client reason)
  (let ((reason (create-string reason)))
        (c-server-kick *server* client reason)
        (destroy-string reason)))
