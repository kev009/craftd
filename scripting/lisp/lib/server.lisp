(in-package :craftd)

(export '(server-name kick))

(uffi:def-struct timeloop)
(uffi:def-struct config)
(uffi:def-struct plugins)
(uffi:def-struct logger)

(uffi:def-struct server
    (timeloop (* :void))
    (config   (* :void)))

(uffi:def-function ("CD_ServerToString" c-server-name) ((self (* :void)))
                   :returning :cstring)

(uffi:def-function ("CD_ServerKick" c-server-kick) ((self (* :void)) (client (* :void)) (reason (* :void))))

(defun server-name ()
  (uffi:convert-from-cstring (c-server-name *server*)))

(defun kick (client reason)
  (let ((reason (create-string reason)))
        (c-server-kick *server* client reason)
        (destroy-string reason)))
