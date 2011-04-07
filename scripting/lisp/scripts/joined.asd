(defsystem :joined
  :version "0.1")

(format t "Server: ~a~%" (craftd:server-name))

(craftd:register "Client.connect" (lambda (client)
    (let ((client (craftd:wrap-client client)))
        (format t "~a~%" (craftd:client-ip client)))))
