(defsystem :joined
  :version "0.1")

(craftd:register "Client.connect" #'(lambda (client)
    (let ((client (craftd:wrap client 'client)))
      (format t "~a~%" (craftd:client-ip client)))))
