(defsystem :joined
  :version "0.1")

(craftd:register :Server.start! #'(lambda ()
  (format t "Server ~a started ;)~%" (craftd:server-name))))

(craftd:register :Client.connect #'(lambda (client)
  (format t "Client ~a connected :>~%" (craftd:client-ip client))))
