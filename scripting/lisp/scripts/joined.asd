(defsystem :joined
  :version "0.1")

(craftd:register :Server.start! #'(lambda ()
  (format t "~s~%" craftd:server-name)))

(craftd:register :Client.connect #'(lambda (client)
  (format t "~a~%" (craftd:client-ip client))))
