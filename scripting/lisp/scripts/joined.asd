(defsystem :joined
  :version "0.1")

(craftd:register :Client.connect #'(lambda (client)
  (format t "~a connected~%" (craftd:client-ip client))))

(craftd:register :Player.login #'(lambda (player status)
  (format t "~a joined the game~%" (craftd:player-username player))))
