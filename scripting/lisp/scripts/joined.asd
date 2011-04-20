(defsystem :joined
  :version "0.1")

(craftd:register :Player.login #'(lambda (player status)
  (format t "~s (~s) joined the game~%" (craftd:player-username player) '(craftd:player-ip player))))

(craftd:register :Player.logout #'(lambda (player status)
  (format t "~s (~s) left the game~%" (craftd:player-username player) '(craftd:player-ip player))))
