(in-package craftd)

(export '(#:player-ip))

(uffi:def-struct entity)

(uffi:def-struct player)

(defun player-ip (player)
  (client-ip (get-slot-value player 'player 'client)))
