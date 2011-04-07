(in-package craftd)

(export '(player-ip))

(uffi:def-struct player
    (entity entity)

    (client (* :void))
    (world  (* :void))

    (yaw   :float)
    (pitch :float)

    (username (* :void))

    (dynamic (* :void))
    (error   error-type))

(defun player-ip (player)
  (let ((player (wrap player 'player)))
      (client-ip (get-wrapped-slot player 'client))))
