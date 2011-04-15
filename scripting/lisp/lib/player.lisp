(in-package craftd)

(export '(player-ip player-username))

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
  (client-ip (wrap (get-wrapped-value player 'client) 'client)))

(defun player-username (player)
  (string-content (get-wrapped-value player 'username)))
