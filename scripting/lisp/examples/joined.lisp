(format t "lol~%")

(craftd:register "Client.connect" (lambda (client)
    (format t "~a" (craftd:client-ip client))))
