(in-package :craftd)

(export '(dynamic-get dynamic-put dynamic-delete))

(uffi:def-function ("CD_HashGet" c-hash-get) ((self (* :void)) (name :cstring))
                   :returning (* :void))

(uffi:def-function ("CD_HashPut" c-hash-put) ((self (* :void)) (name :cstring) (value (* :void)))
                   :returning (* :void))

(uffi:def-function ("CD_HashDelete" c-hash-delete) ((self (* :void)) (name :cstring))
                   :returning (+ :void))
