(in-package craftd)

(uffi:def-struct chunk-position
    (x :int)
    (z :int))

(uffi:def-struct block-position
    (x :int)
    (y :int)
    (z :int))

(uffi:def-struct absolute-position
    (x :int)
    (y :int)
    (z :int))

(uffi:def-struct precise-position
    (x :double)
    (y :double)
    (z :double))

(uffi:def-struct relative-position
    (x :char)
    (y :char)
    (z :char))

(uffi:def-foreign-type entity-id :int)

(uffi:def-enum entity-type (:player :pickup :mob :object))

(uffi:def-struct entity
    (id       entity-id)
    (type     entity-type)
    (position precise-position))

(uffi:def-foreign-type error-type :int)
