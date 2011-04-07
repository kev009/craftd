(in-package craftd)

(export '(wrap get-wrapped-slot))

(defun wrap (object struct)
  (if (equal (type-of object) 'cons)
    object
    (list object struct)))

(defun get-wrapped-object (object)
  (first object))

(defun get-wrapped-slot (object attribute)
  (get-slot-value (first object) (second object) attribute))
