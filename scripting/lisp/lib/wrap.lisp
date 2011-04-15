(in-package craftd)

(export '(wrap get-wrapped-value get-wrapped-pointer))

(defun wrap (object struct)
  (if (equal (type-of object) 'cons)
    object
    (list object struct)))

(defun get-wrapped-object (object)
  (first object))

(defun get-wrapped-value (object attribute)
  (uffi:get-slot-value (first object) (second object) attribute))

(defun get-wrapped-pointer (object attribute)
  (uffi:get-slot-pointer (first object) (second object) attribute))
