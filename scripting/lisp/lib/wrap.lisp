(in-package craftd)

(export '(get-wrapped-slot))

(defun get-wrapped-slot (object attribute)
  (get-slot-value (first object) (second object) attribute))
