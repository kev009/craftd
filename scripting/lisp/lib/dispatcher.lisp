(in-package craftd)

(export '(register unregister))

(defparameter *event-callbacks* (make-hash-table))

(defun get-callbacks (name)
  (gethash name *event-callbacks*))

(defun set-callbacks (name callbacks)
  (setf (gethash name *event-callbacks*) callbacks))

(defun sort-callbacks (a b)
  (cond
    ((= (second a) (second b)) 0)
    ((< (second a) (second b)) 1)
    (t                        -1)))

(defun register (name callback &optional priority)
  (let ((callbacks (get-callbacks name)))
    (set-callbacks name (sort (append (list (list callback (or priority 0))) callbacks) #'sort-callbacks))))

(defun unregister (name &optional callback)
  (let ((callbacks (get-callbacks name)))
    (set-callbacks name (if callback
      (remove-if #'(lambda (element) (equal (first element) callback)) callbacks)
      nil))))
