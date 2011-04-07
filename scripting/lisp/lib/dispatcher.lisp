(in-package craftd)

(export '(register unregister))

(defparameter *event-callbacks* (make-hash-table))

(defun sort-callbacks (a b)
  (cond
    ((= (second a) (second b)) 0)
    ((< (second a) (second b)) 1)
    (t                    -1)))

(defun register (name callback &optional priority)
  (let ((callbacks (gethash name *event-callbacks*)))
    (setf (gethash name *event-callbacks*)
      (append callbacks (list (list callback priority)) callbacks))))

(defun unregister (name &optional callback)
  (let ((callbacks (gethash name *event-callbacks*)))
    (setf (gethash name *event-callbacks*)
          (if callback
            (remove-if #'(lambda (element) (equal (first element) callback)) callbacks)
            nil))))
