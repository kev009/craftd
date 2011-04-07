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
    (push (list callback priority) callbacks)))

(defun unregister (name &optional callback)
  (let ((callbacks (gethash name *event-callbacks*)))
    (if callback
      (clrhash callbacks)
      (remhash callback callbacks))))
