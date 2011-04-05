(defpackage #:craftd
  (:use    #:common-lisp)
  (:export #:register #:unregister))

(in-package craftd)

(defparameter *event-callbacks* (make-hash-table))

(defun sort-callbacks (a b)
  (cond
    ((= (second a) (second b)) 0)
    ((< (second a) (second b)) 1)
    (t                    -1)))

(defmacro with-callbacks ((callbacks name) &body body)
  `(let ((,callbacks (gethash ,name *event-callbacks*)))
     (if (not ,callbacks)
         (setf ,callbacks nil))
     ,@body))

(defun register (name callback &optional priority)
  (with-callbacks (callbacks name)
    (setf (gethash name *event-callbacks*)
          (append (list callback priority) callbacks))))

(defun unregister (name &optional callback)
  (with-callbacks (callbacks name)
    (setf (gethash name *event-callbacks*)
          (if callback
            (remove-if #'(lambda (element) (equal (first element) callback)) callbacks)
            nil))))


