(defpackage :craftd
  (:use :common-lisp)
  (:export :register :unregister))

(in-package craftd)

(defparameter *event-callbacks* (make-hash-table))

(defun sort-callbacks (a b)
  (cond
    ((= (second a) (second b)) 0)
    ((< (second a) (second b)) 1)
    (t                        -1)))

(defun register (name callback &optional priority)
  (let ((callbacks (gethash name *event-callbacks*)))
    (if (not callbacks)
        (setf callbacks (list)))

    (setf (gethash name *event-callbacks*)
          (append (list callback priority) callbacks))))

(defun unregister (name &optional callback)
  (let ((callbacks (gethash name *event-callbacks*)))
    (if (not callbacks)
        (setf callbacks (list)))

    (setf (gethash name *event-callbacks*)
          (if callback
            (remove-if #'(lambda (element) (= (first element) callback)) callbacks)
            (list)))))
