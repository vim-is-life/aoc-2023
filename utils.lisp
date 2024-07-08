(defun get-file-contents (filename)
  "Return a list of strings where each string line in the file FILENAME."
  (uiop:read-file-lines filename))

(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

;;; QUEUE ADT IMPLEMENTATION
(defclass queue ()
  ((head :initform nil :accessor qhead)))

(defmethod print-object ((q queue) stream)
  "Print representation of queue Q to STREAM."
  (format stream "#<QUEUE with items: (~{ ~A ~})>" (qhead q)))

(defmethod qempty? ((q queue))
  "Return non-nil if queue is empty, nil otherwise"
  (null (qhead q)))

(defmethod enqueue ((q queue) item)
  "Add ITEM to the end of the the queue Q"
  (with-slots (head) q
    (if (qempty? q)
        (setf head (list item))
        (nconc head (list item)))))

(defmethod dequeue ((q queue))
  (with-slots (head) q
    (unless (null head)
      (let ((retval (first head)))
        (setf head (rest head))
        retval))))

(defmethod qclear ((q queue))
  (loop :while (not (qempty? q))
        :do (dequeue q)))
