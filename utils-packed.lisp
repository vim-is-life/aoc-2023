(defpackage aoc-2023
  (:use :cl :arrow-macros)
  (:import-from :alexandria)
  (:import-from :str)
  (:import-from :uiop))

(in-package :aoc-2023)

(defun fact (n)
  "Return the factorial of N."
  (labels ((recur (n acc)
             (if (zerop n)
                 acc
                 (recur (1- n) (* acc n)))))
    (recur n 1)))

(defun combinations (n r)
  "Return the number of combinations *with no repitition* where N is the number
of things to choose from and R is how many we choose."
  (/ (fact n)
     (* (fact r)
        (fact (- n r)))))

(defun get-file-contents (filename)
  "Return a list of strings where each string line in the file FILENAME."
  (uiop:read-file-lines filename))

(defun get-file-contents-vec (filename)
  "Return a vector of strings where each string line in the file FILENAME."
  (coerce (uiop:read-file-lines filename) 'vector))

;; this doesn't really work the way i think it should, so i'll just use
;; alexandria for now
;; (defun curry (function &rest args)
;;   (lambda (&rest more-args)
;;     (apply function (append args more-args))))

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
