;;;; Program to solve    Advent of Code 2023's Day 5 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 10 Dec 2023

(load "~/quicklisp/setup.lisp")
(load "../utils.lisp")

(defun parse-input (filename)
  (let* ((lines (uiop:read-file-lines filename)))
    (map 'simple-vector (lambda (line) (map 'simple-vector #'parse-integer (uiop:split-string line)))
         lines)))

(defparameter *puzzle-input* (parse-input "input.txt"))
(defparameter *example-input* (parse-input "example.txt"))
(defparameter *example-row-1* #(0 3 6 9 12 15))
(defparameter *example-row-2* #(1 3 6 10 15 21))
(defparameter *example-row-3* #(10  13  16  21  30  45))

;;; part 1: find the sum of each next value in each sequence
;; (defparameter *example-row* #(-3 -10 -4 30 107 242 450 746 1145 1662 2312 3110 4071 5210 6542 8082 9845 11846 14100 16622 19427))

(defun find-differences (vec)
  "Return a list containing the differences between elements in VEC"
  (loop for idx from 1 below (length vec)
        collect (let* ((x1  (aref vec (1- idx)))
                       (x2 (aref vec idx))
                       (delta-x (- x2 x1)))
                  delta-x)))

(defun find-all-differences (vec)
  (loop
    for difference-list = (list (coerce (find-differences vec) 'simple-vector)) then (push differences difference-list)
    for differences = (coerce (find-differences (car difference-list)) 'simple-vector)
    until (every #'zerop differences)
    ;; note: we cons vec with list of differences to make sure we pass the
    ;; original seq back to the caller for easy finding of the next value just
    ;; with what we give them
    finally (return (cons vec (nreverse difference-list)))))

(defun find-next-value-in-original-seq (differences-list)
  "Return the expected next value in the original sequence, where the original
sequence is the CAR of DIFFERENCES-LIST, the next value of DIFFERENCES-LIST are
the first differences, etc"
  (let ((last-values (mapcar (lambda (vec) (aref vec (1- (length vec)))) differences-list)))
    (reduce #'+ last-values)))

(defun solve-part-one ()
  (let ((next-values-in-sequences (map 'simple-vector
                                       (lambda (vec)
                                         (find-next-value-in-original-seq
                                          (find-all-differences vec)))
                                       *puzzle-input*)))
    (reduce #'+ next-values-in-sequences)))

;; (solve-part-one)
;; => 1993300041 (31 bits, #x76CF5849)

;;; part 2: find the sum of each new first value in each sequence
(defun find-new-first-value-in-original-seq (differences-list)
  "Return the expected next value in the original sequence, where the original
sequence is the CAR of DIFFERENCES-LIST, the next value of DIFFERENCES-LIST are
the first differences, etc"
  (labels ((get-new-first (lst)
             (if (endp (cdr lst))
                 (car lst)
                 (- (car lst) (get-new-first (cdr lst))))))
    (let ((first-values (mapcar (lambda (vec) (aref vec 0)) differences-list)))
      (get-new-first first-values))))


(defun solve-part-two (input)
  (let ((new-first-values-in-sequences (map 'simple-vector
                                            (lambda (vec)
                                              (find-new-first-value-in-original-seq
                                               (find-all-differences vec)))
                                            input)))
    (reduce #'+ new-first-values-in-sequences)))

;; (find-all-differences *example-row-2*)
;; reverse the list
;; then reduce by adding (+ (- x) y)
;; => (#(1 3 6 10 15 21) #(2 3 4 5 6) #(1 1 1 1))
;; (solve-part-two *puzzle-input*)
;; => 1038 (11 bits, #x40E)
