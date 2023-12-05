;;;; Program to solve Advent of Code 2023's Day 4 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 4 Dec 2023

;; (load "../quicklisp/setup.lisp")
(load "../utils.lisp")

(defun get-matching-numbers (line)
  (let* ((after-colon (cadr (str:split ":" line)))
         (columns (str:split "|" after-colon))
         (matches (intersection (str:words (car  columns))
                                (str:words (cadr columns))
                                :test 'string=)))
    matches))

;; (matching-numbers (mapcan 'parse-integer matches))

(defun get-points-for-matches (list-of-matches)
  (cond ((null list-of-matches)         0)
        ((= 1 (length list-of-matches)) 1)
        (t (reduce (lambda (result cur)
                     (cond ((and (stringp result) cur) 2)
                           (cur (* 2 result))))
                   list-of-matches))))

(defun solve-part-one ()
  (let* ((lines   (get-file-contents "./input-p1-p2.txt"))
         (matches (mapcar #'get-matching-numbers lines))
         (points  (mapcar #'get-points-for-matches matches)))
    (reduce '+ points)))

(solve-part-one)
