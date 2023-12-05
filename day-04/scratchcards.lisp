;;;; Program to solve Advent of Code 2023's Day 4 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 4 Dec 2023

;; (load "../quicklisp/setup.lisp")
(load "../utils.lisp")

;;; part 1: figure out which numbers in the card appear in the list of winning numbers jj
(defun get-matching-numbers (line)
  (let* ((after-colon (cadr (str:split ":" line)))
         (columns (str:split "|" after-colon))
         (matches (intersection (str:words (car  columns))
                                (str:words (cadr columns))
                                :test 'string=)))
    matches))

;; (matching-numbers (mapcan 'parse-integer matches))

(defun get-points-for-matches (list-of-matches)
  ;; if points and matches are like following:
  ;; 0 matches -> 0 pts
  ;; 1 matches -> 1 pts
  ;; 2 matches -> 2 pts
  ;; 3 matches -> 4 pts
  ;; then it's given by 2^(matches - 1) for matches > 0 and 0 for matches = 0
  ;; not that (length nil) => 0
  (let ((number-of-matches (length list-of-matches)))
    (if (zerop number-of-matches)
        0
        (expt 2 (1- number-of-matches)))))

(defun solve-part-one ()
  (let* ((lines   (get-file-contents "./input-p1-p2.txt"))
         (matches (mapcar #'get-matching-numbers lines))
         (points  (mapcar #'get-points-for-matches matches)))
    (reduce '+ points)))

(solve-part-one)
