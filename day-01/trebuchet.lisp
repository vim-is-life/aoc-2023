;;;; Program to solve Advent of Code 2023's Day 1 problem
;;;; Author: vim-is-life
;;;; Date: 2 Dec 2023
(ql:quickload "str")

;;; part 1: find the calibration value the elves need by combining
;;; - first digit
;;; - last digit (in this order)
;;; to form a single two-digit number
(defun get-file-contents (filename)
  "Return a list of strings where each string line in the file FILENAME."
  (str:lines
   (str:from-file filename)))

(defun make-single-number-from-chars (char-one char-two)
  "Returns a single integer with CHAR-ONE in the tens place and CHAR-TWO in the
ones place"
  ;; call digit-char-p because it acts as a predicate and conversion
  (+ (* 10 (digit-char-p char-one))
     (digit-char-p char-two)))

;; want to say for each thing in the string, if it is a number, return it
(defun find-numbers-in-str (file-input)
  "Returns the number made of the two single digits in contents of FILE-INPUT.
For example, for some file x.txt containing the following:
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
this function called as (find-numbers-in-str \"x.txt\") the list of numbers
(12 38 15 77)."
  (let ((str (get-file-contents file-input)))
    (mapcar (lambda (line)
              (let ((first-num (find-if #'digit-char-p line))
                    (second-num (find-if #'digit-char-p line :from-end t)))
                (make-single-number-from-chars first-num second-num)))
            str)))

(defun solve-part-one ()
  "Return the solution to solve part one of day 1's problem. This function
  returns the sum of all the calibration values."
  (let ((nums (find-numbers-in-str "./input-p1.txt")))
    (reduce '+ nums)))

(solve-part-one)
