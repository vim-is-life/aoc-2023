;;;; Program to solve Advent of Code 2023's Day 1 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
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
  (let ((nums (find-numbers-in-str "./input-p1-p2.txt")))
    (reduce '+ nums)))

;;; part 2: find the digit matches, where a match can be a number or the word
;;; representing the number.
(defconstant +word-number-alist+ '(("one"   . 1)
                                   ("two"   . 2)
                                   ("three" . 3)
                                   ("four"  . 4)
                                   ("five"  . 5)
                                   ("six"   . 6)
                                   ("seven" . 7)
                                   ("eight" . 8)
                                   ("nine"  . 9))
  "Maps english word representations of numbers to their integer counterparts")

;; have zero here to allow easy handling of edge case where we only found one
;; number in the string
(defconstant +number-number-alist+ '(("0" . 0)
                                     ("1" . 1)
                                     ("2" . 2)
                                     ("3" . 3)
                                     ("4" . 4)
                                     ("5" . 5)
                                     ("6" . 6)
                                     ("7" . 7)
                                     ("8" . 8)
                                     ("9" . 9))
  "Maps string numerical representations of numbers to their integer counterparts")

;; implemented as a macro so we can return the code to just look up from the one
;; list based on what list we found it.
(defun lookup-string-repr-of-number (str)
  "Return the integer number that corresponds to STR by looking it up in
+WORD-NUMBER-ALIST+ and +NUMBER-NUMBER-ALIST+."
  ;; test this way because assoc returns nil if key wasn't found, so truthy
  ;; value means we're found it in first alist
  (if (assoc str +word-number-alist+ :test 'equal)
      (cdr (assoc str +word-number-alist+ :test 'equal))
      (cdr (assoc str +number-number-alist+ :test 'equal))))

(defun find-items-from-side (alist-element line from-back?)
  (let* ((item-to-lookup (car alist-element))
         (index-of-item (search item-to-lookup line :from-end from-back?))
         (result-pair (when index-of-item (cons index-of-item item-to-lookup))))
    result-pair))



;; for a line, we can search for numbers or words from the front and back end
;; - collect the strings and indices
;; - find the pair with the minimum index and choose that one
;; - then look that one up and return its mapping
;; - then combine them like we usually do
(defun get-number-substrings (lookup-alist line)
  "Returns a list of dotted pairs that represent the index of a substring found
in LINE and the substring from LOOKUP-ALIST that we were looking for."
  (remove nil (union (mapcar (lambda (alist-element)
                               (find-items-from-side alist-element line nil))
                             lookup-alist)
                     (mapcar (lambda (alist-element)
                               (find-items-from-side alist-element line t))
                             lookup-alist)
                     :test 'equal)))

(defun extract-number-from-line (line)
  "Returns the integer in LINE found by putting the first digit found in the
tens place and the second digit found in the ones place, where digits are found
by looking for the numerical representation of a number (1:\"1\") and the word
representation of the number (1:\"one\")."
  (let* ((index-item-pairs (union (get-number-substrings +word-number-alist+ line)
                                  (get-number-substrings +number-number-alist+ line)))
         ;; if we have a list of conses of the form (index . number-representation) we can
         ;; sort the list based on the car of each of the pairs and then take the first and
         ;; last element of the list
         (sorted-matches (sort index-item-pairs (lambda (index-of-first-item index-of-second-item)
                                                  (< index-of-first-item index-of-second-item))
                               :key #'car))
         (first-match (first sorted-matches))
         ;; last returns a list containing the last n (default 1) elements of
         ;; the passed list, so we need to take the car of it
         (last-match (car (last sorted-matches)))
         (first-num (lookup-string-repr-of-number (cdr first-match)))
         (last-num (lookup-string-repr-of-number (cdr last-match))))
    (+ (* 10 first-num)
       last-num)))

(defun solve-part-two ()
  "TODO doc"
  ;; if first element of dotted pair is nil, that means it wasn't found.
  (let* ((filename "./input-p1-p2.txt")
         (numbers-found-in-lines (mapcar #'extract-number-from-line
                                         (get-file-contents filename))))
    (reduce '+ numbers-found-in-lines)))

;; (print (solve-part-two))
