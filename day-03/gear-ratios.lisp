;;;; Program to solve Advent of Code 2023's Day 3 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 4 Dec 2023
(ql:quickload "str")
(ql:quickload "cl-ppcre")

(defparameter *symbol-regex* (ppcre:create-scanner  "[~!@#$%^&*-+=-]")
  "The regex scanner to identify symbol characters in the input.")
(defparameter *number-regex* (ppcre:create-scanner "\\d\\d?\\d?")
  "The regex scanner to identify a range of 1 to 3 consecutive numbers in the
  input.")

(defun get-line-length (str)
  "Returns the length of a line in STR before the newline, assuming that all
lines are of the same length in STR."
  (ppcre:scan "\\n" str))

(defun reshape-results (search-results &optional working-with-symbols?)
  "Return an list of values with cells of the form (START END) representing
the inclusive start and end indices of where numbers or symbols are located. If
WORKING-WITH-SYMBOLS? is T, then return a list of ints represeting their indices
in the line they were searched for in according to SEARCH-RESULTS."
  (labels ((recursive-reshaper (accum rest)
             (if rest
                 (let ((start (car rest))
                       (end (cadr rest)))
                   (if working-with-symbols?
                       (recursive-reshaper (cons start accum)
                                           (cddr rest))
                       (recursive-reshaper (cons (list start (1- end)) accum)
                                           (cddr rest))))
                 accum)))
    (recursive-reshaper nil search-results)))

(defun get-numbers-and-symbols-in-line (line)
  (list (reshape-results (ppcre:all-matches *number-regex* line))
        (reshape-results (ppcre:all-matches *symbol-regex* line) t)))

(destructuring-bind (numbers symbols) (get-numbers-and-symbols-in-line
                                       ".....*...=.875*342......$........................@........-.....846.........610..829.934*508...79............&691..48...................483.")
  (print numbers)
  (print symbols))

(defun number-adjacent-p (number-start-and-end-indices symbol-index)
  (((labels ((number-index-adjacent-p (number-index symbol-index)
               (destructuring-bind (number-x number-y symbol-x symbol-y)
                   (append number-index symbol-index)
                 ;; number is directly right of symbol
                 (or (and (= number-y symbol-y)
                          (= (1+ number-x) symbol-x))
                     ;; number is directly left of symbol
                     (and (= number-y symbol-y)
                          (= (1- number-x) symbol-x))
                     ;; number is directly above (ie row - 1) of symbol
                     (and (= (1- number-y) symbol-y)
                          (= number-x symbol-x))
                     ;; number is directly below (ie row + 1) of symbol
                     (and (= (1+ number-y) symbol-y)
                          (= number-x symbol-x))
                     ;; number is diagonal down-right
                     (and (= (1+ number-y) symbol-y)
                          (= (1+ number-x) symbol-x))
                     ;; number is diagonal down-left
                     (and (= (1+ number-y) symbol-y)
                          (= (1- number-x) symbol-x))
                     ;; number is diagonal up-right
                     (and (= (1- number-y) symbol-y)
                          (= (1+ number-x) symbol-x))
                     ;; number is diagonal up-left
                     (and (= (1- number-y) symbol-y)
                          (= (1- number-x) symbol-x))))))
      (destructuring-bind (number-start-index number-end-index)
          (or (number-start-index symbol-index)
              (number-end-index symbol-index)))))))


(defun idkman (multiline-str)
  (let ((line-length (get-line-length multiline-str)))
    (labels ((check-for-syms-around-nums (accum last-line-info this-line-info next-line-info rest-of-lines)
               (if rest-of-lines
                   accum
                   ;; for every line, find numbers (list of (start end)) and symbols.
                   ;; for each (start end) elem in number list, collect the number if it is adjacent to a symbol
                   (let ((new-next-line-info (list ())))
                     (check-for-syms-around-nums (cons new-results accum)
                                                 this-line-info next-line-info
                                                 new-next-line-info
                                                 (cdr rest-of-lines)))))))))
