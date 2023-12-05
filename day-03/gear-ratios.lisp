;;;; Program to solve Advent of Code 2023's Day 3 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 4 Dec 2023
(ql:quickload "str")
(ql:quickload "cl-ppcre")
(load "../utils.lisp")

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

(defun get-numbers-in-line (line)
  (reshape-results (ppcre:all-matches *number-regex* line)))

(defun get-symbols-in-line (line)
  (reshape-results (ppcre:all-matches *symbol-regex* line) t))

(defun number-adjacent-p (number-start-and-end-indices symbol-index)
  (labels ((either-side-adjacent-p (number-idx symbol-idx)
             ;; number is to right
             (or (= (1+ number-idx) symbol-idx)
                 ;; number is to left
                 (= (1- number-idx) symbol-idx)
                 ;; number is above or below
                 (= number-idx symbol-idx))))
    (destructuring-bind (number-start-index number-end-index) number-start-and-end-indices
      (or (either-side-adjacent-p number-start-index symbol-index)
          (either-side-adjacent-p number-end-index symbol-index)))))

(defun not-adjacent-p (number-index symbol-indices)
  (notany (lambda (symbol-index)
            (number-adjacent-p number-index symbol-index))
          symbol-indices))

(defun get-adjacent-numbers-from-3-lines (line-1-number-indices
                                          line-2-number-indices
                                          line-3-number-indices
                                          symbol-indices)
  (let ((adjacent-from-1 (remove-if (lambda (num-idx)
                                      (not-adjacent-p num-idx symbol-indices))
                                    line-1-number-indices))
        (adjacent-from-2 (remove-if (lambda (num-idx)
                                      (not-adjacent-p num-idx symbol-indices))
                                    line-2-number-indices))
        (adjacent-from-3 (remove-if (lambda (num-idx)
                                      (not-adjacent-p num-idx symbol-indices))
                                    line-3-number-indices)))
    (union adjacent-from-1 (union adjacent-from-2 adjacent-from-3))))

(defun get-number-from-index (start-end-index-pair candidate-lines)
  (destructuring-bind (idx _ line-1 line-2 line-3) (append start-end-index-pair candidate-lines)
    (some (lambda (x) (if x x)) (list (parse-integer line-1 :junk-allowed t :start idx)
                                      (if line-2 (parse-integer line-2 :junk-allowed t :start idx))
                                      (if line-3 (parse-integer line-3 :junk-allowed t :start idx))))))

(defun get-numbers-from-indices (number-indices candidate-lines)
  (labels ((collect-numbers (accum number-indices lines)
             (if (not number-indices)
                 accum
                 (let ((number (get-number-from-index (car number-indices) lines)))
                   (print number)
                   (collect-numbers (cons number accum) (cdr number-indices) lines)))))
    (collect-numbers nil number-indices candidate-lines)))

(defun get-indices-of-numbers-adjacent-to-symbols (lines)
  (labels ((check-for-syms-around-nums (accum
                                        numbers-in-last-line
                                        current-line-numbers
                                        current-line-symbols
                                        numbers-in-next-line
                                        rest-of-lines)
             (if (not rest-of-lines)
                 accum
                 ;; for every line, find numbers (list of (start end)) and symbols.
                 ;; for each (start end) elem in number list, collect the number if it is adjacent to a symbol
                 ;; we're only ever checking off the symbols in the middle line in every triplet of lines
                 (let* ((last-line    (car   rest-of-lines))
                        (current-line (cadr  rest-of-lines))
                        (next-line    (caddr rest-of-lines))
                        (numbers-in-line-after-next (get-numbers-in-line (cadddr rest-of-lines)))
                        (adjacent-indices           (get-adjacent-numbers-from-3-lines numbers-in-last-line
                                                                                       current-line-numbers
                                                                                       numbers-in-next-line
                                                                                       current-line-symbols))
                        (adjacent-numbers (get-numbers-from-indices adjacent-indices
                                                                    (list last-line current-line next-line))))
                   (check-for-syms-around-nums (append adjacent-numbers accum)
                                               current-line-numbers ; make this line last line
                                               numbers-in-next-line ; make next line this line
                                               (get-symbols-in-line (car rest-of-lines))
                                               numbers-in-line-after-next ; make line after next the new next
                                               (cdr rest-of-lines))))))
    (let* ((first-line-numbers  (get-numbers-in-line (car   lines)))
           (second-line-numbers (get-numbers-in-line (cadr  lines)))
           (second-line-symbols (get-symbols-in-line (cadr  lines)))
           (third-line-numbers  (get-numbers-in-line (caddr lines))))
      (check-for-syms-around-nums nil
                                  first-line-numbers
                                  second-line-numbers
                                  second-line-symbols
                                  third-line-numbers
                                  lines))))

                                        ; => ((2 4) (6 8) (2 3) (0 2) (6 8) (0 2) (2 3))

(defun solve-part-one ()
  (let* ((file-lines (get-file-contents "./input-p1-ex.txt"))
         (adjacent-numbers (get-indices-of-numbers-adjacent-to-symbols file-lines)))
    (reduce '+ adjacent-numbers)))

(solve-part-one)
