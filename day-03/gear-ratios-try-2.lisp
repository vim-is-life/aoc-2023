;;;; Program to solve Advent of Code 2023's Day 3 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 29 June 2024
(load "../utils.lisp")

(defconstant +p1-example-input+ (coerce (get-file-contents "input-p1-ex.txt") 'vector))
(defconstant +real-input+ (coerce (get-file-contents "input-p1-ex.txt") 'vector))

(defun clamp-index (idx lower upper)
  "Return IDX if it is within UPPER and LOWER. If IDX is below LOWER, then
return LOWER, or if IDX is above or equal to UPPER, return (1- UPPER)."
  (cond ((< idx lower) lower)
        ((>= idx upper) (1- upper))
        (t idx)))

(clamp-index 0 0 10)

(defmacro aref-clamped (array idx lower upper)
  "Like AREF, but clamp the index before doing the array access"
  `(aref ,array (clamp-index ,idx ,lower ,upper)))

(defun is-char-symbol? (charac)
  "Return non-nil if CHARAC is in a list of symbols, otherwise return nil"
  (find charac "[~!@#$%^&*-+=-]"))

(defun symbol-nearby-p (line-idx char-idx input-lines)
  "Return true if there is a symbol nearby adjacent to the character at the given index pair."
  (let* ((current-line (aref input-lines line-idx))
         (width        (length current-line))
         (height       (length input-lines))
         (left-idx     (clamp-index (1- char-idx) 0 width))
         (right-idx    (clamp-index (1+ char-idx) 0 width))
         (line-below   (aref-clamped input-lines (1+ line-idx) 0 height))
         (line-above   (aref-clamped input-lines (1- line-idx) 0 height)))
    ;; laterals
    (cond ((is-char-symbol? (aref current-line left-idx)))
          ((is-char-symbol? (aref current-line right-idx)))
          ;; verticals
          ((is-char-symbol? (aref line-below char-idx)))
          ((is-char-symbol? (aref line-above char-idx)))
          ;; diagonals
          ((is-char-symbol? (aref line-above right-idx)))
          ((is-char-symbol? (aref line-above left-idx)))
          ((is-char-symbol? (aref line-below right-idx)))
          ((is-char-symbol? (aref line-below left-idx))))))

(defun gear! (problem-input-lines)
  "PROBLEM-INPUT-LINES: a vector of strings containing the lines representing the text of the
problem"
  (loop
    :for line-idx :below (length problem-input-lines)
    :for current-line = (aref problem-input-lines line-idx)
    :with grand-total = 0
    :finally (return grand-total) :do
      (loop
        :for char-idx :below (length current-line)
        :for current-char = (aref current-line char-idx)
        :with adjacent? = nil
        :with accum = 0 :do
          (if (numberp current-char)
              (progn
                (incf accum (parse-integer current-char))
                (if (symbol-nearby-p line-idx char-idx problem-input-lines)
                    (setf adjacent? t)))
              ;; we've finished with the current number, so either update total if appropriate
              (if (> accum 0)
                  (if adjacent? (incf grand-total accum))
                  (setf accum 0))))))
;;; NOTE: start here when come back.
;;; the above should work, but when i run it it reports zero. i'd be interested
;;; to know what the nonzero values of accum are on every non number. i
;;; basically need to see how this is actually working. i'm thinking maybe
;;; something could be going on funky with the scope of the GRAND-TOTAL variable
;;; so that might also need fixing.

(gear! +p1-example-input+)
