;;;; Program to solve Advent of Code 2023's Day 3 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 29 June 2024
(load "../utils.lisp")

(defconstant +p1-example-input+ (coerce (get-file-contents "input-p1-ex.txt") 'vector))
(defconstant +real-input+ (coerce (get-file-contents "input-p1-p2.txt") 'vector))

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
  (find charac "[~!\/@#$%^&*-+=-]"))

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
          (if (digit-char-p current-char)
              (progn
                ;; have to shift all values left by one then add 
                (setf accum (+ (parse-integer (string current-char))
                               (* 10 accum)))
                (format t "~%line: ~A  char-idx: ~A  accum: ~A" line-idx char-idx accum)
                (if (symbol-nearby-p line-idx char-idx problem-input-lines)
                    (setf adjacent? t)))
              ;; we've finished with the current number, so update total if appropriate
              (if (> accum 0)
                  (progn
                    (format t "~%line: ~A char-idx: ~A  accum: ~A  grand-total: ~A"
                            line-idx char-idx accum grand-total)
                    (if adjacent?
                        (progn
                          (incf grand-total accum)
                          (setf accum     0
                                adjacent? nil)
                          (format t "~%~4Tline: ~A char-idx: ~A  adjacent?: ~A  grand-total: ~A"
                                  line-idx char-idx adjacent? grand-total))
                        (setf accum 0)))))
        :finally (if (> accum 0)
                     (progn
                       (format t "~%line: ~A char-idx: ~A (end)  accum: ~A  grand-total: ~A"
                               line-idx char-idx accum grand-total)
                       (if adjacent?
                           (incf grand-total accum)))))))




(gear! +real-input+)                    ; => 525911
