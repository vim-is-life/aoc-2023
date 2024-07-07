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
  "Return the solution to part 1 of the day 3 problem.
PROBLEM-INPUT-LINES: a vector of strings containing the lines representing the text of the
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


(gear! +real-input+)
;; => 525911

;;;; PART 2
(defun prep-input (lines)
  (let* ((text-len (length (first lines)))
         (blank-line (make-string text-len :initial-element #\.))
         (text (copy-seq lines)))
    (push blank-line text)
    (nconc text `(,blank-line))))

(defconstant +p2-example-input+ (coerce
                                 (prep-input (get-file-contents "input-p1-ex.txt"))
                                 'vector))
(defconstant +p2-real-input+ (coerce
                              (prep-input (get-file-contents "input-p1-p2.txt"))
                              'vector))

(defun read-in-num (line-idx char-idx input-lines direction)
  "Return the full number at the index based on the DIRECTION in which to read
the number.

LINE-IDX: The index of the line in which to read from.
CHAR-IDX: The index of the character with which to start reading in a number.
INPUT-LINES-DIRECTION: The direction in which to read characters to form the
  number. One of the symbols :LEFT, :RIGHT, or :MIDDLE.

For instance, if you want to parse the line below where this is line idx 3:
  738*..617.*43
and you want to parse the 738 before the *, you would call
  (read-in-num 3 2 :left)
but if you want parse the 43 after the second *, you would call
  (read-in-num 3 11 :right)
but if you want to parse the 617 in the middle from the middle 1 (say that there's
a star above), you would call
   (read-in-num 3 7 :middle)"
  (let ((problem-line (aref input-lines line-idx)))
    (flet ((read-right (idx &key (return-number? t))
             (let ((accum (make-array 0 :element-type 'character
                                        :fill-pointer 0 :adjustable t)))
               (loop :for char-idx :from idx :below (length problem-line)
                     :for current-char = (aref problem-line char-idx)
                     :while (digit-char-p current-char)
                     :do (vector-push-extend current-char accum))
               (if return-number?
                   (parse-integer accum)
                   accum)))
           (read-left (idx &key (return-number? t))
             (let ((accum (make-array 0 :element-type 'character
                                        :fill-pointer 0 :adjustable t)))
               (loop :for char-idx :from idx :downto 0
                     :for current-char = (aref problem-line char-idx)
                     :while (digit-char-p current-char)
                     :do (vector-push-extend current-char accum))
               (setf accum (reverse accum))
               (if return-number?
                   (parse-integer accum)
                   accum))))
      (case direction
        (:right (read-right char-idx))
        (:left (read-left char-idx))
        (:middle (let* ((left-side (read-left char-idx :return-number? nil))
                        (right-side (read-right (1+ char-idx) :return-number? nil))
                        (sides-combined (concatenate 'string left-side right-side)))
                   (parse-integer sides-combined)))))))

;; (read-in-num 1 1 +p2-example-input+ :middle)

(defun get-numbers-around (line-idx char-idx input-lines)
  "Return a list of all the numbers around the given character at the coordinates.

LINE-IDX: the line index of the character in question
CHAR-IDX: the index of the character in the given line
INPUT-LINES: the vector of strings representing the problem input"
  (flet ((num-at? (coords)
           (destructuring-bind (line-idx char-idx) coords
             (digit-char-p (aref (aref input-lines line-idx) char-idx))
             (if (digit-char-p (aref (aref input-lines line-idx) char-idx))
                 (read-in-num line-idx char-idx input-lines :middle)))))
    ;; top-left     top-middle      top-right
    ;; left                         right
    ;; bottom-left  bottom-middle   bottom-right
    (let* ((width (length (aref input-lines 0)))
           (above-idx (1- line-idx))
           (below-idx (1+ line-idx))
           (left-idx (clamp-index (1- char-idx) 0 width))
           (right-idx (clamp-index (1+ char-idx) 0 width))
           (locs-to-check `((,above-idx ,left-idx) (,above-idx ,char-idx) (,above-idx ,right-idx)
                            (,line-idx ,left-idx)   (,line-idx ,right-idx)
                            (,below-idx ,left-idx) (,below-idx ,char-idx) (,below-idx ,right-idx)))
           (result (mapcar #'num-at? locs-to-check)))
      (remove-duplicates (remove nil result)))))

;; (get-numbers-around 9 5 +p2-example-input+)

(defun get-sum-of-line-gear-ratios (line-idx input-lines)
  "Return the sum of the gear ratios in the line, where the gear ratio is the product of the
number pair adjacent to a star (*) that only has two adjacent numbers.
Note that this function assumes that it cannot go out of bounds by going to the line above or
below the given line. You can prepare the input for this by adding a line of dots to the top
and bottom before calling the function.

LINE-IDX: the index of the line in question
INPUT-LINES: the array of strings representing the lines in the problem input"
  (loop :with current-line = (aref input-lines line-idx)
        :with star = #\*
        :for char-idx :below (length current-line)
        :for current-char = (aref current-line char-idx)
        :summing (if (eql current-char star)
                     (let ((numbers-around (get-numbers-around line-idx char-idx
                                                               input-lines)))
                       (if (= (length numbers-around) 2)
                           (* (first numbers-around) (second numbers-around))
                           0))
                     0)))

(defun gear2! (problem-input)
  "NOTE: this function assumes you've expanded the input by adding one blank
line to the beginning and end of the input"
  (loop :for line-idx :from 1 :below (1- (length problem-input))
        :summing (get-sum-of-line-gear-ratios line-idx problem-input)))

(gear2! +p2-real-input+)
;; => 75805607 (27 bits, #x484B3A7)
