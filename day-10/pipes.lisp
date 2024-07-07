;;;; Program to solve Advent of Code 2023's Day 10 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 7 July 2024
(load "../utils.lisp")

(defun extend-input (lines)
  "add a blank line before and after LINES, and then for every line, add a period
at the beginning and end"
  (flet ((add-dot-on-ends (line)
           (format nil ".~A." line)))
    (let* ((lines-extended (mapcar #'add-dot-on-ends lines))
           (text-len (length (first lines-extended)))
           (blank-line (make-string text-len :initial-element #\.)))
      (push blank-line lines-extended)
      (nconc lines-extended (list blank-line)))))

(defconstant +example-input+ (coerce (extend-input (get-file-contents "example-input.txt"))
                                     'vector))
(defconstant +puzzle-input+ (coerce (extend-input (get-file-contents "puzzle-input.txt"))
                                    'vector))

;;; PART ONE
(defmacro flat-aref (array line-idx char-idx)
  `(aref (aref ,array ,line-idx) ,char-idx))

(defun get-start-pos (input-lines)
  "Return the line and character index of the start character in INPUT-LINES,
in that order. Caller can use the result with a DESTRUCTURING-BIND"
  (loop :with start-line = -1 :and start-char = -1
        :with width = (length (aref input-lines 0))
        :with height = (length input-lines)
        :for line-idx :from 1 :below (1- height)
        :do (loop :for char-idx :from 1 :below (1- width)
                  :when (eql (flat-aref input-lines line-idx char-idx) #\S)
                    :do (return-from get-start-pos (list line-idx char-idx)))))

;; (get-start-pos +example-input+)

(defun has-neighbors-in-direc? (line-idx char-idx input-lines &key direction)
  "Return a list of line and character indices if the character at LINE-IDX, CHAR-IDX
has a neighbor in the given DIRECTION"
  (let ((char-north (flat-aref input-lines (1- line-idx) char-idx))
        (char-east (flat-aref input-lines line-idx (1+ char-idx)))
        (char-south (flat-aref input-lines (1+ line-idx) char-idx))
        (char-west (flat-aref input-lines line-idx (1- char-idx))))
    (case direction
      (:north (when (member char-north '(#\| #\7 #\F))
                (list (1- line-idx) char-idx)))
      (:east (when (member char-east '(#\- #\J #\7))
               (list line-idx (1+ char-idx))))
      (:south (when (member char-south '(#\| #\L #\J))
                (list (1+ line-idx) char-idx)))
      (:west (when (member char-west '(#\- #\L #\F))
               (list line-idx (1- char-idx)))))))

;; (has-neighbors-in-direc? 1 3 +example-input+ :direction :south)

(defun get-neigbors (line-idx char-idx input-lines)
  "Return a list of line and character index pairs of the valid neighbors or
spots that you can go based on the peice around your original position.

LINE-IDX: Index of the line of your original position
CHAR-IDX: Index of the character of your original position
INPUT-LINES: The vector of strings representing the input"
  (let* ((current-char (flat-aref input-lines line-idx char-idx))
         (north (has-neighbors-in-direc? line-idx char-idx input-lines
                                         :direction :north))
         (east (has-neighbors-in-direc? line-idx char-idx input-lines
                                        :direction :east))
         (south (has-neighbors-in-direc? line-idx char-idx input-lines
                                         :direction :south))
         (west (has-neighbors-in-direc? line-idx char-idx input-lines
                                        :direction :west))
         (neighbors (case current-char
                      (#\| (list north south))
                      (#\- (list west east))
                      (#\L (list north east))
                      (#\J (list north west))
                      (#\7 (list south west))
                      (#\F (list south east))
                      (#\. '())
                      (#\S (list north east south west)))))
    (remove nil neighbors)))

;; (get-neigbors 1 3 +example-input+)

(defun make-dist-array (input-lines)
  "Return an array of dimensions (HEIGHT WIDTH) where HEIGHT refers to the length
of the vector INPUT-LINES and WIDTH refers to the length of an element of INPUT-LINES"
  (let ((dims `(,(length input-lines) ,(length (aref input-lines 0)))))
    (make-array dims :initial-element -1)))

;; (let ((arr (make-dist-array +example-input+)))
;;   (setf (aref arr 1 3) 5)
;;   arr)

(defun solve-part-one (input)
  (let ((pipe-queue (make-instance 'queue))
        (dist-array (make-dist-array input))
        (max-dist (* 10000 -1))
        (starting-pos (get-start-pos input)))
    (enqueue pipe-queue starting-pos)
    (loop :until (qempty? pipe-queue)
          :for coords = (dequeue pipe-queue)
          :for line-idx = (first coords)
          :for char-idx = (second coords)
          :for neighbors = (get-neigbors line-idx char-idx input)
          :do (print neighbors))))

(solve-part-one +example-input+)
;; (solve-part-one +puzzle-input+)

;;; PART TWO

(defun solve-part-two (input)
  t)
