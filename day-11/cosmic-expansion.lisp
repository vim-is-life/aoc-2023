(in-package :aoc-2023)

(defun custom-read (filename)
  "Read in the problem input where every galaxy is read in as T and each
non-galaxy space is read in as NIL. Returns an array of rank 2"
  (let* ((file-contents (get-file-contents-vec filename))
         (num-lines   (length file-contents))
         (num-columns (length (aref file-contents 0))))
    (loop
      :with arr = (make-array `(,num-lines ,num-columns) :initial-element nil)
      :for line :below num-lines
      :do (loop
            :for col :below num-columns
            :for cur-char = (-> file-contents (aref line) (aref col))
            :when (eql cur-char #\#)
              :do (setf (aref arr line col) t))
      :finally (return arr))))

(defconstant +d11/example-input+ (custom-read "example-input.txt"))
(defconstant +d11/puzzle-input+  (custom-read "puzzle-input.txt"))

;;; PART 1
(defun num-galaxies (input-arr)
  "Return the number of galaxies in the universe that INPUT represents"
  (destructuring-bind (lines cols) (array-dimensions input-arr)
    (let ((flat-input (make-array (* lines cols) :displaced-to input-arr)))
      (count t flat-input))))

;; (num-galaxies +d11/example-input+)

(defun num-pairs (input)
  "Return the number of galaxy pairs in the universe represented by INPUT"
  (combinations (num-galaxies input) 2))

;; (num-pairs +d11/example-input+)

(defun d11/part-1 (input)
  -1)

(assert (= (d11/part-1 +d11/example-input+) 374))

;;; PART 2
(defun d11/part-2 (input)
  -1)
