(in-package :aoc-2023)

(defun custom-read (filename)
  (let ((file-contents (get-file-contents filename)))
    (mapcar (lambda (line)
              (map 'list
                   (lambda (char)
                     (if (eql char #\#) 'gal nil))
                   line))
            file-contents)))

;; (custom-read "example-input.txt")

(defconstant +d11/example-input+ (custom-read "example-input.txt"))
(defconstant +d11/puzzle-input+  (custom-read "puzzle-input.txt"))

;;; PART 1
(defun num-galaxies (input)
  "Return the number of galaxies in the universe that INPUT represents"
  (->> input
    (mapcar (lambda (line)
              (count 'gal line)))
    (reduce #'+)))

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
