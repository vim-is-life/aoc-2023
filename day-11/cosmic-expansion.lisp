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

(defun list-of-lists-as-string (list-of-lists)
  (->> list-of-lists
    (mapcar (lambda (row)
              (map 'string
                   (lambda (spc) (if (eql spc 'gal) #\# #\.))
                   row)))
    (format nil "~{~A~%~}")))

;; (list-of-lists-as-string +d11/example-input+)

(defun transpose (list-of-lists)
  "Return the tranpose of LIST-OF-LISTS, where LIST-OF-LISTS is a list of lists
of symbols."
  ;; taken from 2-3 of Common Lisp Recipes book
  (apply #'mapcar #'list list-of-lists))

;; (transpose +d11/example-input+)

(defun expand-universe (input)
  "Return the expanded universe or expanded version of INPUT where all empty
rows and columns are doubled (1 row -> 2 rows, and same for columns).
Note that the columns and rows will come back reversed."
  (labels ((double-rows (rows acc)
             (if rows
                 (let ((row (first rows))
                       (rest-rows (rest rows)))
                   (if (every #'null row)
                       ;; need to double the row that has all empties
                       (double-rows rest-rows (cons row (cons row acc)))
                       (double-rows rest-rows (cons row acc))))
                 acc)))
    (-> input
      ;; first pass: do rows
      (double-rows '())
      ;; then have to transpose to do columns, then re-transpose
      transpose
      (double-rows '())
      transpose)))

;; (expand-universe +d11/example-input+)
;; (expand-universe +d11/puzzle-input+)

;; this is really only to test that i have the first function right
(defun expand-universe-inorder (input)
  "Return the expanded universe or expanded version of INPUT where all empty
rows and columns are doubled (1 row -> 2 rows, and same for columns)."
  (labels ((double-rows (rows acc)
             (if rows
                 (let ((row (first rows))
                       (rest-rows (rest rows)))
                   (if (every #'null row)
                       ;; need to double the row that has all empties
                       (double-rows rest-rows (cons row (cons row acc)))
                       (double-rows rest-rows (cons row acc))))
                 acc)))
    (-> input
      ;; first pass: do rows
      (double-rows '())
      ;; then have to transpose to do columns, then re-transpose
      reverse
      transpose
      (double-rows '())
      reverse
      transpose)))

(assert (equal (expand-universe-inorder +d11/example-input+)
               (custom-read "example-expansion.txt")))

(defun list-of-lists-to-vecvec (list-of-lists)
  "Convert LIST-OF-LISTS to a vector of vectors."
  (map 'vector
       (lambda (row) (coerce row 'vector))
       list-of-lists))

;; (list-of-lists-to-vecvec (expand-universe +d11/example-input+))

(defun get-galaxy-ids (universe-vec num-galaxies)
  "Return a hashmap mapping coordinate-pair keys to numbered ID values, which
represents each galaxy's unique number in UNIVERSE-VEC."
  (loop :with galaxy-ids = (make-hash-table :test #'equal :size num-galaxies)
        :with highest-id = -1           ; first is rly 0 because use incf in set
        :with num-lines  = (length universe-vec)
        :with num-cols   = (length (aref universe-vec 0))
        :for line-idx :below num-lines
        :for line :across universe-vec
        :do (loop :for col-idx :below num-cols
                  :for spc = (aref line col-idx)
                  :if spc
                    :do (setf (gethash `(,line-idx ,col-idx) galaxy-ids)
                              (incf highest-id)))
        :finally (return galaxy-ids)))

;; (->> (get-galaxy-ids (-> +d11/example-input+
;;                        expand-universe-inorder
;;                        list-of-lists-to-vecvec)
;;                      (num-galaxies +d11/example-input+))
;;   (alexandria:hash-table-alist)
;;   (mapc (lambda (elem) (print elem))))

(defun d11/part-1 (input)
  -1)

(assert (= (d11/part-1 +d11/example-input+) 374))

;;; PART 2
(defun d11/part-2 (input)
  -1)
