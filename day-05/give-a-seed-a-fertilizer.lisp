;;;; Program to solve Advent of Code 2023's Day 5 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 8 Dec 2023
(ql:quickload "str")
(ql:quickload "cl-ppcre")
(load "../utils.lisp")

;;; part 1: find the lowest location number that corresponds to any of the initial seeds
;;; pipeline:
;;; seed -> soil
;;; soil -> fertilizer
;;; fertilizer -> water
;;; water -> light
;;; light -> temperature
;;; temperature -> humidity
;;; humidity -> location
(defparameter *puzzle-input* (str:from-file "input-p1-ex.txt")
  "*PUZZLE-INPUT* contains the text of the input for today's puzzle.")

(defparameter *puzzle-input* (str:from-file "input-p1-p2.txt")
  "*PUZZLE-INPUT* contains the text of the input for today's puzzle.")

(defun get-region-map (source str)
  "Return a hash table of source to destination mappings found by searching STR,
where source is SOURCE and destination is what SOURCE maps to.
SOURCE is one of:
'seed
'soil
'fertilizer
'water
'light
'temperature
'humidity"
  (let* ((region (case source
                   ('seed        "seed-to-soil")
                   ('soil        "soil-to-fertilizer")
                   ('fertilizer  "fertilizer-to-water")
                   ('water       "water-to-light")
                   ('light       "light-to-temperature")
                   ('temperature "temperature-to-humidity")
                   ('humidity    "humidity-to-location")))
         (search-text  (concatenate 'string region " map:"))
         (region-start (search search-text str))
         ;; have to search end with regex because can't get builtin SEARCH to
         ;; match two newlines in a row for some reason
         (region-end    (ppcre:scan "\\n\\n" str :start region-start))
         (search-region (subseq str region-start region-end)))
    (labels ((recur (lines acc)
               (if lines
                   (let ((line (car lines))
                         (rest (cdr lines)))
                     (destructuring-bind (dest-range-start src-range-start range-len)
                         (mapcar #'parse-integer (str:words line))
                       (setf (gethash))
                       (recur rest acc)))
                   acc)))
      (recur (cdr (str:lines search-region))
             (make-hash-table)))))

(defun get-seed-values (str)
  "Returns the found seed values in STR as a list of integers."
  (let ((line-length (ppcre:scan "\\n" str)))
    (mapcar #'parse-integer
            (ppcre:all-matches-as-strings "\\d.*?\\b" str :end line-length))))

(defmacro lookup-in-src-dest-map (source-value src-to-dest-map)
  "Return what SOURCE-VALUE maps to in SRC-TO-DEST-MAP, otherwise return
SOURCE-VALUE."
  `(gethash ,source-value ,src-to-dest-map ,source-value))

(defun solve-part-one ()
  (let ((seeds-to-plant (get-seed-values *puzzle-input*))
        (seed-map (get-region-map 'seed *puzzle-input*))
        (region-maps (mapcar (lambda (src) (cons src (get-region-map src *puzzle-input*)))
                             '(seed soil fertilizer water light temperature humidity))))
    region-maps))

(solve-part-one)
