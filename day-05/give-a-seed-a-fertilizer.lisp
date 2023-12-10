;;;; Program to solve Advent of Code 2023's Day 5 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 8 Dec 2023
(ql:quickload "str")
(ql:quickload "cl-ppcre")
(ql:quickload "lparallel")
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

(defstruct (source-dest-rule (:conc-name sdr-))
  "SOURCE-DEST-RULE represents a rule to map an integer to another (for
instance, a seed number to a soil number). In the input it represents a single
line in a section's mapping.
START is where the range for the rule starts (inclusive).
END is where the range for the rule ends (inclusive).
RULE is how much you change a source number by to get a destination number."
  start
  end
  rule)

(defun source-in-rule-range? (source rule)
  (and (<= (sdr-start rule) source)
       (<= source (sdr-end rule))))

(defun get-dest-from-source (source rule)
  (+ source (sdr-rule rule)))

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
                       (let ((new-rule
                               (make-source-dest-rule :start src-range-start
                                                      :end   (+ src-range-start  (1- range-len))
                                                      :rule  (- dest-range-start src-range-start))))
                         (recur (cdr lines) (cons new-rule acc)))))
                   acc)))
      (recur (cdr (str:lines search-region))
             nil))))

(defun get-seed-values (str)
  "Returns the found seed values in STR as a list of integers."
  (let ((line-length (ppcre:scan "\\n" str)))
    (mapcar #'parse-integer
            (ppcre:all-matches-as-strings "\\d.*?\\b" str :end line-length))))

(defun get-seed-location (seed-value rules-alist)
  "Return the final location a seed of SEED-VALUE should be planted in (ie
location number. RULES-ALIST is a list whose car is a symbol representing the
source it takes in and its cdr is a series of SOURCE-DEST-RULEs."
  (let ((temp seed-value))
    (dolist (stage rules-alist temp)
      (dolist (cur-rule (cdr stage))
        (when (source-in-rule-range? temp cur-rule)
          (setf temp (+ temp (sdr-rule cur-rule)))
          (return))))))


(defun solve-part-one ()
  (let* ((seeds-to-plant (get-seed-values *puzzle-input*))
         (region-maps (mapcar (lambda (src) (cons src (get-region-map src *puzzle-input*)))
                              '(seed soil fertilizer water light temperature humidity)))
         (seed-locations (mapcar (lambda (seed) (get-seed-location seed region-maps)) seeds-to-plant)))
    (reduce #'min seed-locations)))


;; (solve-part-one)
                                        ; => 240320250 (28 bits, #xE52FEFA)

;;;; part 2: find lowest location number corresponding to any of initial seed numbers
;;; but now, seed numbers are given as ranges, ie in first line every pair gives
;;; seeds where left is start of range and right is length of range

(defun get-seeds-from-range (start length &optional acc)
  (if (= length 0)
      acc
      (get-seeds-from-range (1+ start) (1- length) (cons start acc))))


(defun get-seed-values-p2 (str)
  "Returns the found seed values in STR as a list of integers."
  (let* ((line-length (ppcre:scan "\\n" str))
         (seed-line-numbers (mapcar #'parse-integer
                                    (ppcre:all-matches-as-strings "\\d.*?\\b" str :end line-length)))
         (seeds (loop for (range-start range-len) on seed-line-numbers
                        by 'cddr
                      collect (get-seeds-from-range range-start range-len))))
    (reduce #'append seeds)))

(defun get-region-map-p2 (source str)
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
         (search-region (subseq str region-start region-end))
         (lines         (str:lines search-region))
         (rules-list))
    (dolist (line lines rules-list)
      (destructuring-bind (dest-range-start src-range-start range-len)
          (mapcar #'parse-integer (str:words line))
        (let ((new-rule (make-source-dest-rule :start src-range-start
                                               :end   (+ src-range-start  (1- range-len))
                                               :rule  (- dest-range-start src-range-start))))
          (push new-rule rules-list))))))

(defun solve-part-two ()
  (let* ((seeds-to-plant (get-seed-values-p2 *puzzle-input*)))
    seeds-to-plant))

;; (solve-part-two)
;; since we have issue where sbcl stalls as we try to run the program, (ie it
;; maxes out mem and then drops to 0 cpu) maybe don't try to store all the
;; values to map on them etc. instead, we could maybe have a function that
;; handles finding the min location for a single range of seeds and then we do
;; that recursively or in a loop to get all the mins for all the ranges. then
;; find the mins of all those mins.
