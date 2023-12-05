;;;; Program to solve Advent of Code 2023's Day 3 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 4 Dec 2023
(ql:quickload "str")
(ql:quickload "cl-ppcre")

(defparameter *symbol-regex* (ppcre:create-scanner  "[~!@#$%^&*-+=-]"))
(defparameter *number-regex* (ppcre:create-scanner "\\d\\d?\\d?"))

(defun get-line-length (str)
  "Returns the length of a line in STR before the newline, assuming that all
lines are of the same length in STR."
  (ppcre:scan "\\n" str))

(defun convert-2d-string-index-to-1d (2d-index-list line-length-upto-newline)
  ;; increment, multiply, add
  (destructuring-bind (x y) 2d-index-list
    (+ (* (1+ line-length-upto-newline)
          y)
       x)))

(defun convert-1d-string-index-to-2d (flat-index line-length-upto-newline)
  ;; subtract, divide, decrement
  (let ((y (floor flat-index
                  (1+ line-length-upto-newline)))
        (x (mod flat-index
                (1+ line-length-upto-newline))))
    (list x y)))

(defun get-symbol-indicies-in-multiline-str (str-to-search pattern-to-look-for)
  (let ((line-length (get-line-length str-to-search))
        (result-list nil)
        (first-match-position (ppcre:scan pattern-to-look-for str-to-search)))
    (do ((current-match-position first-match-position
                                 (ppcre:scan pattern-to-look-for str-to-search
                                             :start (1+ current-match-position))))
        ((not current-match-position) result-list)
      (push (convert-1d-string-index-to-2d current-match-position line-length)
            result-list))))





                                        ; => (15 16 16 17 17 18 30 31 31 32 32 33 51 52 52 53 53 54 57 58 58 59 59 60 63 64
                                        ; 64 65 65 66 107 108 108 109 109 110 114 115 115 116 116 117 127 128 128 129
                                        ; 129 130)

(defun reshape-results (search-results)
  (labels ((recursive-reshaper (accum rest)
             (if rest
                 (let ((x (car rest))
                       (y (cadr rest)))
                   (recursive-reshaper (cons (cons x y) accum) (cddr rest)))
                 accum)))
    (recursive-reshaper nil (remove-duplicates search-results))))

(ppcre:all-matches *number-regex* "...............307............130..................969...601...186.........................................312....628..........878..........")
                                        ; => (15 16 16 17 17 18 30 31 31 32 32 33 51 52 52 53 53 54 57 58 58 59 59 60 63 64
                                        ; 64 65 65 66 107 108 108 109 109 110 114 115 115 116 116 117 127 128 128 129
                                        ; 129 130)
(reshape-results (ppcre:all-matches *number-regex* "...............307............130..................969...601...186.........................................312....628..........878.........."))
                                        ; => ((129 . 130) (127 . 128) (116 . 117) (114 . 115) (109 . 110) (107 . 108)
                                        ; (65 . 66) (63 . 64) (59 . 60) (57 . 58) (53 . 54) (51 . 52) (32 . 33)
                                        ; (30 . 31) (17 . 18) (15 . 16))

                                        ; => ((129 . 130) (128 . 129) (127 . 128) (116 . 117) (115 . 116) (114 . 115)
                                        ; (109 . 110) (108 . 109) (107 . 108) (65 . 66) (64 . 65) (63 . 64) (59 . 60)
                                        ; (58 . 59) (57 . 58) (53 . 54) (52 . 53) (51 . 52) (32 . 33) (31 . 32)
                                        ; (30 . 31) (17 . 18) (16 . 17) (15 . 16))


(defun number-adjacent-p (number-index symbol-index)
  (destructuring-bind (number-x number-y) number-index
    (destructuring-bind (symbol-x symbol-y) symbol-index
      ;; number is directly right of symbol
      (or (and (= number-y symbol-y)
               (= (1+ number-x) symbol-x))
          ;; number is directly left of symbol
          (and (= number-y symbol-y)
               (= (1- number-x) symbol-x))
          ;; number is directly above (ie row - 1) of symbol
          (and (= (1- number-y) symbol-y)
               (= number-x symbol-x))
          ;; number is directly below (ie row + 1) of symbol
          (and (= (1+ number-y) symbol-y)
               (= number-x symbol-x))
          ;; number is diagonal down-right
          (and (= (1+ number-y) symbol-y)
               (= (1+ number-x) symbol-x))
          ;; number is diagonal down-left
          (and (= (1+ number-y) symbol-y)
               (= (1- number-x) symbol-x))
          ;; number is diagonal up-right
          (and (= (1- number-y) symbol-y)
               (= (1+ number-x) symbol-x))
          ;; number is diagonal up-left
          (and (= (1- number-y) symbol-y)
               (= (1- number-x) symbol-x))))))

;; for every number want to


(let* ((text (str:from-file "./input-p1-p2.txt"))
       (symbols (get-symbol-indicies-in-multiline-str text *symbol-regex*))
       (numbers (get-symbol-indicies-in-multiline-str text *number-regex*))
       ;; i know this probably isn't the most efficient, but it felt like an easy functional way to do things
       (numbers-by-symbols (mapcar (lambda (symbol-idx)
                                     (remove-if-not (lambda (number-idx)
                                                      (number-adjacent-p number-idx symbol-idx))
                                                    numbers))
                                   symbols)))
  numbers-by-symbols)

(defun idkman (multiline-str)
  (let ((line-length (get-line-length multiline-str)))
    (labels ((check-for-syms-around-nums (accum last-line-info this-line-info next-line-info rest-of-lines)
               (if rest-of-lines
                   accum
                   ;; for every line, find numbers (list of (start end)) and symbols.
                   ;; for each (start end) elem in number list, collect the number if it is adjacent to a symbol
                   (let ((new-next-line-info (list ())))
                     (check-for-syms-around-nums (cons new-results accum)
                                                 this-line-info next-line-info
                                                 new-next-line-info
                                                 (cdr rest-of-lines)))))))))
