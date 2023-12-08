;;;; Program to solve    Advent of Code 2023's Day 4 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 4 Dec 2023

;; (load "../quicklisp/setup.lisp")
(load "../utils.lisp")

;;; part 1: figure out which numbers in the card appear in the list of winning numbers jj
(defun get-matching-numbers (line)
  (let* ((after-colon (cadr (str:split ":" line)))
         (columns (str:split "|" after-colon))
         (matches (intersection (str:words (car  columns))
                                (str:words (cadr columns))
                                :test 'string=)))
    matches))

;; (matching-numbers (mapcan 'parse-integer matches))

(defun get-points-for-matches (list-of-matches)
  ;; if points and matches are like following:
  ;; 0 matches -> 0 pts
  ;; 1 matches -> 1 pts
  ;; 2 matches -> 2 pts
  ;; 3 matches -> 4 pts
  ;; then it's given by 2^(matches - 1) for matches > 0 and 0 for matches = 0
  ;; not that (length nil) => 0
  (let ((number-of-matches (length list-of-matches)))
    (if (zerop number-of-matches)
        0
        (expt 2 (1- number-of-matches)))))

(defun solve-part-one ()
  (let* ((lines   (get-file-contents "./input-p1-p2.txt"))
         (matches (mapcar #'get-matching-numbers lines))
         (points  (mapcar #'get-points-for-matches matches)))
    (reduce '+ points)))

(solve-part-one)

;;; part 2: find total scratchcards we end up with at end given
;;; that for every scratchcard you have your number of matches causes you to win
;;; more scratchcards equal to the number of matches you have

;;; if you are at card 10, 5 winning cards, you win one copy each of next 5 cards
;;; if have n winning numbers for given card
;;; - win 1 copy each of next n cards

(defun solve-part-two ()
  (let* ((lines (get-file-contents "./input-p1-p2.txt"))
         (number-cards (length lines))
         (number-matches-per-cards (map 'vector (lambda (line)
                                                  (length (get-matching-numbers line)))
                                        lines))
         ;; make array of 1d of size NUMBER-CARDS, start with 1 inside because
         ;; you have 1 original of every card
         (card-copies (make-array (list number-cards) :initial-element 1)))
    (do ((card-no 0 (1+ card-no)))
        ((= card-no number-cards))
      (let ((num-copies (if (zerop card-no) 0 (1- (aref card-copies (1- card-no)))))
            (nums-from-current-to-copy (aref number-matches-per-cards card-no)))
        ;; (format t "found ~a matches on card ~a with ~a copies~%" nums-from-current-to-copy card-no num-copies)
        (do ((copying-idx card-no (1+ copying-idx)))
            ((= copying-idx (+ card-no nums-from-current-to-copy)))
          (let ((old-value (aref card-copies copying-idx)))
            (setf (aref card-copies copying-idx)
                  (+ old-value 1 num-copies))))))
    (reduce '+ card-copies)))

;; (solve-part-two)
                                        ; => 11024379 (24 bits, #xA837FB)
