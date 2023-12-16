;;;; Program to solve Advent of Code 2023's Day 7 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 10 Dec 2023

(load "~/quicklisp/setup.lisp")
(load "../utils.lisp")

;; (defparameter *puzzle-input* (uiop:read-file-string "./input-p1-p2.txt"))

;;; part 1: rank the hands and then determine the total winnings of the set
;;; order of operations
;;; 0. parse the things into a set of hands (array)
;;; 1. sort/order/rank the hands with a predicate
;;;    - predicate should first order based on type
;;;    - then it should order based on hand contents
;;; 2. find each hand's winnings and then sum those to find the total winnings

;; lookup tables for hand types
(defparameter *hand-types-alist* '((:five-kind  . 7)
                                   (:four-kind  . 6)
                                   (:full-house . 5)
                                   (:three-kind . 4)
                                   (:two-pair   . 3)
                                   (:one-pair   . 2)
                                   (:high-card  . 1)))
;; lookup table for cards
(defparameter *card-strengths-alist*
  (pairlis '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)
           '(13  12  11  10  9   8   7   6   5   4   3   2   1)))

(defparameter *card-strengths-alist-p2*
  (pairlis '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)
           '(13  12  11  0   9   8   7   6   5   4   3   2   1)))

(defun parse-line (line)
  "Returns a list consisting of the string hand and the integer bid amount."
  (destructuring-bind (hand bid-str) (str:words line)
    (list hand (parse-integer bid-str))))

(defun rank-hand-type (hand hand-tbl)
  (let ((counts (pairlis
                 '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)
                 '(0   0   0   0   0   0   0   0   0   0   0   0   0))))
    (do ((idx 0 (1+ idx)))
        ((= idx 5))
      (incf (cdr (assoc (char hand idx) counts))))
    (let* ((occurrences (remove-if #'zerop counts :key #'cdr))
           ;; order does matter, don't move things around without testing the asserts
           (kind-of-hand (cond ((= (length occurrences) 1) :five-kind)
                               ((every (lambda (c) (or (= (cdr c) 3) (= (cdr c) 2))) occurrences) :full-house)
                               ((= (length occurrences) 2) :four-kind)
                               ((every (lambda (c) (= (cdr c) 1)) occurrences) :high-card)
                               ((every (lambda (c) (or (= (cdr c) 3) (= (cdr c) 1))) occurrences) :three-kind)
                               ((= 1 (length (remove-if-not (lambda (occ) (= occ 2)) occurrences :key #'cdr))) :one-pair)
                               ((every (lambda (c) (or (= (cdr c) 2) (= (cdr c) 1))) occurrences) :two-pair))))
      (cdr (assoc kind-of-hand hand-tbl)))))

;; (assert (eql 7 (rank-hand-type "AAAAA" *hand-types-alist*)))
;; (assert (eql 6 (rank-hand-type "AA8AA" *hand-types-alist*)))
;; (assert (eql 5 (rank-hand-type "23332" *hand-types-alist*)))
;; (assert (eql 4 (rank-hand-type "TTT98" *hand-types-alist*)))
;; (assert (eql 3 (rank-hand-type "23432" *hand-types-alist*)))
;; (assert (eql 2 (rank-hand-type "A23A4" *hand-types-alist*)))
;; (assert (eql 1 (rank-hand-type "23456" *hand-types-alist*)))

(defun first-has-stronger-cards? (card-tbl first-hand second-hand &optional (idx 0))
  (when (/= idx 5)
    (let* ((first-hand-char (char first-hand idx))
           (second-hand-char (char second-hand idx))
           (first-hand-card-rank  (cdr (assoc first-hand-char card-tbl)))
           (second-hand-card-rank (cdr (assoc second-hand-char card-tbl)))
           (same-card? (eql first-hand-char second-hand-char)))
      (if same-card?
          (first-has-stronger-cards? card-tbl first-hand second-hand (1+ idx))
          (> first-hand-card-rank second-hand-card-rank)))))

(defun higher-rank? (rank-tbl card-tbl first-hand second-hand)
  (let ((rank-of-first  (rank-hand-type first-hand rank-tbl))
        (rank-of-second (rank-hand-type second-hand rank-tbl)))
    (if (= rank-of-first rank-of-second)
        (first-has-stronger-cards? card-tbl first-hand second-hand)
        (> rank-of-first rank-of-second))))

;; (assert (eql t (higher-rank? *hand-types-alist* *card-strengths-alist* "AAAAA" "23332")))
;; (assert (eql t (higher-rank? *hand-types-alist* *card-strengths-alist* "KK677" "KTJJT")))

(defun solve-part-one ()
  (setf (symbol-function 'is-higher-rank?) (curry #'higher-rank? *hand-types-alist* *card-strengths-alist*))
  (let* ((hands (map 'vector #'parse-line (uiop:read-file-lines "input.txt")))
         (sorted-hands (nreverse (sort (copy-seq hands) #'is-higher-rank? :key #'car))))
    (do* ((idx 0 (1+ idx))
          (sum 0))
         ((= (length sorted-hands) idx) sum)
      (incf sum (* (cadr (aref sorted-hands idx))
                   (1+ idx))))))

;; (solve-part-one)
                                        ; => 249726565 (28 bits, #xEE28665)

;;; part 2: determine total winnings according to new rule below:
;;; - joker is now individually weakest card but in a collection it will behave
;;;   like whatever card what would make the hand be the best hand

;;; TODO

(defun adjust-for-jokers (unadjusted-occurrences count-jokers)
  (let* ((occurrences-sorted (sort (remove-if (lambda (c) (eql c #\J)) unadjusted-occurrences :key #'car)
                                   #'> :key #'cdr))
         (count-to-inc (car occurrences-sorted))
         (card (car count-to-inc))
         (count (cdr count-to-inc))
         (remaining-cards (cdr occurrences-sorted)))
    (cons (cons card (+ count count-jokers))
          remaining-cards)))


;; (adjust-for-jokers '((#\6 . 1) (#\7 . 2) (#\K . 2)) 0)
;; (adjust-for-jokers '((#\T . 2) (#\J . 2) (#\K . 1)) 2)

(defun rank-hand-type-p2 (hand hand-tbl)
  (let ((counts (pairlis
                 '(#\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)
                 '(0   0   0   0   0   0   0   0   0   0   0   0   0))))
    (do ((idx 0 (1+ idx)))
        ((= idx 5))
      (incf (cdr (assoc (char hand idx) counts))))
    (let* ((unadjusted-occurrences (remove-if #'zerop counts :key #'cdr))
           (count-jokers (cdr (assoc #\J counts)))
           (occurrences (if (string= hand "JJJJJ")
                            unadjusted-occurrences
                            (adjust-for-jokers unadjusted-occurrences count-jokers)))
           ;; order does matter, don't move things around without testing the asserts
           (kind-of-hand (cond ((= (length occurrences) 1) :five-kind)
                               ((every (lambda (c) (or (= (cdr c) 3) (= (cdr c) 2))) occurrences) :full-house)
                               ((= (length occurrences) 2) :four-kind)
                               ((every (lambda (c) (= (cdr c) 1)) occurrences) :high-card)
                               ((every (lambda (c) (or (= (cdr c) 3) (= (cdr c) 1))) occurrences) :three-kind)
                               ((= 1 (length (remove-if-not (lambda (occ) (= occ 2)) occurrences :key #'cdr))) :one-pair)
                               ((every (lambda (c) (or (= (cdr c) 2) (= (cdr c) 1))) occurrences) :two-pair))))
      ;; (print hand)
      ;; (print occurrences)
      (cdr (assoc kind-of-hand hand-tbl)))))

;; (assert (eql 2 (rank-hand-type-p2 "32T3K" *hand-types-alist*)))
;; (assert (eql 3 (rank-hand-type-p2 "KK677" *hand-types-alist*)))
;; (assert (eql 6 (rank-hand-type-p2 "T55J5" *hand-types-alist*)))
;; (assert (eql 6 (rank-hand-type-p2 "KTJJT" *hand-types-alist*)))
;; (assert (eql 6 (rank-hand-type-p2 "QQQJA" *hand-types-alist*)))

(defun p2-higher-rank? (rank-tbl card-tbl first-hand second-hand)
  (let ((rank-of-first  (rank-hand-type-p2 first-hand rank-tbl))
        (rank-of-second (rank-hand-type-p2 second-hand rank-tbl)))
    (if (= rank-of-first rank-of-second)
        (first-has-stronger-cards? card-tbl first-hand second-hand)
        (> rank-of-first rank-of-second))))

(defun solve-part-two ()
  (setf (symbol-function 'is-higher-rank?) (curry #'p2-higher-rank? *hand-types-alist* *card-strengths-alist-p2*))
  (let* ((hands (map 'vector #'parse-line (uiop:read-file-lines "input.txt")))
         (sorted-hands (nreverse (sort (copy-seq hands) #'is-higher-rank? :key #'car))))
    (do* ((idx 0 (1+ idx))
          (sum 0))
         ((= (length sorted-hands) idx) sum)
      (incf sum (* (cadr (aref sorted-hands idx))
                   (1+ idx))))))

;; (solve-part-two)
                                        ; => 251135960 (28 bits, #xEF807D8)
