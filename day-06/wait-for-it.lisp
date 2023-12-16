;;;; Program to solve    Advent of Code 2023's Day 5 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 10 Dec 2023

;; (load "../quicklisp/setup.lisp")
(load "../utils.lisp")

;; (defparameter *puzzle-input* (uiop:read-file-string "./input-p1-p2.txt"))

;;; part 1: what is product of number of ways you can beat the record in each race?
;; this is a physics problem!!
;; distance appears to be d=t_p*t_l
;; - where t_p + t_l = t_tot
;; note that we can max their product of t_p and t_l given that they sum to t_tot by setting them both to be the halves of t_tot
(defun find-travel-dist (total-time time-btn-pressed)
  (* time-btn-pressed (- total-time time-btn-pressed)))

(defun range (start stop)
  (do ((x start (1+ x))
       (lst nil (cons x lst)))
      ((= x stop) lst)))

(defun get-ways-to-beat-high-score (best-dist-so-far total-time)
  ;; we are currying with 1 more than best-dist-so-far because of we shouldn't
  ;; count something as a best distance if it's equal to the current best.
  ;; remove if filters by returning the things that DON'T fit the predicate
  (setf (symbol-function 'new-best-dist?) (curry #'> (1+ best-dist-so-far))
        (symbol-function 'get-dist-from-time) (curry #'find-travel-dist total-time))
  (let* ((button-hold-times    (range 1 (1+ total-time)))
         (distances-possible   (mapcar #'get-dist-from-time button-hold-times))
         (better-distances     (remove-if #'new-best-dist? distances-possible))
         (number-winning-times (length better-distances)))
    ;; (format t "~%for race with best time of ~a and total time ~a"
    ;;         best-dist-so-far total-time)
    ;; (print distances-possible)
    ;; (print better-distances)
    ;; (print number-winning-times)
    number-winning-times))

(defun solve-part-one ()
  (let* ((input (get-file-contents "./input-p1.txt"))
         (race-times (mapcar #'parse-integer (cdr (str:words (car input)))))
         (race-best-distances (mapcar #'parse-integer (cdr (str:words (cadr input)))))
         (ways-to-beat-each-record (mapcar #'get-ways-to-beat-high-score race-best-distances race-times)))
    (reduce #'* ways-to-beat-each-record)))

;; (solve-part-one)
;; => 2065338 (21 bits, #x1F83BA)

;;; part 2: find ways to win when theres a single race instead of multiple
(defun get-range-lower-bound (best-distance total-time)
  "Returns the lowest time needed to beat the time used to obtain the
current best distance BEST-DISTANCE given that the time for the race is
TOTAL-TIME. For example, if the time that matches BEST-DISTANCE is 13 or even
13.5, this function returns 14, as this is the lowest integer time you can hold
the button and still get a distance beating the current record."
  ;; binary search
  ;; have target, upper bound, and lower bound
  ;; - get midpoint (lower + (upper - lower)/2)
  ;; - if target greater than midpoint then call self with lower as mid+1 and upper as upper
  ;; - if target less than midpoint then call self with lower as lower and upper as mid - 1
  (labels ((find-time-matching-best-dist (best-distance total-time time-lower-bound time-upper-bound)
             (let* ((time-midpoint (+ time-lower-bound
                                      (floor (- time-upper-bound time-lower-bound)
                                             2)))
                    (current-dist (find-travel-dist total-time time-midpoint)))
               ;; (format t "~%target: ~a | low: ~a ; high:: ~a; guess ~a" best-distance
               ;;         (find-travel-dist total-time time-lower-bound)
               ;;         (find-travel-dist total-time time-upper-bound)
               ;;         current-dist)
               (cond ((= current-dist best-distance) time-midpoint)
                     ;; edge case where we get close but can't get the time that
                     ;; maps to the target exactly. once we're here we just want
                     ;; to return so we don't end up infinitely recursing
                     ((< time-upper-bound time-lower-bound) time-midpoint)
                     ((< current-dist best-distance)
                      ;; (print "guess is lower")
                      (find-time-matching-best-dist best-distance total-time
                                                    (1+ time-midpoint) time-upper-bound))
                     ((> current-dist best-distance)
                      ;; (print "guess is higher")
                      (find-time-matching-best-dist best-distance total-time
                                                    time-lower-bound (1- time-midpoint)))))))
    (let* ((record-time-guess (find-time-matching-best-dist best-distance total-time
                                                            0 (floor total-time 2)))
           (distance-guess (find-travel-dist total-time record-time-guess)))
      (cond ((< distance-guess best-distance) (1+ record-time-guess))
            ((> distance-guess best-distance) record-time-guess)))))



(defun get-ways-to-beat-high-score-p2 (best-dist-so-far total-time)
  ;; we are currying with 1 more than best-dist-so-far because of we shouldn't
  ;; count something as a best distance if it's equal to the current best.
  ;; remove if filters by returning the things that DON'T fit the predicate
  (setf (symbol-function 'new-best-dist?) (curry #'> (1+ best-dist-so-far))
        (symbol-function 'get-dist-from-time) (curry #'find-travel-dist total-time))
  ;; we can enumerate only half the range because we know that the max is at half the time then we can just add one
  (let* ((button-hold-times    (range 1 (floor total-time 2)))
         (distances-possible   (mapcar #'get-dist-from-time button-hold-times))
         (better-distances     (remove-if #'new-best-dist? distances-possible))
         (number-winning-times (1+ (length better-distances))))
    ;; (format t "~%for race with best time of ~a and total time ~a"
    ;;         best-dist-so-far total-time)
    ;; (print distances-possible)
    ;; (print better-distances)
    ;; (print number-winning-times)
    number-winning-times))

(defun solve-part-two ()
  (let* ((input (get-file-contents "./input-p2.txt"))
         (race-time (parse-integer (cadr (str:words (car input)))))
         (race-best-distance (parse-integer (cadr (str:words (cadr input)))))
         (lowest-time-beating-best (get-range-lower-bound race-best-distance race-time)))
    ;; tbh don't know why it wouldn't just be the difference of the total time
    ;; and 2*lowest-time-beating-best, but i think it's because we have to
    ;; remove the endpoint on the right side
    (- race-time (1- lowest-time-beating-best) lowest-time-beating-best)))

;; (solve-part-two)
;; => 34934171 (26 bits, #x2150D9B)
