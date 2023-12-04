;;;; Program to solve Advent of Code 2023's Day 2 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 3 Dec 2023
(ql:quickload "str")

;;; part 1: must find the games that would be possible if the bag only has
;;; - 12 red cubes
;;; - 13 green cubes
;;; - 14 blue cubes
;;; and then find the sum of the ids of those games
;;;
;;; logic: in order to find what games are possible, we can track the max number
;;; of times we've seen the elf pull out a given color from his bag for a given
;;; game.

(defun process-data-section (game-data-alist remainder-of-data)
  "TODO doc"
  (if remainder-of-data
      (let ((data-section (str:split "," (car remainder-of-data))))
        (dolist (color-datapoint data-section)
          (destructuring-bind (number-str color-name) (str:words color-datapoint)
            (let* ((color-data (cdr game-data-alist))
                   (matching-color-data (assoc color-name color-data :test 'string=))
                   (highest-occurences-yet (cdr matching-color-data)))
              (setf (cdr matching-color-data) (max highest-occurences-yet
                                                   (parse-integer number-str)))
              (format t "just before maessing with game-data-alist")
              (setf (cdr game-data-alist) (append (remove-if (lambda (alist)
                                                               (string= (car alist)
                                                                        color-name))
                                                             color-data)
                                                  matching-color-data))
              (print matching-color-data))))
        (format t "out of dolist")
        (process-data-section game-data-alist (cdr remainder-of-data)))
      game-data-alist))

(defun process-data-line (line)
  "Returns an association list that contains information about the maximum
number of times we've seen a die for a given game id. Returned alist is of form
(<game-id> . (\"<color>\" . <number-of-times-seen>)."
  (destructuring-bind (id-section-str rest-after-id) (str:split ":" line)
    ;; we use digit-char-p twice because can be used to convert or as a predicate
    (let ((id-as-number (digit-char-p (find-if #'digit-char-p id-section-str))))
      ;; use labels here to let us define a closure or mini function to handle problem.
      (process-data-section (cons id-as-number '(("red"   . 0)
                                                 ("green" . 0)
                                                 ("blue"  . 0)))
                            (str:split ";" rest-after-id)))))




;; (process-data-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
