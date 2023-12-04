;;;; Program to solve Advent of Code 2023's Day 2 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 3 Dec 2023
(ql:quickload "str")
(ql:quickload "alexandria")
(load "../utils.lisp")

;;; part 1: must find the games that would be possible if the bag only has
;;; - 12 red cubes
;;; - 13 green cubes
;;; - 14 blue cubes
;;; and then find the sum of the ids of those games
;;;
;;; logic: in order to find what games are possible, we can track the max number
;;; of times we've seen the elf pull out a given color from his bag for a given
;;; game.

(defun process-data-section (section)
  (destructuring-bind (cube-count-as-str cube-color) (str:words section)
    (let* ((cube-count-as-int (parse-integer cube-count-as-str)))
      (cons cube-color cube-count-as-int))))

(defun get-all-data-after-id (draw-info)
  "Returns an alist containing data on all occurrences of the drawing of the
given colors."
  (let* ((all-fields (str:split ";" draw-info))
         (data-from-all-fields (alexandria:flatten
                                (mapcar (lambda (field)
                                          (mapcar #'process-data-section
                                                  (str:split "," field)))
                                        all-fields))))
    (alexandria:plist-alist data-from-all-fields)))

                                        ; => (("blue" . 3) ("red" . 4) ("red" . 1) ("green" . 2) ("blue" . 6) ("green" . 2))
(get-all-data-after-id " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(defun get-draw-stats-with-max-cube-count (all-game-data-alist)
  "Returns a plist of pairs of colors and how many times those colors appeared
in a given line based on the data for said line passed in as
ALL-GAME-DATA-ALIST. Caller can rely on plist being returned in the form of
(\"red\" NUMBER-OF-RED-OCCURENCES \"green\" NUMBER-OF-GREEN-OCCURENCES \"blue\"
NUMBER-OF-BLUE-OCCURENCES)."
  (labels ((get-maxes (datapoints-left-to-check maxes-so-far-plist)
             (let* ((remaining-datapoints (cdr datapoints-left-to-check))
                    (working-datapoint    (car datapoints-left-to-check))
                    (color-in-question    (car working-datapoint))
                    (number-in-question   (cdr working-datapoint)))
               (destructuring-bind (color-1 count-1 color-2 count-2 color-3 count-3) maxes-so-far-plist
                 (if working-datapoint
                     (cond ((string= color-1 color-in-question)
                            (get-maxes remaining-datapoints
                                       (list color-1 (max number-in-question count-1)
                                             color-2 count-2
                                             color-3 count-3)))
                           ((string= color-2 color-in-question)
                            (get-maxes remaining-datapoints
                                       (list color-1 count-1
                                             color-2 (max number-in-question count-2)
                                             color-3 count-3)))
                           ((string= color-3 color-in-question)
                            (get-maxes remaining-datapoints
                                       (list color-1 count-1
                                             color-2 count-2
                                             color-3 (max number-in-question count-3)))))
                     maxes-so-far-plist)))))
    (get-maxes all-game-data-alist (list "red"   0
                                         "green" 0
                                         "blue"  0))))


;; (let ((my-data (get-all-data-after-id
;;                 " 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")))
;;   (get-draw-stats-with-max-cube-count my-data))
                                        ; => ("red" 14 "green" 3 "blue" 15)

(defparameter *max-red*   12)
(defparameter *max-green* 13)
(defparameter *max-blue*  14)

(defun is-a-possible-game-p (game-data red-capacity green-capacity blue-capacity)
  "Returns true if game data has less occurrences of red, green, and blue than
RED-CAPACITY, GREEN-CAPACITY, and BLUE-CAPACITY, respectively. Input is
expected to be in the form
(GAME-ID (\"red\" RED-OCCURRENCES \"green\" GREEN-OCCURRENCES \"blue\" BLUE-OCCURRENCES))."
  (let ((data-without-id (cadr game-data)))
    (destructuring-bind (_ red-seen _ green-seen _ blue-seen) data-without-id
      (and (<= red-seen red-capacity)
           (<= green-seen green-capacity)
           (<= blue-seen blue-capacity)))))


(defun solve-part-one ()
  (let* ((input-file "./input-p1-p2.txt")
         (file-lines (get-file-contents input-file))
         ;; all game stats is list of game id and stats of form (GAME-ID (<STATS>)),
         ;; see docs in GET-DRAW-STATS-WITH-MAX-CUBE-COUNT for what format of
         ;; format of stats is
         (all-game-stats (mapcar (lambda (line)
                                   (destructuring-bind (game-id game-information) (str:split ":" line)
                                     (let* ((game-data (get-all-data-after-id game-information))
                                            (color-occurrence-stats (get-draw-stats-with-max-cube-count game-data)))
                                       ;; word "Game" is 4 letters long + 1 space, index is 1 less
                                       (list (parse-integer game-id :start 4)
                                             color-occurrence-stats))))
                                 file-lines))
         (sum-of-possible-game-ids (reduce (lambda (first-it second-it)
                                             (+ (if (numberp first-it)
                                                    first-it
                                                    (if (is-a-possible-game-p first-it *max-red* *max-green* *max-blue*)
                                                        (car first-it)
                                                        0))
                                                (if (is-a-possible-game-p second-it *max-red* *max-green* *max-blue*)
                                                    (car second-it)
                                                    0)))
                                           all-game-stats)))

    sum-of-possible-game-ids))

;; (solve-part-one))

;;; part 2: find the sum of the powers of the sets of the least amount of cubes
;;; necessary to make a game possible.
;;;
;;; in other words (in my understanding of the problem), i have to
;;; 1. find the fewest number of cubes of each color that would make each of the
;;;    games possible
;;; 2. then for each set of three numbers i got from finding 1.,
;;;    compute the power of the set, which we find by multiplying all three
;;;    numbers together
;;; 3. then for the final answer find the sum of all numbers we got from 2.
;;;
;;; my initial thoughts is that it seems doable because i have a function that
;;; gives me the highest number of cubes pulled of all colors in a given game,
;;; so i should be able to work with that.

(defun solve-part-two ()
  (let* ((input-file "./input-p1-p2.txt")
         (file-lines (get-file-contents input-file))
         (game-information (mapcar (lambda (line) (cadr (str:split ":" line)))
                                   file-lines))
         (min-cubes-for-each-game (mapcar (lambda (info-line)
                                            (let* ((draw-data-alist (get-all-data-after-id info-line))
                                                   (color-occurrence-data-plist
                                                     (get-draw-stats-with-max-cube-count draw-data-alist)))
                                              (remove-if 'stringp color-occurrence-data-plist)))
                                          game-information))
         (set-of-powers (mapcar (lambda (set) (reduce '* set))
                                min-cubes-for-each-game)))
    (reduce '+ set-of-powers)))

;; (solve-part-two)
