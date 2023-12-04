;;;; Program to solve Advent of Code 2023's Day 2 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 3 Dec 2023
(ql:quickload "str")
(ql:quickload "alexandria")

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


(let ((my-data (get-all-data-after-id
                " 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")))
  (get-draw-stats-with-max-cube-count my-data))
                                        ; => ("red" 14 "green" 3 "blue" 15)
