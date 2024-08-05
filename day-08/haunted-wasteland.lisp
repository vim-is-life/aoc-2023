;;;; Program to solve    Advent of Code 2023's Day 5 problem.
;;;; To solve part 1, call (solve-part-one) in the REPL, and for two you can
;;;; call (solve-part-two).
;;;; Author: vim-is-life
;;;; Date: 10 Dec 2023

(load "~/quicklisp/setup.lisp")
(load "../utils.lisp")
(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")

(defparameter *ex-1-input* (uiop:read-file-lines "input-ex-1.txt"))
(defparameter *ex-2-input* (uiop:read-file-lines "input-ex-2.txt"))
(defparameter *puzzle-input* (uiop:read-file-lines "input.txt"))
(defparameter *node-regex* (ppcre:create-scanner "[A-Z]{3}"))

;;; part 1: find count steps it takes to get from first node (AAA) to last node (ZZZ)
(defun parse-lines-to-map (search-regex lines-with-map-data)
  (let ((map-data (make-hash-table :size (length lines-with-map-data))))
    (dolist (line lines-with-map-data map-data)
      (destructuring-bind (node instruc-left instruc-right)
          (ppcre:all-matches-as-strings search-regex line)
        (let ((node-processed (read-from-string node))
              (left-processed (read-from-string instruc-left))
              (right-processed (read-from-string instruc-right)))
          (setf (gethash node-processed map-data) (list left-processed right-processed)))))))

(defun get-next-direction (desired-idx directions)
  "Return the next direction given the current direction came from index
DESIRED-IDX, with wraparound."
  (let* ((adjusted-idx (mod desired-idx (length directions))))
    (char directions adjusted-idx)))

;; (assert (eql #\R (get-next-direction 5 "LLR")))

(defun get-next-node (map current-direction current-node)
  ;; (print "")
  ;; (print current-node)
  ;; (print "options")
  ;; (print (gethash current-node map))
  ;; (print "choose")
  (destructuring-bind (fst lst) (gethash current-node map)
    (case current-direction
      ;; (#\L (print fst))
      ;; (#\R (print lst))
      (#\L fst)
      (#\R lst))))

(defun get-count-steps-to-end (map directions starting-node)
  "Return the count of steps it takes to get to the end (the node ZZZ) given the
node we start with is FIRST-NODE and MAP is a hash table mapping nodes (ie 'ZZZ)
to instructions, which is an list of nodes to go to (ie '(ABQ DBZ))."
  (do* ((direction-idx 0 (1+ direction-idx))
        (direction (char directions 0) (get-next-direction direction-idx directions))
        (current-node starting-node))
       ((eql current-node 'ZZZ) direction-idx)
    (setf current-node (get-next-node map direction current-node))))


(defun solve-part-one (lines)
  ;; (declare (optimize (speed 3) (safety 0)))
  (let* ((directions (car lines))
         (lines-with-map-data (cddr lines))
         (first-node 'AAA)
         (map-data (parse-lines-to-map *node-regex* lines-with-map-data)))
    (get-count-steps-to-end map-data directions first-node)))

;; (solve-part-one *puzzle-input*)
;; => 12737 (14 bits, #x31C1)

;;; part 2: given that you start at every node ending in A, how many steps does
;;; it take for you to get to a node ending in Z with every node that you
;;; started with?
;;; note: this appears to be an LCM problem because in the example it starts
;;; with two nodes and it takes the first 2 steps and then the second 3, and it
;;; takes them both 6 steps combined.
(defparameter *ex-3-input* (uiop:read-file-lines "input-ex-3.txt"))

(defun start-or-end-node-p (regex node)
  "Return the node if it ends with A, otherwise return nil"
  (when (ppcre:scan regex (string node))
    node))

(defun get-count-steps-to-end-p2 (map end-regex directions starting-node)
  "Return the count of steps it takes to get to the end (the node ZZZ) given the
node we start with is FIRST-NODE and MAP is a hash table mapping nodes (ie 'ZZZ)
to instructions, which is an list of nodes to go to (ie '(ABQ DBZ))."
  (do* ((direction-idx 0 (1+ direction-idx))
        (direction (char directions 0) (get-next-direction direction-idx directions))
        (current-node starting-node))
       ((start-or-end-node-p end-regex current-node) direction-idx)
    (setf current-node (get-next-node map direction current-node))))

(defun solve-part-two (lines)
  (let* ((directions (car lines))
         (lines-with-map-data (cddr lines))
         (map-data (parse-lines-to-map *node-regex* lines-with-map-data))
         (starting-node-regex (ppcre:create-scanner "..A"))
         (ending-node-regex   (ppcre:create-scanner "..Z"))
         (starting-nodes (remove nil (mapcar (lambda (k) (start-or-end-node-p starting-node-regex k))
                                             (alexandria:hash-table-keys map-data))))
         (steps-for-each-starting-node (mapcar (lambda (node) (get-count-steps-to-end-p2 map-data ending-node-regex directions node))
                                               starting-nodes)))
    (apply #'lcm steps-for-each-starting-node)))

;; (solve-part-two *puzzle-input*)
;; => 9064949303801 (44 bits, #x83E991549F9)
;; => (15989 14363 12737 21409 11653 18157)
;; => (LTA HHA AAA QGA XKA LPA)
