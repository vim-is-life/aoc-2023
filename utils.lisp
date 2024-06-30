(defun get-file-contents (filename)
  "Return a list of strings where each string line in the file FILENAME."
  (uiop:read-file-lines filename))

(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))
