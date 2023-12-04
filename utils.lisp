(ql:quickload "str")

(defun get-file-contents (filename)
  "Return a list of strings where each string line in the file FILENAME."
  (str:lines (str:from-file filename)))
