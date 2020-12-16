https://adventofcode.com/2020/day/4

(defvar *input* "c:/Users/lzakr/projects/cl/input.txt")

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect (substitute #\space #\: line))))) ;;evtl not a good idea
    x))

