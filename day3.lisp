;;;https://adventofcode.com/2020/day/3

(defvar *input* "c:/Users/lzakr/projects/cl/input.txt")

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect line))))
    x))

(defun slide (hstep vstep input)
  (loop ))
