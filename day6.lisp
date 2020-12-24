                                        ;https://adventofcode.com/2020/day/6

(defvar *input* "~/Projects/input.txt")

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect line))))
    x))
