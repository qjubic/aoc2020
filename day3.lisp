;;;https://adventofcode.com/2020/day/3

(defvar *input* "Pathtoinputfile")

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect line))))
    x))

(defun slide-down (input hstep vstep turn trees)
  (labels ((slide (h-pos v-pos)
             (if (char-equal #\# (char (elt input v-pos) h-pos)) 1 0))
           (find-h-pos ()
             (let ((x (* turn hstep))
                   (y (length (first input))))
               (if (< x y) x (mod x y))))
           (find-v-pos ()
             (* turn vstep)))
    (let ((v-pos (find-v-pos)))
      (if (>= v-pos (length input)) (values trees turn)
          (slide-down input hstep vstep (1+ turn) (+ trees (slide (find-h-pos) v-pos)))))))


(defun day3p1 ()
  (slide-down (get-input *input*) 3 1 1 0))
