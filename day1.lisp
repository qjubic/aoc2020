;;;https://adventofcode.com/2020/day/1

(defvar *inputpath* "Pathname here")

(defun check (inputlist)
  (let ((x (first inputlist))
        (y (cdr inputlist)))
    (loop for a in y
          if (= 2020 (+ x a))
          return (list a x))))

(defun check2 (inputlist fullinputlist)
  (let ((x (first inputlist))
        (y (cdr inputlist)))
    (loop for a in y
          if (find (- 2020 (+ x a)) fullinputlist)
          return (list a x (find (- 2020 (+ x a)) fullinputlist)))))

(defun checker (inputlist)
  (if inputlist
      (if (check inputlist) (check inputlist)
          (checker (cdr inputlist)))))

(defun checker2 (inputlist fullinputlist)
  (if inputlist
      (if (check2 inputlist fullinputlist)(check2 inputlist fullinputlist)
          (checker2 (cdr inputlist) inputlist))))

(defun day1 (input)
  (let* ((x (with-open-file (in input)
               (loop for line = (read in nil)
                     while line
                     collect line)))
         (y (checker x)))
    (* (first y) (second y))))

(defun day1p2 (input)
  (let* ((x (with-open-file (in input)
              (loop for line = (read in nil)
                    while line
                    collect line)))
         (y (checker2 x x)))
    (* (first y) (second y) (third y))))
