                                        ;https://adventofcode.com/2020/day/5

(defvar *input* "~/Projects/input.txt")
(defvar *steplist* '(128 64 32 16 8 4 2 8 4 2))

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect line))))
    x))

(defun go-binary (string steplist &Optional (step 0) (row 0) (column 0))
  (if (= step (length steplist)) (list row column)
      (let ((stepvalue (elt steplist step))
            (stringvalue (char string step)))
        (cond ((char-equal stringvalue #\F) (go-binary string steplist (1+ step) row column))
              ((char-equal stringvalue #\B) (go-binary string steplist (1+ step) (+ row (/ stepvalue 2)) column))
              ((char-equal stringvalue #\L) (go-binary string steplist (1+ step) row column))
              ((char-equal stringvalue #\R) (go-binary string steplist (1+ step) row (+ column (/ stepvalue 2))))))))

(defun calc-seat-id (seat)
  (+ 5 (* 8 (first seat))))

(defun find-highest-seat-id (inputlist steps &Optional (highest-id 0))
  (if (equal inputlist '()) highest-id
      (let ((id (calc-seat-id (go-binary (first inputlist) steps))))
        (if (< highest-id id) (find-highest-seat-id (cdr inputlist) steps id)
            (find-highest-seat-id (cdr inputlist) steps highest-id)))))

------------------------------- Part 2 ------------------------------------------------------------------------
