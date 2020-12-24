                                        ;https://adventofcode.com/2020/day/5

(defvar *input* "~/Projects/input.txt")

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect line))))
    x))

(defun calc-seat-pos (string &Optional (step 0) (row 0) (column 0))
  (let ((steplist '(128 64 32 16 8 4 2 8 4 2)))
    (if (= step (length steplist)) (list row column)
        (let ((stepvalue (elt steplist step))
              (stringvalue (char string step)))
          (cond ((char-equal stringvalue #\F) (calc-seat-pos string (1+ step) row column))
                ((char-equal stringvalue #\B) (calc-seat-pos string (1+ step) (+ row (/ stepvalue 2)) column))
                ((char-equal stringvalue #\L) (calc-seat-pos string (1+ step) row column))
                ((char-equal stringvalue #\R) (calc-seat-pos string (1+ step) row (+ column (/ stepvalue 2)))))))))

(defun calc-seat-id (seat-pos)
  (+ (second seat-pos) (* 8 (first seat-pos))))

(defun find-highest-seat-id (inputlist  &Optional (highest-id 0))
  (if (equal inputlist '()) highest-id
      (let ((id (calc-seat-id (calc-seat-pos (first inputlist)))))
        (if (< highest-id id) (find-highest-seat-id (cdr inputlist) id)
            (find-highest-seat-id (cdr inputlist) highest-id)))))

------------------------------- Part 2 ------------------------------------------------------------------------
(defun find-seat (input)
  (let ((id-list (sort (map 'list #'calc-seat-id (map 'list #'calc-seat-pos input)) #'<)))
    (tester id-list))))

(defun tester (slist)
  (if (equal (first slist) (- (second slist) 2 )) (1+ (first slist))
      (tester (cdr slist))))
