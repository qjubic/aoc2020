;;;https://adventofcode.com/2020/day/2

(defvar *input* "pathhere")

(defun get-list (input)
  (let ((x (with-open-file (in input)
              (loop for line = (read-line in nil)
                    while line
                    collect (substitute #\space #\: (substitute #\space #\- line))))))
    (map 'list #'mk-list x)))

(defun mk-list (strg)
  "Make a list of lists out of a list with strings with 4 parts"
  (let* ((x (multiple-value-bind (a b)(read-from-string strg t nil) (list a b)))
         (y (multiple-value-bind (a b)(read-from-string strg t nil :start (second x)) (list a b)))
         (z (multiple-value-bind (a b)(read-from-string strg t nil :start (second y)) (list a b)))
         (a (multiple-value-bind (a b)(read-from-string strg t nil :start (second z)) (list a b))))
    (list (car x) (car y) (coerce (car z) 'character) (prin1-to-string (car a)))))

(defun validate (passwordrule)
  "Takes a list with a password an its rule and return 1 if valid and 0 if not"
  (let ((x (count (third passwordrule) (fourth passwordrule))))
    (if (and (>= x (first passwordrule))(<= x (second passwordrule))) 1 0)))


(defun day2 (input)
  (let* ((x (get-list input))
        (y (map 'list #'validate x)))
    (reduce #'+ y)))

