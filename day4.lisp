https://adventofcode.com/2020/day/4

(defvar *input* "~/Projects/input.txt")
(defvar *pdata* '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" )) ;; no cid, since optional 

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect (substitute #\space #\: line))))) ;;columns are annoying, not necessary
    x))

(defun verify-part (sublist testlist)
  "Checks a string against a list of substrings and returns a number of matches."
  (labels ((tester ()
             (lambda (x) (if (search x sublist) 1 0))))
    (reduce #'+ (map 'list (tester) testlist))))

(defun verify-passport (data pos criteria sum)
  (let ((part (elt data pos)))
    (if (= (length part) 0) (list sum pos)
        (verify-passport data (1+ pos) criteria (+ sum (verify-part part criteria))))))

(defun day4p1 (passportdata criteria)
  (let ((x (length passportdata))
        (y (length criteria))
        ())
    ()))

