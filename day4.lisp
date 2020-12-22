https://adventofcode.com/2020/day/4

(defvar *input* "~/Projects/input.txt")
(defvar *ver-data* '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" )) ;; no cid, since optional 

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect line))))
    x))

(defun verify-part (sublist testlist)
  "Checks a string against a list of substrings and returns a number of matches." ;; not sure about efficiency since tester is created with every call? And its called a lot. Maybe it would be better to pass it as argument?
  (labels ((tester ()
             (lambda (x) (if (search x sublist) 1 0))))
    (reduce #'+ (map 'list (tester) testlist))))

(defun verify-passport (data criteria pos sum)
  (let ((part (if (>= pos (length data)) "" (elt data pos))))
    (if (= (length part) 0) (values sum (1+ pos))
        (verify-passport data criteria (1+ pos) (+ sum (verify-part part criteria))))))

(defun verify-all-passports (passportdata criteria pos res)
  (if (>= pos (1- (length passportdata))) (reduce #'+ res)
      (multiple-value-bind (a b) (verify-passport passportdata criteria pos 0)
        (day4p1 passportdata criteria b (cons (if (= a (length criteria)) 1 0) res)))))

(verify-all-passports (get-input *input*) *ver-data* 0 'nil) ;; part 1 
