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

;;---------------------------------- Part 2 -----------------------------------------

;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.

(defvar *vdata* '("byr:" "iyr:" "eyr:" "hgt:" "hcl:" "ecl:" "pid:" ))

                                        ; TODO: make an internal function for (subseq part (+ pos 4..... this shows up a lot.
                                        ; TODO: make an internal function for read-from-string
(defun tester (prefix pos part)
  (cond ((eq pos 'nil) 'nil)
        ((string-equal prefix "byr:") ;byr (Birth Year) - four digits; at least 1920 and at most 2002.
         (let ((x (read-from-string (subseq part (+ pos 4) (+ pos 8)))))
           (and (or (= (length part)(+ pos 8))
                    (string-equal #\space (char part (+ pos 8)))) ;;This checks if number is 4 digits
                (numberp x)(> 2003 x)(< 1919 x))))
        ((string-equal prefix "iyr:") ;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
         (let ((x (read-from-string (subseq part (+ pos 4) (+ pos 8)))))
           (and (or (= (length part)(+ pos 8))
                    (string-equal #\space (char part (+ pos 8)))) 
                (numberp x)
                (> 2021 x)
                (< 2009 x))))
        ((string-equal prefix "eyr:") ;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
         (let ((x (read-from-string (subseq part (+ pos 4) (+ pos 8)))))
                                       (and (or (<= (length part)(+ pos 8))
                                                (string-equal #\space (char part (+ pos 8)))) 
                                            (numberp x)
                                            (> 2031 x)
                                            (< 2019 x)))) 
        ((string-equal prefix "hgt:") ;hgt (Height) - a number followed by either cm or in:
         (let ((x (read-from-string (subseq part (+ pos 4) (+ pos 7))))
               (y (read-from-string (subseq part (+ pos 4) (+ pos 6)))))
           (or (and (string-equal "in" (subseq part (+ pos 6) (+ pos 8)))
                   (<= 59 y)
                   (>= 76 y))
               (and (string-equal "cm" (subseq part (+ pos 7) (+ pos 9)))
                    (<= 150 x)
                    (>= 193 x))))) 
        ((string-equal prefix "hcl:") ;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
         (let ((x (subseq part (+ pos 5) (+ pos 11))))
           (and (or (<= (length part)(+ pos 12))
                    (string-equal #\space (subseq part (+ pos 11) (+ pos 12))))
                (char-equal #\# (char part (+ pos 4)))
                (reduce (lambda (x y) (and x y)) (map 'list #'is-hexp x)))))
        ((string-equal prefix "ecl:") ;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
         (reduce (lambda (u v) (or u v)) (map 'list (lambda (y) (string-equal (subseq part (+ pos 4)(+ pos 7)) y)) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))
        ((string-equal prefix "pid:")'nil) ;pid (Passport ID) - a nine-digit number, including leading zeroes.      
        (t 'oops)))

(defun get-pdata (part vdata)
  (map 'list (lambda (x) (search x part)) vdata))

(defun verify-part2 (vdata part)
  (loop for x in vdata
        for y in (map 'list (lambda (x) (search x part)) vdata)
        collect (tester x y part)))

(defun is-hexp (char)
  (if (= 1(count char "0123456789ABCDEFabcdef"))T 'nil))

(defun is-eclp (x)
  (reduce (lambda (u v) (or u v)) (map 'list (lambda (y) (string-equal x y)) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))

