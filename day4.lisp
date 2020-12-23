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
  "Checks a string against a list of substrings and returns a number of matches." 
  (labels ((tester ()
             (lambda (x) (if (search x sublist) 1 0))))
    (reduce #'+ (map 'list (tester) testlist))))

(defun verify-passport (data criteria pos sum)
  (let ((part (if (>= pos (length data)) "" (elt data pos)))) ;;if we reach end-of-input we send "" to end recursion
    (if (= (length part) 0) (values sum (1+ pos))
        (verify-passport data criteria (1+ pos) (+ sum (verify-part part criteria))))))

(defun verify-all-passports (passportdata criteria pos res)
  (if (>= pos (1- (length passportdata))) (reduce #'+ res)
      (multiple-value-bind (a b) (verify-passport passportdata criteria pos 0)
        (verify-all-passports passportdata criteria b (cons (if (= a (length criteria)) 1 0) res)))))

(verify-all-passports (get-input *input*) *ver-data* 0 'nil) ;; part 1 

;;---------------------------------- Part 2 -----------------------------------------

(defvar *vdata* '("byr:" "iyr:" "eyr:" "hgt:" "hcl:" "ecl:" "pid:" ))

(defun tester (prefix pos part)
  (labels ((get-element (start element-length) (subseq part (+ pos start) (+ pos start element-length)))
           (is-hexp (char)(if (= 1(count char "0123456789ABCDEFabcdef"))T 'nil))
           (is-x-long (x) (and (not (< (length part)(+ pos x 4))) ;; checks if a thing is x long starting from the end of prefix - 4 being prefix length
                               (or (= (length part)(+ pos x 4))
                                   (string-equal #\space (char part (+ pos x 4)))))))
    (cond ((eq pos 'nil) 'nil)
          ((string-equal prefix "byr:") ;byr (Birth Year) - four digits; at least 1920 and at most 2002.
           (if (is-x-long 4)
               (let ((x (read-from-string (get-element 4 4))))
                 (and (numberp x)
                      (> 2003 x)
                      (< 1919 x)))
               'nil))
          ((string-equal prefix "iyr:") ;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
           (if (is-x-long 4)
               (let ((x (read-from-string (get-element 4 4))))
                 (and (numberp x)
                      (> 2021 x)
                      (< 2009 x)))
               'nil))
          ((string-equal prefix "eyr:") ;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
           (if (is-x-long 4)
               (let ((x (read-from-string (get-element 4 4))))
                 (and (numberp x)
                      (> 2031 x)
                      (< 2019 x)))
               'nil)) 
          ((string-equal prefix "hgt:") ;hgt (Height) - a number followed by either cm or in:
           (or (if (is-x-long 4)
                   (let ((y (read-from-string (get-element 4 2))))
                     (or (and (string-equal "in" (get-element 6 2)) ;;If in, the number must be at least 59 and at most 76.
                              (<= 59 y)
                              (>= 76 y))))
                   'nil) 
               (if (is-x-long 5)
                   (let ((x (read-from-string (get-element 4 3))))
                     (and (string-equal "cm" (get-element 7 2)) ;;If cm, the number must be at least 150 and at most 193.
                          (<= 150 x)
                          (>= 193 x)))
                   'nil))) 
          ((string-equal prefix "hcl:") ;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
           (and (is-x-long 7)
                (char-equal #\# (char part (+ pos 4)))
                (reduce (lambda (x y) (and x y)) (map 'list #'is-hexp (get-element 5 6)))))
          ((string-equal prefix "ecl:") ;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
           (and (is-x-long 3)
                (reduce (lambda (u v) (or u v)) (map 'list (lambda (y) (string-equal (get-element 4 3) y)) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))))
          ((string-equal prefix "pid:") ;pid (Passport ID) - a nine-digit number, including leading zeroes.      
           (and (is-x-long 9)
                (numberp (read-from-string (get-element 4 9)))))
          (t 'nil)))))

(defun get-pdata (part vdata)
  (map 'list (lambda (x) (search x part)) vdata))

(defun verify-part2 (vdata part)
  (loop for x in vdata
        for y in (map 'list (lambda (x) (search x part)) vdata)
        collect (tester x y part)))

(defun verify-passport2 (data vdata pos resultlist)
  "Checks validity until a \"\" comes, create a list with T or nil for each element in vdata which gets reduce to 1 if valid and 0 if not"
  (let ((part (if (>= pos (length data)) "" (elt data pos))))
    (if (= (length part) 0) (values (if (reduce (lambda (u v) (and u v)) resultlist) 1 0) (1+ pos))
        (verify-passport2 data vdata (1+ pos) (map 'list (lambda (u v) (or u v))(verify-part2 vdata part) resultlist)))))

(defun verify-all-passports2 (data vdata pos res)
  (if (>= pos (1- (length data))) (reduce #'+ res)
      (multiple-value-bind (a b) (verify-passport2 data vdata pos (make-sequence 'list (length vdata) :initial-element 'nil))
        (verify-all-passports2 data vdata b (cons a res)))))

(verify-all-passports2 (get-input *input*) *vdata* 0 'nil)
