                                        ;https://adventofcode.com/2020/day/6

(defvar *input* "~/Projects/input.txt")
(defvar *input2* "~/Projects/input2.txt")

(defun get-input (input)
  (let ((x (with-open-file (in input)
             (loop for line = (read-line in nil)
                   while line
                   collect line))))
    x))

(defun get-unique-group-answers (inputlist &Optional (pos 0) (resultstring ""))
  "Merges all answers by one group an removes duplicates, returns sum of answers and last position in inputlist"
  (if (or (>= pos (length inputlist))
          (= 0 (length (elt inputlist pos)))) (values (length resultstring) (1+ pos))
      (get-unique-group-answers inputlist (1+ pos) (remove-duplicates (sort (format 'nil "~A~A" (elt inputlist pos) resultstring) #'char-lessp)))))

(defun sum-of-all-groups-yes (inputlist &Optional (start 0) (sum 0))
  (if (>= start (length inputlist)) sum
      (multiple-value-bind (a b) (get-unique-group-answers inputlist start)
        (sum-of-all-groups-yes inputlist b (+ sum a)))))

-------------------------------- Part 2 --------------------------------------------
(defun get-common-group-answers (inputlist &Optional  (pos 0) (resultstring "") (members 0))
  "Merges all answers by one group and removes uniques, returns sum of answers and last position in inputlist"
  (if (or (>= pos (length inputlist))
          (= 0 (length (elt inputlist pos))))
      (labels ((remove-unique (sequence) (remove-if (lambda (x) (> members (count x sequence))) sequence)))
        (values (length (remove-duplicates (remove-unique resultstring))) (1+ pos))) 
      (get-common-group-answers inputlist (1+ pos) (sort (format 'nil "~A~A" (elt inputlist pos) resultstring) #'char-lessp) (1+ members))))

(defun sum-all-common-groups-yes (inputlist &Optional (start 0) (sum 0))
  (if (>= start (length inputlist)) sum
      (multiple-value-bind (a b) (get-common-group-answers inputlist start)
        (sum-all-common-groups-yes inputlist b (+ sum a)))))
