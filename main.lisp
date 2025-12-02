(in-package #:com.thejach.aoc25)

(defun puzzle-lines (input)
  (remove-if #'str:empty? (cl-ppcre:split "\\n" input)))

;; part 1
(defun update-dial (val amount)
  (mod (+ val amount) 100))

(let ((dial 50)
      (pw 0)
      (input (puzzle-lines *day1-input*)))
  (loop for rotation in input
        do
        (let* ((direction (if (eql (elt rotation 0) #\L) -1 1))
               (amount (* direction (parse-integer (subseq rotation 1)))))
          (setf dial (update-dial dial amount))
          (when (zerop dial)
            (incf pw))))
  pw)

;; part 2
(defun update-dial-counting (val amount &aux (new-val val) (zeros 0))
  "Updates val by amount, returning the new amount as the first element of a list.
   The second element is the number of times the amount crosses over the mod boundary."
  (loop for i below (abs amount)
        with delta = (/ amount (abs amount))
        do
        (incf new-val delta)
        (cond ((zerop new-val) (incf zeros))
              ((minusp new-val) (setf new-val 99))
              ((>= new-val 100) (setf new-val 0)
                                (incf zeros))))
  (list new-val zeros))

; test:
(update-dial-counting 50 -1000) ; -> should be 10

(let ((dial 50)
      (pw 0)
      (input (puzzle-lines *day1-input*)))
  (loop for rotation in input
        do
        (let* ((direction (if (eql (elt rotation 0) #\L) -1 1))
               (amount (* direction (parse-integer (subseq rotation 1))))
               (update (update-dial-counting dial amount)))
          (setf dial (first update))
          (incf pw (second update))))
  pw)


;;;; day 2

;; part 1

(defun ids-from-range-str (range-str)
  "Given range string like '11-22', produces ids (list of numbers from 11 to 22 inclusive)"
  (let ((bounds (cl-ppcre:split "-" range-str)))
    (loop for i from (parse-integer (first bounds)) to (parse-integer (second bounds))
          collect i)))

(defun repeating-invalid-ids (ids)
  "Given a list of numerical ids, returns the invalid ones."
  (loop for id in ids
        for id-str = (format nil "~a" id)
        for id-str-len = (length id-str)
        for half-len = (truncate (/ id-str-len 2))
        when (string-equal (subseq id-str 0 half-len)
                           (subseq id-str half-len))
        collect id))

(let ((ranges (cl-ppcre:split "," *day2-input*)))
  (loop for range in ranges
        for invalid-ids = (repeating-invalid-ids (ids-from-range-str range))
        when invalid-ids
        summing (reduce #'+ invalid-ids)))

;; part 2

(defun twice-repeating-invalid-ids (ids)
  "Now we have to partition each id all the way down to its first numerical character and check if any substring along the way, when repeated the appropriate number of times,
   yields the original string."
  (loop for id in ids
        for id-str = (format nil "~a" id)
        when (any-twice-repeating? id-str)
        collect id))

(defun any-twice-repeating? (id-str)
  (loop for substr-end from 1 to (truncate (/ (length id-str) 2))
        for substr = (subseq id-str 0 substr-end)
        for chars-left = (truncate (/ (length id-str) substr-end))
        for repeated-str = (str:repeat chars-left substr)
        ;do
        ;(format t "substr ~a repeated ~a ~a~%" substr chars-left repeated-str)
        ;))
        when (string-equal id-str repeated-str)
        return t))
(any-twice-repeating? "1010")
(any-twice-repeating? "1188511885")
;len(prefix) * x-repeats = len(whole)

(let ((ranges (cl-ppcre:split "," *day2-input*)))
  (loop for range in ranges
        for invalid-ids = (twice-repeating-invalid-ids (ids-from-range-str range))
        when invalid-ids
        summing (reduce #'+ invalid-ids)))
