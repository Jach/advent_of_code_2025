(in-package #:com.thejach.aoc25)


(defun puzzle-lines (input)
  (remove-if #'str:empty? (cl-ppcre:split "\\n" input)))

(defun update-dial (val amount)
  (mod (+ val amount) 100))

;; part 1
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
