(in-package #:com.thejach.aoc25)

(defun puzzle-lines (input)
  (remove-if #'str:empty? (cl-ppcre:split "\\n" input)))

(defmacro ns-time (&body form)
  (let ((start (gensym)))
    `(let ((,start (lgame.time:now-seconds)))
       ,@form
       (format t "Time: ~,6fms~%" (* 1e3 (- (lgame.time:now-seconds) ,start))))))

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

(defun // (a b)
  (truncate (/ a b)))

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
        for half-len = (// id-str-len 2)
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
  (loop for substr-end from 1 to (// (length id-str) 2)
        for substr = (subseq id-str 0 substr-end)
        for chars-left = (// (length id-str) substr-end)
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



;;;; day 3

;; part 1

;; let's just do the all-pairs-take-the-top brute approach...

(defun all-pairs (bank)
  (alexandria:flatten
    (loop for i below (length bank)
          collect
          (loop for j from (1+ i) below (length bank)
                collect (parse-integer (uiop:strcat (elt bank i) (elt bank j)))))))

(let ((banks (puzzle-lines *day3-input*)))
  (loop for bank in banks
        for all-pairs-nums = (all-pairs bank)
        sum (first (sort all-pairs-nums #'>))))

;; part 2

(defun best-twelves (bank)
  (let ((length (length bank))
        (best-2-digit 0)
        (best-3-digit 0)
        (best-4-digit 0)
        (best-5-digit 0)
        (best-6-digit 0)
        (best-7-digit 0)
        (best-8-digit 0)
        (best-9-digit 0)
        (best-10-digit 0)
        (best-11-digit 0)
        (best 0))
    (loop for a1 below length do
          (loop for a2 from (1+ a1) below length do
                ; there is no point continuing if "a1 a2" is smaller than the previous "a1 a2"
                (let ((a1a2 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2)))))
                  (when (> a1a2 best-2-digit)
                    (setf best-2-digit a1a2)
                    (loop for a3 from (1+ a2) below length do
                          (let ((a1a2a3 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3)))))
                            (when (> a1a2a3 best-3-digit)
                              (setf best-3-digit a1a2a3)
                              (loop for a4 from (1+ a3) below length do
                                    (let ((a1a2a3a4 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4)))))
                                      (when (> a1a2a3a4 best-4-digit)
                                        (setf best-4-digit a1a2a3a4)
                                        (loop for a5 from (1+ a4) below length do
                                    (let ((a1a2a3a4a5 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4) (aref bank a5)))))
                                      (when (> a1a2a3a4a5 best-5-digit)
                                        (setf best-5-digit a1a2a3a4a5)
                                              (loop for a6 from (1+ a5) below length do
                                    (let ((a1a2a3a4a5a6 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4) (aref bank a5) (aref bank a6)))))
                                      (when (> a1a2a3a4a5a6 best-6-digit)
                                        (setf best-6-digit a1a2a3a4a5a6)
                                                    (loop for a7 from (1+ a6) below length do
                                    (let ((a1a2a3a4a5a6a7 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4) (aref bank a5) (aref bank a6) (aref bank a7)))))
                                      (when (> a1a2a3a4a5a6a7 best-7-digit)
                                        (setf best-7-digit a1a2a3a4a5a6a7)
                                                          (loop for a8 from (1+ a7) below length do
                                    (let ((a1a2a3a4a5a6a7a8 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4) (aref bank a5) (aref bank a6) (aref bank a7) (aref bank a8)))))
                                      (when (> a1a2a3a4a5a6a7a8 best-8-digit)
                                        (setf best-8-digit a1a2a3a4a5a6a7a8)
                                                                (loop for a9 from (1+ a8) below length do
                                    (let ((a1a2a3a4a5a6a7a8a9 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4) (aref bank a5) (aref bank a6) (aref bank a7) (aref bank a8) (aref bank a9)))))
                                      (when (> a1a2a3a4a5a6a7a8a9 best-9-digit)
                                        (setf best-9-digit a1a2a3a4a5a6a7a8a9)
                                                                      (loop for a10 from (1+ a9) below length do
                                    (let ((a1a2a3a4a5a6a7a8a9a10 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4) (aref bank a5) (aref bank a6) (aref bank a7) (aref bank a8) (aref bank a9) (aref bank a10)))))
                                      (when (> a1a2a3a4a5a6a7a8a9a10 best-10-digit)
                                        (setf best-10-digit a1a2a3a4a5a6a7a8a9a10)
                                                                            (loop for a11 from (1+ a10) below length do
                                    (let ((a1a2a3a4a5a6a7a8a9a10a11 (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4) (aref bank a5) (aref bank a6) (aref bank a7) (aref bank a8) (aref bank a9) (aref bank a10) (aref bank a11)))))
                                      (when (> a1a2a3a4a5a6a7a8a9a10a11 best-11-digit)
                                        (setf best-11-digit a1a2a3a4a5a6a7a8a9a10a11)
                                                                                  (loop for a12 from (1+ a11) below length do
                                                                                        (let ((joltage (parse-integer (uiop:strcat (aref bank a1) (aref bank a2) (aref bank a3) (aref bank a4)
                                                                                                                                   (aref bank a5) (aref bank a6) (aref bank a7) (aref bank a8)
                                                                                                                                   (aref bank a9) (aref bank a10) (aref bank a11) (aref bank a12)))))
                                                                                          (when (> joltage best)
                                                                                            (setf best joltage)))))))))))))))))))))))))))))))))))
    best))

; omfg that's ugly...

(setf lparallel:*kernel* (lparallel:make-kernel 24))

(eval-when (:execute)
  (best-twelves (first (puzzle-lines *day3-sample*)))
  (best-twelves (first (puzzle-lines *day3-input*)))
  (time (let ((results (lparallel:pmap 'list #'best-twelves (puzzle-lines *day3-input*))))
          (reduce #'+ results)))
  ; 61 seconds....
  )


;;;; day 4

;; part 1

(defun what-at (lines row col)
  (let ((cell (elt (elt lines row) col)))
    (case cell
      (#\. :empty)
      (#\@ :paper))))

(defun make-grid (rows cols lines)
  (let ((grid (make-array (list rows cols))))
    (loop for row below rows do
          (loop for col below cols do
                (setf (aref grid row col) (what-at lines row col))))
    grid))

(defun within-bounds? (cell bounds)
  (let ((r1 (first cell))
        (c1 (second cell))
        (r2 (first bounds))
        (c2 (second bounds)))
    (and (<= 0 r1 (1- r2))
         (<= 0 c1 (1- c2)))))

(defun neighbors (grid cell)
  "Given (r c) cell, return list of 8 ( (r1 c1) ...) neighbor cells including diagonals, so long as they're also in bounds."
  (let* ((dims (array-dimensions grid))
         (r (first cell))
         (c (second cell))
         (west (list r (1- c)))
         (east (list r (1+ c)))
         (north (list (1- r) c))
         (south (list (1+ r) c))
         (ne (list (1- r) (1+ c)))
         (nw (list (1- r) (1- c)))
         (se (list (1+ r) (1+ c)))
         (sw (list (1+ r) (1- c))))
    (remove-if-not (lambda (el) (within-bounds? el dims))
                   (list north south east west ne nw se sw))))

(let* ((lines (puzzle-lines *day4-input*))
       (grid-rows (length lines))
       (grid-cols (length (first lines)))
       (grid (make-grid grid-rows grid-cols lines))
       (accessible 0))
  (loop for row below grid-rows do
        (loop for col below grid-cols do
              (when (eql :paper (aref grid row col))
                (let* ((neighbors (neighbors grid (list row col)))
                       (paper-neighbors (count :paper (mapcar (lambda (cell) (aref grid (first cell) (second cell))) neighbors))))
                  ;(format t "at r=~a c=~a, neighbors=~a and paper count=~a~%" row col neighbors paper-neighbors)
                  (when (< paper-neighbors 4)
                    (incf accessible))))))
  accessible)


;; part 2

(defun removal-pass (grid)
  "Destructively modifies grid to remove paper that is accessible. Returns the number of papers removed."
  (let* ((dims (array-dimensions grid))
         (rows (first dims))
         (cols (second dims))
         (removed 0))
    (loop for row below rows do
          (loop for col below cols do
                (when (eql :paper (aref grid row col))
                  (let* ((neighbors (neighbors grid (list row col)))
                         (paper-neighbors (count :paper (mapcar (lambda (cell) (aref grid (first cell) (second cell))) neighbors))))
                    (when (< paper-neighbors 4)
                      (setf (aref grid row col) :removed)
                      (incf removed))))))
    removed))

(let* ((lines (puzzle-lines *day4-input*))
       (grid-rows (length lines))
       (grid-cols (length (first lines)))
       (grid (make-grid grid-rows grid-cols lines)))
  (loop for removed = (removal-pass grid)
        until (zerop removed)
        summing removed))


;;;; day 5

;; part 1

(defun convert-fresh-ranges (ranges)
  "List of ranges like 3-5, 10-14, etc. converted to vector of [a b] vectors with a/b being integers."
  (map 'vector (lambda (range)
                 (let* ((nums (cl-ppcre:split "-" range))
                        (lower (parse-integer (first nums)))
                        (upper (parse-integer (second nums))))
                   (assert (<= lower upper))
                   (vector lower upper)))
       ranges))

(defun fresh-id? (ranges id)
  (loop for range across ranges
        when (<= (aref range 0) id (aref range 1))
        return t))

(let* ((puzzle-parts (cl-ppcre:split "\\n\\n" *day5-input*))
       (fresh-ranges (puzzle-lines (first puzzle-parts)))
       (converted-fresh-ranges (convert-fresh-ranges fresh-ranges))
       (available-ids (puzzle-lines (second puzzle-parts))))
  (loop for available in available-ids
        for id = (parse-integer available)
        when (fresh-id? converted-fresh-ranges id)
        sum 1))

;; part 2

(defun unseen-range-count (ranges current-range-id)
  "Quadratic algorithm and fails if the ranges aren't sorted first."
  (let* ((prev-range-id (1- current-range-id))
         (range (aref ranges current-range-id))
         (lower (aref range 0))
         (upper (aref range 1)))
    ; check lower against all previous range's lowers, then check upper, to get the new range (if any)
    (loop for i from 0 to prev-range-id do
          (let* ((prior-range (aref ranges i))
                 (prior-lower (aref prior-range 0))
                 (prior-upper (aref prior-range 1)))
            (when (<= prior-lower lower prior-upper)
              ; new lower is the older's upper + 1
              (setf lower (1+ prior-upper))
              (when (> lower upper) ; whole range was previously in former range
                (return-from unseen-range-count 0)))
            (when (<= prior-lower upper prior-upper)
              (setf upper (1- prior-lower))
              (when (< upper lower)
                (return-from unseen-range-count 0)))))
    (1+ (- upper lower))))

(defun sort-ranges (ranges)
  (sort ranges (lambda (a b)
                 (let ((lower-a (aref a 0))
                        (lower-b (aref b 0)))
                    (if (= lower-a lower-b) ; sort on upper
                        (< (aref a 1) (aref b 1))
                        (< lower-a lower-b))))))

; Still, under a ms...
(ns-time
(let* ((puzzle-parts (cl-ppcre:split "\\n\\n" *day5-input*))
       (fresh-ranges (sort-ranges (convert-fresh-ranges (puzzle-lines (first puzzle-parts))))))
  (loop for range-id below (length fresh-ranges)
        sum (unseen-range-count fresh-ranges range-id)))
)
