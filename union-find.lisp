;; A generic solution for connected components. "I have some number of graphs. Is this element connected to
;; this other element? Or are the graphs/sets each element belongs to disjoint?"
;;
;; a 'backwards' tree with pointers from a node to its parent, which lets you union two separate trees together by
;; just taking the shorter one's root and pointing it at the taller one's (or vice versa, but this way preserves log
;; behavior). The path compression optimization seems to just be an extra pass in find(), after you have the result,
;; to re-parent each item along the path to the found root parent so that any future finds() of any of those items
;; will only have one lookup to reach the component root.

(in-package #:com.thejach.aoc25)

(defclass union-find ()
  ((parent-lookup-table
     :accessor uf-parent-lookup-table
     :initform (make-hash-table :test #'equal)
     :documentation "Table that associates a set element (key) to its parent element (val)).")
   (subtree-size-table
     :accessor uf-subtree-size-table
     :initform (make-hash-table :test #'equal)
     :documentation "Table that associates a subtree with root element (key) to its subtree count of elements (val).")))

(defmethod uf-parent-lookup ((uf union-find) key)
  "Looks up key in uf's parent-lookup table.
   If key hasn't been added yet, sets the key's value
   to key itself and returns it."
  (multiple-value-bind (parent present?) (gethash key (uf-parent-lookup-table uf))
    (unless present?
      (setf (gethash key (uf-parent-lookup-table uf)) key)
      (setf parent key))
    parent))

(defmethod uf-subtree-size ((uf union-find) subtree)
  "Gets the size of the specified subtree. If it's not part of the
   lookup map yet, creates an entry and sets it to 1."
  (multiple-value-bind (size present?) (gethash subtree (uf-subtree-size-table uf))
    (unless present?
      (setf (gethash subtree (uf-subtree-size-table uf)) 1)
      (setf size 1))
    size))

(defmethod uf-find ((uf union-find) element)
  "Finds the root parent of the uf structure."
  (let ((parent (uf-parent-lookup uf element)))
    (if (equal parent element)
        element
        (uf-find uf parent))))

(defmethod uf-union ((uf union-find) set1 set2)
  "Makes set1 and set2 subsets of each other (if not already) by parenting the root
   of the smallest set to the other's root."
  (let ((root1 (uf-find uf set1))
        (root2 (uf-find uf set2))
        (parent-table (uf-parent-lookup-table uf))
        (size-table (uf-subtree-size-table uf)))
    (unless (equal root1 root2)
      ; make sure that root2's size is always greatest, swapping if needed,
      ; then parent root1 -> root2
      (if (> (uf-subtree-size uf root1)
             (uf-subtree-size uf root2))
          (rotatef root1 root2))
      (setf (gethash root1 parent-table)
            root2)
      ; update root2's size to be num elements unioned from root1
      (incf (gethash root2 size-table)
            (gethash root1 size-table))))
  nil)

(defmethod uf-connected ((uf union-find) cmp1 cmp2)
  "If cmp1 and cmp2 have the same root in the uf,
   then they are connected in the same subtree."
  (equal (uf-find uf cmp1)
          (uf-find uf cmp2)))

(defun jumpgame-w/-union-find (input)
  (let ((jump-uf (make-instance 'union-find)))
    (flet ((generate-neighbors (idx max-jump-len)
             (loop :for i :from idx :to (+ idx max-jump-len)
                   :collect i)))
      (loop
        :for i :from 0
        :for el :across input
        :do
        (let ((neighbors (generate-neighbors i el)))
          (dolist (neighbor neighbors)
            (uf-union jump-uf i neighbor)))))
    (uf-connected jump-uf 0 (1- (length input)))))


