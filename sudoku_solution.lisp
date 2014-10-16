(in-package :cs325-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XOR Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (solve-xors xor-lists)
;;;   xor-lists: a list of lists of (proposition value) pairs.
;;;   value can be true (T), false (F), or unknown (U).
;;;
;;; solve-xors repeatedly calls update-xor until no U is
;;; replaced with T or F.
;;;
;;; Note: there no checks for inconsistent input, e.g., an xor-list
;;; with more than one T.

(defun solve-xors (xors)
  (loop while (some 'update-xor xors)))

;;; (update-xor xor-list)
;;;   Returns true if it can apply either of the following
;;;   rules:
;;;    - if xor-list has one T value and one more U values,
;;;      set all the U values to F
;;;    - else if xor-list has exactly one U value,
;;;      set the U value to T and return true.
;;;
;;; There are other rules that can be used, by considering
;;; more than one xor-list. For example, if any xor-list has
;;; A and B with U and the rest F, then any other xor-list with
;;; A and B must have all other values F, because either A or B
;;; must be T.

(defun update-xor (xor)
  (let ((t-count (count 't xor :key 'prop-value))
        (u-count (count 'u xor :key 'prop-value)))
    (cond ((and (= t-count 1) (> u-count 0))
           (format t "~%T -> F:~{ ~A~}" xor)  ;; trace output
           (dolist (prop xor)
             (when (eql (prop-value prop) 'u)
               (set-proposition prop 'f)))
           t)
          ((= u-count 1)
           (format t "~%F -> T:~{ ~A~}" xor)
           (set-proposition (find 'u xor :key 'prop-value) 't)
           t)
          (t nil))))

(defun set-proposition (prop value)
  (setf (cdr prop) value))

(defun prop-value (prop) (cdr prop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sudoku Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NOTE: To do real Sudoku, use 9x9 and includes constraints for 3x3 subblocks.

;;; (solve-sudoku initial-values)
;;;     initial-values: list of cell-number choices, e.g., '(a2 f3)
;;;
;;; solve-puzzle
;;;   - creates the 27 possible cell-number propositions, A1, A2, ... I2, I3
;;;   - creates the 27 XOR lists for the row, column and cell constraints
;;;   - calls solve-xors
;;;   - prints the final T and U propositions
;;;   - returns true if no U propositions

(defun solve-sudoku (initial-values)
  (let* ((props (make-sudoku-propositions initial-values))
         (xors (make-sudoku-xors props)))
    (solve-xors xors)
    (let ((t-props (remove 't props :key 'prop-value :test-not 'eql))
          (u-props (remove 'u props :key 'prop-value :test-not 'eql)))
      (format t "~%True:~:{ ~A~}~@[~%Unknown:~:{ ~A~}~]"
        t-props u-props)
      (null u-props))))

;;; Create unique (proposition truth-value) pairs and make XORs of those
;;; so that updating a truth-value is reflected in all XORs immediately.

(defun make-sudoku-propositions (true-props)
   (loop for x in '(a b c d e f g h i)
         append (loop for y from 1 upto 3
                      collect (make-proposition x y true-props))))

(defun make-proposition (cell value true-props)
  (let ((name (make-proposition-name cell value)))
    (cons name (if (member name true-props) 't 'u))))

(defun make-proposition-name (cell value)
  (intern (format nil "~A~A" cell value)))

(defun get-proposition (cell value props)
  (assoc (make-proposition-name cell value) props))


;;; In a regular 9x9 Sudoku, there would also be XORs to say that
;;; every number has to appear in every 3x3 block
(defun make-sudoku-xors (props)
  (append (get-cell-xors props)
          (get-row-xors props)
          (get-column-xors props)))

;;; every cell has one of the 3 numbers
(defun get-cell-xors (props)
  (loop for x in '(a b c d e f g h i)
        collect (loop for y from 1 upto 3
                      collect (get-proposition x y props))))

;;; every number appears in every row
(defun get-row-xors (props)
  (loop for x from 1 upto 3
        append (loop for y in '((a b c) (d e f) (g h i))
                     collect (loop for z in y
                                   collect (get-proposition z x props)))))

;;; every number appears in every column
(defun get-column-xors (props)
  (loop for x from 1 upto 3
        append (loop for y in '((a d g) (b e h) (c f i))
                     collect (loop for z in y
                                   collect (get-proposition z x props)))))