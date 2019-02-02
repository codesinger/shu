

;;
;;  shu-date-date-equal
;;
(defun shu-date-date-equal (lhs rhs)
  "Return t if the date in LHS is equal to the date in RHS."
  (let ((lhsday)
        (rhsday))
    (shu-date-extract-date lhsday lhs)
    (shu-date-extract-date rhsday rhs)
    (= lhsday rhsday)
    ))



;;
;;  shu-date-date-less
;;
(defun shu-date-date-less (lhs rhs)
  "Return t if the date in LHS is less than the date in RHS."
  (let ((lhsday)
        (rhsday))
    (shu-date-extract-date lhsday lhs)
    (shu-date-extract-date rhsday rhs)
    (< lhsday rhsday)
    ))



;;
;;  shu-date-date-greater
;;
(defsubst shu-date-date-greater (lhs rhs)
  "Return t if the date in LHS is greater than the date in RHS."
  (shu-date-date-less rhs lhs)
)



;;
;;  shu-date-time-equal
;;
(defun shu-date-time-equal (lhs rhs)
  "Return t if the time in LHS is less than the time in RHS."
  (let ((lhs-sec)
        (lhs-mic)
        (rhs-sec)
        (rhs-mic))
    (shu-date-extract-time lhs-sec lhs-mic lhs)
    (shu-date-extract-time rhs-sec rhs-mic rhs)
    (and (= lhs-sec rhs-sec)
         (= lhs-mic rhs-mic))
    ))



;;
;;  shu-date-time-less
;;
(defun shu-date-time-less (lhs rhs)
  "Return t if the time in LHS is less than the time in RHS."
  (let ((lhs-sec)
        (lhs-mic)
        (rhs-sec)
        (rhs-mic))
    (shu-date-extract-time lhs-sec lhs-mic lhs)
    (shu-date-extract-time rhs-sec rhs-mic rhs)
    (or (< lhs-sec rhs-sec)
        (and (= lhs-sec rhs-sec) (< lhs-mic rhs-mic)))
    ))




;;
;;  shu-date-time-greater
;;
(defsubst shu-date-time-greater (lhs rhs)
  "Return t if the time in LHS is less than the time in RHS."
  (shu-date-time-less rhs lhs)
    )

(defalias 'shu-date-date< 'shu-date-date-less)
