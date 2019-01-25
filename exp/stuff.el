
;;
;;  aaa
;;
(defun aaa ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (x (current-time))
        (dt)
        (da)
        )
    (princ x gb) (princ "\n" gb)
    (princ
     (format-time-string "%Y-%m-%d %H:%M:%S.%6N" x) gb)
    (princ "\n" gb)
    (princ
     (format-time-string "%FT%T%z"  x) gb)
    (princ "\n\n" gb)
    (setq x (calendar-nth-named-day 1 0 4 1964))
    (princ x gb)
    (princ "\n\n" gb)
    (put 'dt 'shu-date 'datetime)
    (setq dt 1)
    (setq da dt)
    (princ "dt: " gb) (princ (symbol-plist 'dt) gb) (princ "\n" gb)
    (princ "da: " gb) (princ (symbol-plist 'da) gb) (princ "\n" gb)


    ))



;;
;;  shu-invert-list-list
;;
(defun shu-invert-list-list (alist &optional compare-fn)
  "ALIST is an alist in which the car of each item is the key and the cdr of
each item is a list of things associated with the key.  This function inverts
the alist.  The car of each item in the new list is a member of one of the value
lists in ALIST.  The cdr of each item in the new list is a list of all of those
keys that map to the value that is now the key in the new list.

If COMPARE-FN is non-nil, then the lists in the car of each item in the new list are
sorted using COMPARE-FN.

As an example, the following input:

     A -> (X Y Z)
     B -> (Q X Z)
     C -> (P X)

results in the following output being returned:

     P -> (C)
     Q -> (B)

     P -> (C)
     Q -> (B)
     X -> (A B C)
     Y -> (A)
     Z -> (A B)"
  (let ((al alist)
        (item)
        (keys)
        (key)
        (zalues)
        (value)
        (new-item)
        (added-item)
        (new-list)
        (nl))
    (while al
      (setq item (car al))
      (setq key (car item))
      (setq zalues (cdr item))
      (while zalues
        (setq value (car zalues))
        (setq new-item (cons value (list key)))
        (shu-add-to-alist added-item new-item new-list)
        (when (not (eq added-item new-item))
          (setq keys (cdr added-item))
          (push key keys)
          (setcdr added-item keys))
        (setq zalues (cdr zalues)))
      (setq al (cdr al)))
    (when compare-fn
      (setq nl new-list)
      (while nl
        (setq item (car nl))
        (setq values (cdr item))
        (setq values (sort values compare-fn))
        (setcdr item values)
        (setq nl (cdr nl))))
    new-list
    ))




;;
;;  shu-test-shu-invert-list-list-1
;;
(ert-deftest shu-test-shu-invert-list-list-1 ()
  (let ((classes
         (list
          (cons "A" (list "X" "Y" "Z"))
          (cons "B" (list "Q" "X" "Z"))
          (cons "C" (list "P" "X"))))
        (x)
        (actual)
        (expected
         (list
          (cons "P" (list "C"))
          (cons "Q" (list "B"))
          (cons "X" (list "A" "B" "C"))
          (cons "Y" (list "A"))
          (cons "Z" (list "A" "B")))))
    (setq x (shu-invert-list-list classes 'string<))
    (setq actual (sort x (lambda(lhs rhs) (string< (car lhs) (car rhs)))))
    (should (equal expected actual))
    ))




;;
;;  shu-test-shu-invert-list-list-2
;;
(ert-deftest shu-test-shu-invert-list-list-2 ()
  (let ((classes
         (list
          (cons "std"   (list "string" "set" "map" "vector"))
          (cons "world" (list "Hello" "Goodbye" "string" "set" "map"))))
        (x)
        (actual)
        (expected
         (list
          (cons "Goodbye" (list "world"))
          (cons "Hello"   (list "world"))
          (cons "map"     (list "world" "std"))
          (cons "set"     (list "world" "std"))
          (cons "string"  (list "world" "std"))
          (cons "vector"  (list "std")))))
    (setq x (shu-invert-list-list classes))
    (setq actual (sort x (lambda(lhs rhs) (string< (car lhs) (car rhs)))))
    (should (equal expected actual))
    ))



;; ((Goodbye world) (Hello world) (map world std) (set world std) (string world std) (vector std))
