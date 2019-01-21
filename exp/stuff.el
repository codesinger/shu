
(require 'ert)



;;
;;  shu-add-to-alist-1
;;
(defmacro shu-add-to-alist-1 (added-item new-item alist &optional testfn)
  "Add an item to an alist.  The car of NEW-ITEM is a key to be added to the
alist ALIST.  If the key does not already exist in ALIST, NEW-ITEM is added to
ALIST.  ADDED-ITEM is either the item that was added or the item that was
previously there.  If (eq ADDED-ITEM NEW-ITEM), then NEW-ITEM was added to the
list.  If (not (eq ADDED-ITEM NEW-ITEM)), then the key already existed in the
list and ADDED-ITEM is the item that was already on the list with a matching
key."
  `(if (not ,alist)
       (progn
         (setq ,alist (cons ,new-item ,alist))
         (setq ,added-item ,new-item)
         )
     (setq ,added-item (assoc (car ,new-item) ,alist ,testfn))
     (when (not ,added-item)
       (setq ,alist (cons ,new-item ,alist))
       )
     )
  )





;;
;;  shu-add-to-alist
;;
(defmacro shu-add-to-alist (added-item new-item alist &optional testfn)
  "Add an item to an alist.  The car of NEW-ITEM is a key to be added to the
alist ALIST.  If the key does not already exist in ALIST, NEW-ITEM is added to
ALIST.  ADDED-ITEM is either the item that was added or the item that was
previously there.  If (eq ADDED-ITEM NEW-ITEM), then NEW-ITEM was added to the
list.  If (not (eq ADDED-ITEM NEW-ITEM)), then the key already existed in the
list and ADDED-ITEM is the item that was already on the list with a matching
key."
  `(if (not ,alist)
       (progn
         (push ,new-item alist)
         (setq ,added-item ,new-item))
     (setq ,added-item (assoc (car ,new-item) ,alist ,testfn))
     (when (not ,added-item)
       (push ,new-item alist)
       (setq ,added-item ,new-item)))
  )






;;
;;  shu-test-shu-add-to-alist-1
;;
(ert-deftest shu-test-shu-add-to-alist-1 ()
  (let ((alist)
        (item (cons "A" 0))
        (added-item))
    (shu-add-to-alist added-item item alist)
    (should added-item)
    (should (consp added-item))
    (should (stringp (car added-item)))
    (should (numberp (cdr added-item)))
    (should (string= (car item) (car added-item)))
    (should (= (cdr item) (cdr added-item)))
    (should (eq added-item item))
    ))



;;
;;  shu-test-shu-add-to-alist-2
;;
(ert-deftest shu-test-shu-add-to-alist-2 ()
  (let ((alist)
        (item (cons "A" 0))
        (item2 (cons "B" 0))
        (added-item))
    (shu-add-to-alist added-item item alist)
    (shu-add-to-alist added-item item2 alist)
    (should added-item)
    (should (consp added-item))
    (should (stringp (car added-item)))
    (should (numberp (cdr added-item)))
    (should (string= (car item2) (car added-item)))
    (should (= (cdr item2) (cdr added-item)))
    (should (eq added-item item2))
    ))



;;
;;  shu-test-shu-add-to-alist-3
;;
(ert-deftest shu-test-shu-add-to-alist-3 ()
  (let ((alist)
        (item (cons "A" 0))
        (item2 (cons "B" 0))
        (item3 (cons "B" 0))
        (added-item))
    (shu-add-to-alist added-item item alist)
    (shu-add-to-alist added-item item2 alist)
    (shu-add-to-alist added-item item2 alist)
    (should added-item)
    (should (consp added-item))
    (should (stringp (car added-item)))
    (should (numberp (cdr added-item)))
    (should (string= (car item3) (car added-item)))
    (should (= (cdr item3) (cdr added-item)))
    (should (not (eq added-item item3)))
    ))



;;
;;  shu-test-shu-add-to-alist-4
;;
(ert-deftest shu-test-shu-add-to-alist-4 ()
  (let ((alist)
        (items (list "A" "B" "C" "A" "B"))
        (item)
        (count)
        (expected
         (list
          (cons "A" 2)
          (cons "B" 2)
          (cons "C" 1)))
        (actual)
        (x)
        (added-item))
    (while items
      (setq x (car items))
      (setq item (cons x 0))
      (shu-add-to-alist added-item item alist)
      (setq count (cdr added-item))
      (setq count (1+ count))
      (setcdr added-item count)
      (setq items (cdr items)))
    (should alist)
    (should (listp alist))
    (should (= 3 (length alist)))
    (setq actual (sort alist
                       (lambda(obj1 obj2)
                         (string< (car obj1) (car obj2)))))
    (should (equal expected actual))
    ))


;;
;;  aaa
;;
(defun aaa ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (x)
        (added-item)
        (new-item)
        (alist)
        )
    (setq x (macroexpand '(shu-add-to-alist added-item new-item alist)))
    (princ x gb)

    ))
