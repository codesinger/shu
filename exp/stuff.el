



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
         (push new-item alist)
         (setq ,added-item ,new-item)
         )
      (setq ,added-item (assoc (car ,new-item) ,alist ,testfn))
      (when (not ,added-item)
        (push new-item alist)
        )
      )
    )


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
