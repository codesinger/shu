;;; shu-nvpindex.el --- Shu project code for maintaining and querying an index
;;;                     on an nvplist
;;
;;
;; Copyright (C) 2024 Stewart L. Palmer
;;
;; Package: shu-nvpindex
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; There is a copy of the Gnu General Public license in the file
;; LICENSE in this repository.  You should also have received a copy
;; of the GNU General Public License along with GNU Emacs.  If not,
;; see <http://www.gnu.org/licenses/>.
;;
;;
;; This is a set of functions for maintaining and querying an index that has
;; been built from a set of name value pairs (See nvplist.el).
;;
;; A single set of name value pairs starts with an opening "<" and is
;; terminated by a closing "/>".
;;
;; Here is an example of a set of name value pairs:
;;
;;      < name="Fred email" url=mail.google.com id=freddy@gmail.com pw=secret />
;;
;; The names may be arbitrary but the functions in this file enforce
;; uniqueness of the values of \"name\" and \"url\".  These are key values
;; for the index.
;;
;;; Commentary:


;;; Code:


(require 'shu-nvplist)
(require 'shu-misc)

;;
;; ```
;;  nvplist:
;;
;;   is a list of nvpair, each of which contains:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Value
;;        |
;;        +-------------> Name
;;
;;
;;  item:
;;
;;   -------------------
;;   |        |        |
;;   | Item # |   o-------------->nvplist
;;   |        |        |
;;   -------------------
;;
;;
;;  index:
;;
;;   is a list of key-item, each of which contains:
;;
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> item
;;        |
;;        +-------------> Key
;;
;;  Given a key value, one can use (assoc), (assq), (assoc-default), etc. to find
;;  the item associated with the key.
;; ```
;;




;;
;;  shu-nvpindex-get-item-from-key-item
;;
(defmacro shu-nvpindex-get-item-from-key-item (index-entry)
  "Fetch the ITEM from an INDEX-ENTRY."
    `(cdr ,index-entry)
    )



;;
;;  shu-nvpindex-get-key-from-key-item
;;
(defmacro shu-nvpindex-get-key-from-key-item (index-entry)
  "Fetch the ITEM from an INDEX-ENTRY."
    `(car ,index-entry)
    )



;;
;;  shu-nvpindex-get-nvplist-from-item
;;
(defmacro shu-nvpindex-get-nvplist-from-item (item)
  "Fetch the NVPLIST from an ITEM."
    `(cdr ,item)
    )



;;
;;  shu-nvpindex-get-item-number-from-item
;;
(defmacro shu-nvpindex-get-item-number-from-item (item)
  "Fetch the NVPLIST from an ITEM."
    `(car ,item)
    )



;;
;; shu-nvpindex-parse-file
;;
(defun shu-nvpindex-parse-file (file-name file-type buffer-name)
  "Parse the keyring file and create the in-memory index if the keyring file
contains no duplicate keys."
  (let
      ((gbuf      (get-buffer-create buffer-name))
       (count        )
       (item-list    )
       (ilist        )
       (item         )
       (index        ))
    (princ (format "Shu keyring file is \"%s\".\n" file-name) gbuf)
    (setq item-list (shu-nvplist-parse-file file-name file-type item-list))
    (setq ilist item-list)
    (while ilist
      (setq item (car ilist))
      (setq index (shu-nvpindex-update-index index item))
      (setq ilist (cdr ilist)))
    (setq index (sort index (lambda(t1 t2) (string< (upcase (car t1)) (upcase (car t2))))))
    (if (shu-nvpindex-find-index-duplicates index buffer-name)
        (princ "Index has duplicates\n" gbuf)
      (princ "Index has no duplicates\n" gbuf)
      (setq count (length index))
      (princ (format "Index contains %d entries.\n" count)  gbuf))
    (shu-nvpindex-show-index index buffer-name)
    index
    ))


;;
;;  shu-nvpindex-show-index
;;
(defun shu-nvpindex-show-index (index buffer-name)
  "Print the keyring index"
  (let
      ((gbuf      (get-buffer-create buffer-name))
       (tindex  index)
       (key-item )
       (key     )
       (item    )
       (item-number )
       (item-number-string )
       (count   0))
    (princ "     Item\n" gbuf)
    (princ "    Number   Key ...\n" gbuf)
    (while tindex
      (setq count (1+ count))
      (setq key-item (car tindex))
      (setq key  (car key-item))
      (setq item (cdr key-item))
      (setq item-number (shu-nvplist-get-item-number item))
      (setq item-number-string (shu-fixed-format-num item-number 10))
      (princ (format "%s:  %s\n" item-number-string key) gbuf)
      (setq tindex (cdr tindex)))
    (princ (format "Index contains %d entries.\n" count)  gbuf)
    ))



;;
;;  shu-nvpindex-find-index-duplicates
;;
(defun shu-nvpindex-find-index-duplicates (index buffer-name)
  "Find any duplicates in the keyring index.  When the index is built we filter
duplicate keys for the same item.  But there could be two different items with
the same key.  This function returns TRUE if two or more items have the same
key.  The index must be in sorted order by key value before this function is
called."
  (let
      ((gbuf      (get-buffer-create buffer-name))
       (tindex  index)
       (key-item )
       (key     )
       (item    )
       (item-number )
       (last-key   nil)
       (last-item-number  0)
       (has-duplicate     nil))
    (while tindex
      (setq key-item (car tindex))
      (setq key  (car key-item))
      (setq item (cdr key-item))
      (setq item-number (shu-nvplist-get-item-number item))
      (when last-key
        (when (string= (upcase last-key) (upcase key))
          (setq has-duplicate t)
          (if (= item-number last-item-number)
              (princ (format "Item %d has duplicate key <%s>\n" item-number key) gbuf)
            (princ (format "Items %d and %d share key <%s>\n" last-item-number item-number key) gbuf))))
      (setq last-key key)
      (setq last-item-number item-number)
      (setq tindex (cdr tindex)))
    has-duplicate
    ))


;;
;;  shu-nvpindex-update-index
;;
(defun shu-nvpindex-update-index (index item)
  "Extract the keys from a keyring item and add them to the keyring index."
  (let
      ((vlist   )
       (tlist   )
       (value   )
       (new-value ))
    ;; First add all the names for this item to the index
    (setq vlist (shu-nvplist-get-item-value "name" item))
    (setq index (shu-nvpindex-add-values-to-index index vlist item))

    ;; Now get all the urls
    (setq vlist (shu-nvplist-get-item-value "url" item))
    (setq tlist vlist)

    ;; For each url of the form "www.something", we also add a key of "something"
    ;; This may result in duplicates.  But the function that actually adds the values
    ;; to the index eliminates duplicates.
    (while tlist
      (setq value (car tlist))
      (when (string= (substring (upcase value) 0 4) "WWW.")
        (setq new-value (substring value 4))
        (setq vlist (cons new-value vlist)))
      (setq tlist (cdr tlist)))
    (setq index (shu-nvpindex-add-values-to-index index vlist item))
    index
    ))


;;
;;  shu-nvpindex-in-index
;;
(defun shu-nvpindex-in-index (index item value)
  "Return true if the INDEX already contains the VALUE for this ITEM."
  (let
      ((item-number (shu-nvplist-get-item-number item))
       (tindex  index)
       (done         )
       (pair         )
       (xvalue       )
       (xitem        )
       (xitem-number )
       (got-it    nil))
    (when (not tindex)
      (setq done t))
    (while (not done)
      (setq pair (car tindex))
      (setq xvalue (car pair))
      (setq xitem (cdr pair))
      (setq xitem-number (shu-nvplist-get-item-number xitem))
      (when (and (= item-number xitem-number) (string= xvalue value))
        (setq done t)
        (setq got-it t))
      (setq tindex (cdr tindex))
      (when (not tindex)
        (setq done t)))
    got-it
    ))


;;
;;  shu-nvpindex-add-values-to-index
;;
(defun shu-nvpindex-add-values-to-index (index vlist item)
  "Add a set of keys VLIST to INDEX for ITEM.  Keys within the item are filtered
for duplicates.  But this does not prevent two different items from sharing the
same key, although it would be unusual in a keyring."
  (let
      ((zlist   vlist)
       (value   )
       (pair    ))
    (while zlist
      (setq value (car zlist))
      (when (not (shu-nvpindex-in-index index item value))
        (setq pair (cons value item))
        (setq index (cons pair index)))
      (setq zlist (cdr zlist)))
    index
    ))


;;
;;  shu-nvpindex-values-to-string
;;
(defun shu-nvpindex-values-to-string (values)
  "Turn a list of values into a single string of values separated by slashes."
  (let ((retval "")
        (x       0))
    (while values
      (setq x (1+ x))
      (when (not (= x 1))
        (setq retval (concat retval " / ")))
      (setq retval (concat retval (car values)))
      (setq values (cdr values)))
    retval
    ))


(provide 'shu-nvpindex)


;;; shu-nvpindex.el ends here
