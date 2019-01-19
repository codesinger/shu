;;; shu-new-cpp-project.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-project
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

;;; Commentary:

;; Experimental code that may someday be added to shu-cpp-project

;;; Code:

(require 'ert)
(require 'shu-cpp-general)




;;
;;  shu-project-get-file-info
;;
(defmacro shu-project-get-file-info (plist file-name full-name-list)
  "Extract the file information from one entry in shu-cpp-class-list."
  (let ((titem (make-symbol "item"))
        (tflist (make-symbol "flist")))
    `(let ((,titem)
           (,tflist))
       (setq ,titem (car ,plist))
       (setq ,file-name (car ,titem))
       (setq ,tflist (cdr ,titem))
       (setq ,full-name-list (car ,tflist)))
    ))



;;
;;  shu-cpp-project-get-list-counts
;;
(defun shu-cpp-project-get-list-counts (proj-list)
  "PROJ-LIST is an alist whose structure is identical to that of SHU-CPP-CLASS-LIST.
This function returns a list with three items on it: the number of c / cpp files, the
number of h files, and the number of duplicate names found in the list."
  (let ((plist proj-list)
        (c-count   0)
        (h-count   0)
        (dup-count 0)
        (item)
        (file-name)
        (extension)
        (flist)
        (full-name-list))
    (while plist
      (shu-project-get-file-info plist file-name full-name-list)
      (setq extension (file-name-extension file-name))
      (if (member extension shu-cpp-c-extensions)
          (setq c-count (+ c-count (length full-name-list)))
        (setq h-count (+ h-count (length full-name-list))))
      (when (> (length full-name-list) 1)
        (setq dup-count (1+ dup-count)))
      (setq plist (cdr plist)))
    (list c-count h-count dup-count)
    ))



;;
;;  shu-cpp-project-collapse-list
;;
(defun shu-cpp-project-collapse-list (key-list)
  "KEY-LIST is an alist in which the cdr of each item is the unqualified file name
and the car of each item is the fully qualified file name, including the path to
the file.  The output is a different alist in which the car of each item is the
unqualified file name and the cdr of each item is the list of fully qualified
file names to which the unqualified file name refers.

For example, if KEY-LIST contains:

    (\"xxx_mumble.h\" . \"/foo/bar/xxx_mumble.h\")
    (\"xxx_stumble.h . \"/foo/bar/xxx_stumble..h\")
    (\"xxx_stumble.h . \"/boo/baz/xxx_stumble..h\")

then the returned list will contain

    (\"xxx_mumble.h\" . \"/foo/bar/xxx_mumble.h\")
    (\"xxx_stumble.h . \"/foo/bar/xxx_stumble..h\" \"/boo/baz/xxx_stumble..h\")"
  (let ((ilist)
        (c1)
        (file-name)
        (full-name)
        (full-name-list)
        (limit)
        (nname)
        (item)
        (rlist))
    (setq ilist (sort key-list
                      (lambda(obj1 obj2)
                        (string< (car obj1) (car obj2)))))

    ;;
    ;; Go through the sorted list, merging the information
    ;; for duplicate names.
    ;; The car of each item on the list is the unqualified file name.
    ;; The cdr of each item on the list is a list of all of the fully
    ;; qualified file names that share the same unqualified name.
    ;;
    (while ilist
      (setq c1 (car ilist))
      (setq file-name (car c1))
      (setq full-name (cdr c1))
      (setq full-name-list (list full-name))
      (setq limit ilist)
      (while limit
        (setq ilist (cdr ilist))
        (setq limit ilist)            ; terminate loop if list is done
        (when ilist                   ; have at least one more entry
          (setq c1 (car ilist))
          (setq nname (car c1))
          (setq full-name (cdr c1))
          (if (equal file-name nname)     ; Name remains the same
              (setq full-name-list (cons full-name full-name-list))
            (setq limit nil))))
                                        ; Now have all properties for current name
      (when (> (length full-name-list) 1)
        (setq full-name-list (delete-dups full-name-list))
        (when (> (length full-name-list) 1)
          (setq full-name-list (sort full-name-list 'string<))))
      (setq item (cons file-name (list full-name-list)))
      (setq rlist (cons item rlist)))
    (nreverse rlist)
    ))


;;
;;  shu-cpp-project-invert-list
;;
(defun shu-cpp-project-invert-list (proj-list)
  "PROJ-LIST is an alist in which the cdr of each item is the unqualified file name
and the car of each item is the list of fully qualified file names to which
the unqualified name refers.  The returned output is a single list of fully
qualified file names."
  (let ((plist proj-list)
        (file-list)
        (file-name)
        (full-name)
        (full-name-list))
    (while plist
      (shu-project-get-file-info plist file-name full-name-list)
      (while full-name-list
        (setq full-name (car full-name-list))
        (setq file-list (cons full-name file-list))
        (setq full-name-list (cdr full-name-list)))
      (setq plist (cdr plist)))
    (sort file-list 'string<)
    ))


;;
;;  shu-list-c-project
;;
(defun shu-list-c-project ()
  "Insert into the current buffer the names of all of the code files in the
current project."
  (interactive)
  (if (not shu-cpp-class-list)
      (progn
        (message "There is no project to list.")
        (ding))
    (shu-internal-list-c-project shu-cpp-class-list))
  )


;;
;;  shu-cpp-finish-project
;;
(defun shu-cpp-finish-project (&optional key-list)
  "Finish constructing a C project from a user file list.  The input is
KEY-LIST, which is an a-list.  The cdr of each entry is the short (unqualified)
file name.  The cdr of each entry is the fully qualified name.  This alist may
have duplicate short names.  This function produces a new list.  The car of each
item is still the short (unqualified) file name.  The cdr is a list of all of
the fully qualified file names to which the short name maps.  If a user selects
a file that has only one fully qualified file name, we open the file.  But if it
has more than one fully qualified file name, we have to ask the user which one
is wanted."
  (let ((counts)
        (c-count         0)
        (h-count         0)
        (dup-count       0)
        (name-name       "name")
        (occur-name      "occurs")
        (shu-cpp-buffer (get-buffer-create shu-project-cpp-buffer-name)))
    (setq shu-cpp-class-list (shu-cpp-project-collapse-list key-list))
    (setq counts (shu-cpp-project-get-list-counts shu-cpp-class-list))
    (setq c-count (car counts))
    (setq counts (cdr counts))
    (setq h-count (car counts))
    (setq dup-count (cadr counts))
    (setq shu-cpp-project-time (current-time))
    (setq shu-cpp-c-file-count c-count)
    (setq shu-cpp-h-file-count h-count)
    (when (> dup-count 1)
      (setq name-name "names")
      (setq occur-name "occur"))
    (if (= dup-count 0)
        (message "%d C files, %d H files." c-count h-count)
      (message "%d C files, %d H files.  %d %s %s multiple times."
               c-count h-count dup-count name-name occur-name))
    ))




;;
;;  shu-project-split-file-name
;;
(defun shu-project-split-file-name (file-name)
  "Split FILE-NAME into two parts.  The first part is the prefix and the second
part is the short name.  The rules for splitting are as follows:

If the name has no underscores, then the prefix is empty and the short name is
the whole name.

If the name has one underscore in it (e.g., \"abcdef_mumble.cpp\"), then the
prefix is the part before the underscore (\"abcdef\") and the short name is all
of the rest (\"mumble.cpp\").

If the name has more than one underscore in it (e.g., \"x_abcdef_mumble.cpp\"),
then we look as the length of the first part (\"x\").  If its length is one,
then the prefix is the concatenation of the first two parts (\"x_abcdef\"), and
the short name is the rest (\"mumble.cpp\").  If the length of the first part is
not one (e.g., file name is \"lovely_looking_mumble.cpp\"), then the prefix is
the first part (\"lovely\") and the short name is the rest
(\"looking_mumble.cpp\").

After the split, the short name is converted to all lower case.

Return a cons cell of the form (prefix . short-name)"
  (let ((underscore (regexp-quote "_"))
        (pos -1)
        (nlist)
        (pfx "")
        (np)
        (short))
    (setq nlist (split-string file-name underscore t))
    (setq np (length nlist))
    (cond
     ((= 1 np) ;; No underscores in name
      (setq pfx "")
      (setq short file-name))
     ((= 2 np) ;; One underscore in name
      (setq pfx (car nlist))
      (setq short (cadr nlist)))
     (t        ;; At least two underscores in name
      (setq pfx (car nlist))
      (setq nlist (cdr nlist))
      (when (= 1 (length pfx))
        (setq pfx (concat pfx "_" (car nlist)))
        (setq nlist (cdr nlist)))
      (setq short (car nlist))
      (setq nlist (cdr nlist))
      (while nlist
        (setq short (concat short "_" (car nlist)))
        (setq nlist (cdr nlist)))))
    (setq short (downcase short))
    (cons pfx short)
    ))



;;
;;  shu-project-make-short-key-list
;;
(defun shu-project-make-short-key-list (key-list)
  "KEY-LIST is an alist in which the cdr of each item is the unqualified file
name and the car of each item is the fully qualified file name, including the
path to the file.  This function creates two lists.  One is an alist of all of
the file prefixes.  That car of each item is the prefix.  The cdr of each item
is the number of times that prefix was found.  The second is a list similar to
KEY-LIST with all of the file names changed to their equivalent short names.  If
the long and short names are the same, then that item is omitted from the new
list of short names.

Return is a cons cell whose car is the prefix list and whose cdr is the short
name list."
  (let ((kl key-list)
        (file-name)
        (full-name-list)
        (ps)
        (prefix)
        (short-name)
        (short-keys)
        (item)
        (short-list)
        (prefix-list)
        (x)
        (count))
    (while kl
      (shu-project-get-file-info kl file-name full-name-list)
      (setq ps (shu-project-split-file-name file-name))
      (setq prefix (car ps))
      (setq short-name (cdr ps))
      (when (not (string= short-name file-name))
        (setq item (cons short-name (list full-name-list)))
        (setq short-list (cons item short-list))
        (setq item (cons prefix 1))
        (if (not prefix-list)
            (setq prefix-list (cons item prefix-list))
          (setq x (assoc prefix prefix-list))
          (if (not x)
              (setq prefix-list (cons item prefix-list))
            (setq count (cdr x))
            (setq count (1+ count))
            (setcdr x count))))
      (setq kl (cdr kl)))
    (setq short-list (sort short-list (lambda(obj1 obj2)
                                        (string< (car obj1) (car obj2)))))
    (setq prefix-list (sort prefix-list (lambda(obj1 obj2)
                                          (string< (car obj1) (car obj2)))))
    (cons prefix-list short-list)
    ))



;;
;;  shu-internal-list-c-project
;;
(defun shu-internal-list-c-project (proj-list)
  "Insert into the current buffer the names of all of the code files in the
project whose files are in PROJ-LIST."
  (let
      ((plist (shu-cpp-project-invert-list proj-list))
       (full-name))
    (while plist
      (setq full-name (car plist))
      (insert (concat full-name "\n"))
      (setq plist (cdr plist)))
    ))


;;; shu-new-cpp-project.el ends here
