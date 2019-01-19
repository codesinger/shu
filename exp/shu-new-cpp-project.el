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
  "Input is an alist in which the cdr of each item is the unqualified file name
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
  (let (
        (ilist)
        (c1)
        (file-name)
        (full-name)
        (full-name-list)
        (limit)
        (nname)
        (item)
        (rlist)
        )
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
          (setq full-name-list (sort full-name-list 'string<))
          )
        )
      (setq item (cons file-name (list full-name-list)))
      (setq rlist (cons item rlist))
      )
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
  (let (
        (gb (get-buffer-create "**boo**"))
        (plist proj-list)
        (file-list)
        (file-name)
        (full-name)
        (full-name-list)
        (debug-on-error t)
        )
    (while plist
      (shu-project-get-file-info plist file-name full-name-list)
      (while full-name-list
        (setq full-name (car full-name-list))
        (setq file-list (cons full-name file-list))
        (princ "\nfile-list:\n" gb) (princ file-list gb) (princ "\n" gb)
        (setq full-name-list (cdr full-name-list))
        )
      (setq plist (cdr plist))
      )
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
;;  shu-internal-list-c-project
;;
(defun shu-internal-list-c-project (proj-list)
  "Insert into the current buffer the names of all of the code files in the
project whose files are in PROJ-LIST."
  (let
      (
       (plist (shu-cpp-project-invert-list proj-list))
       (full-name)
       )
    (while plist
      (setq full-name (car plist))
      (insert (concat full-name "\n"))
      (setq plist (cdr plist))
      )
    ))



;;
;;  shu-test-shu-cpp-project-collapse-list-1
;;
(ert-deftest shu-test-shu-cpp-project-collapse-list-1 ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (data
         (list
          (cons "xxx_stumble.h"   "/foo/bar/xxx_stumble.h")
          (cons "xxx_mumble.h"    "/foo/bar/xxx_mumble.h")
          (cons "xxx_stumble.h"   "/boo/baz/xxx_stumble.h")))
        (expected
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))))
        (actual))
    (setq actual (shu-cpp-project-collapse-list data))
    (princ "\nexpected:\n" gb) (princ expected gb) (princ "\n" gb)
    (princ "\nactual:\n" gb) (princ actual gb) (princ "\n" gb)
    (should actual)
    (should (listp actual))
    (should (equal expected actual))
    (shu-cpp-project-get-list-counts expected)

    ))



;;
;;  shu-test-shu-cpp-project-invert-list-1
;;
(ert-deftest shu-test-shu-cpp-project-invert-list-1 ()
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (data
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))
          ))
        (expected
         (list
          "/boo/baz/xxx_stumble.h"
          "/foo/bar/xxx_mumble.h"
          "/foo/bar/xxx_stumble.h"
          ))
        (actual)
        )
    (princ "\n\nexpected:\n" gb) (princ expected gb) (princ "\n" gb)
    (setq actual (shu-cpp-project-invert-list data))
    (should actual)
    (princ "\n\nactual:\n" gb) (princ actual gb) (princ "\n" gb)
    (should (listp actual))
    (should (equal actual expected))
    ))



;;
;;  shu-test-shu-cpp-project-get-list-counts-1
;;
(ert-deftest shu-test-shu-cpp-project-get-list-counts-1 ()
  "Doc string."
  (let (
        (data
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))))
        (counts)
        (c-count -1)
        (h-count -1)
        (dup-count -1)
        )
    (setq counts (shu-cpp-project-get-list-counts data))
    (setq c-count (car counts))
    (setq counts (cdr counts))
    (setq h-count (car counts))
    (setq dup-count (cadr counts))
    (should (= 0 c-count))
    (should (= 3 h-count))
    (should (= 1 dup-count))
    ))



;;
;;  shu-test-shu-cpp-project-get-list-counts-2
;;
(ert-deftest shu-test-shu-cpp-project-get-list-counts-2 ()
  "Doc string."
  (let (
        (data
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_mumble.cpp"  (list (list "/foo/bar/xxx_mumble.cpp")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))
          (cons "xxx_stumble.cpp" (list (list "/boo/baz/xxx_stumble.cpp"
                                              "/foo/bar/xxx_stumble.cpp")))))
        (counts)
        (c-count -1)
        (h-count -1)
        (dup-count -1))
    (setq counts (shu-cpp-project-get-list-counts data))
    (setq c-count (car counts))
    (setq counts (cdr counts))
    (setq h-count (car counts))
    (setq dup-count (cadr counts))
    (should (= 3 c-count))
    (should (= 3 h-count))
    (should (= 2 dup-count))
    ))


;;; shu-new-cpp-project.el ends here
