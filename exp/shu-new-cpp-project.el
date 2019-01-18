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
;;  shu-test-shu-cpp-project-collapse-list-1
;;
(ert-deftest shu-test-shu-cpp-project-collapse-list-1 ()
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (data
         (list
          (cons "xxx_stumble.h"   "/foo/bar/xxx_stumble.h")
          (cons "xxx_mumble.h"    "/foo/bar/xxx_mumble.h")
          (cons "xxx_stumble.h"   "/boo/baz/xxx_stumble.h")
          ))
        (expected
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))
          ))
        (actual)
        )
    (setq actual (shu-cpp-project-collapse-list data))
    (princ "\nexpected:\n" gb) (princ expected gb) (princ "\n" gb)
    (princ "\nactual:\n" gb) (princ actual gb) (princ "\n" gb)
    (should actual)
    (should (listp actual))
    (should (equal expected actual))


    ))


;;; shu-new-cpp-project.el ends here
