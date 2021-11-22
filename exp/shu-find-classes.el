;;; shu-find-classes.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2021 Stewart L. Palmer
;;
;; Package: shu-find-classes
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This is also not part of the shu elisp pckage.                                      ;
;; This is experimental code that is unlikely to become part of the
;; shu slisp package in its current form.
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

;;
;; Some experimental code for extracting fully qualified class names
;; from a tokenized list
;;


;;
;;  zzz
;;
(defun zzz ()
  "Open a C++ file and invoke this function.  Results in **shu-unit-tests**"
  (interactive)
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (token-list)
        (rlist)
        (ret-val)
        (glist)
        (debug-on-error t)
        )
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-all-search-match-tokens rlist shu-cpp-match-classname-forms token-list))
      (if (not ret-val)
          (message "%s" "No qualified names found.")
        (setq rlist (cdr ret-val))
        (if (not rlist)
            (message "Returned name list is empty.")
          (setq rlist (nreverse rlist))
          (princ "\n\n====================================================\n\n" gb)
          (shu-cpp-tokenize-show-list rlist)
          (setq glist (shu-gather-qualified-names rlist))
          (shu-show-glist glist)
          (setq rlist (shu-make-reduced-name-list glist 1))
          (shu-show-reduced-list rlist)
            )
          )

    ))



;;
;;  shu-show-glist
;;
(defun shu-show-glist (glist-in)
  "Doc string."
  (interactive)
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (gather)
        (token)
        (name)
        (glist)
        )
    (while glist-in
      (setq gather (car glist-in))
      (push gather glist)
      (setq glist-in (cdr glist-in))
      )

    (princ "\n\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n" gb)
    (setq glist (sort glist (lambda(lgather rgather)
                             (let (
                                    (less)
                                    )
                              (if (string= (car lgather) (car rgather))
                                  (setq less (string< (cadr lgather) (cadr rgather)))
                                (setq less (string< (car lgather) (car rgather)))
                                  )
                             less
                              )
                              )
                      )
          )
    (while glist
      (setq gather (car glist))
      (setq name "")
      (while gather
        (setq token (car gather))
        (setq name (concat name token))
        (when (cdr gather)
          (setq name (concat name "::"))
          )
        (setq gather (cdr gather))
        )
      (princ (concat name "\n") gb)
      (setq glist (cdr glist))
      )
    ))


;;
;;  shu-show-reduced-list
;;
(defun shu-show-reduced-list (reduced-list)
  "Doc string."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (rlist reduced-list)
        (kv)
        (qname)
        (qlist)
        (gather)
        )
    (princ "\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n" gb)
    (while rlist
      (setq kv (car rlist))
      (setq qname (car kv))
      (setq qlist (cdr kv))
      (princ (concat qname "\n") gb)
      (while qlist
        (setq gather (car qlist))
        (princ (concat "      " (shu-make-qualified-name gather) "\n" )gb)
        (setq qlist (cdr qlist))
        )
      (setq rlist (cdr rlist))
      )

    ))




;;
;;  shu-gather-qualified-names
;;
(defun shu-gather-qualified-names (rlist)
  "RLIST is a list of TOKEN-INFO representing unquoted tokens that are separated
from each other by instances of \"::\".  These potentially represent fully
qualified C++ names.  If names are separated by instances of \"::\", they are
qualified names.  But if there are two names in a row with no intervening
instance of \"::\", then the first of the names is the last part of a fully
qualified name and the second of the names in the first part of the next fully
qualified name.  This function returns a list of the fully qualified names.
Each names is represented by a list of its parts.

For example, the following list represents two fully qualified names:

       1. \"std\"
       2. \"::\"
       3. \"string\"
       4. \"std\"
       5. \"::\"
       6. \"vector\"

The names, of course, are \"std::string\" and \"std::vector\".  They would be
returned as a list that contains two lists.  The first item on the list is the
list

       1. \"std\"
       2. \"string\"

The second item on the list is the list

       1. \"std\"
       2. \"vector\""
  (let ((token-info)
        (token)
        (token-type)
        (last-token)
        (last-token-type)
        (gathered)
        (glist))
    (when rlist
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq last-token token)
      (setq last-token-type token-type)
      (push token gathered)
      (setq rlist (cdr rlist))
      (while rlist
        (setq token-info (car rlist))
        (setq token (shu-cpp-token-extract-token token-info))
        (setq token-type (shu-cpp-token-extract-type token-info))
        (when (= token-type shu-cpp-token-type-uq)
          (if (string= last-token "::")
              (push token gathered)
            (setq gathered (nreverse gathered))
            (push gathered glist)
            (setq gathered nil)
            (push token gathered)))
        (setq last-token token)
        (setq last-token-type token-type)
        (setq rlist (cdr rlist))))
    glist
    ))



;;
;;  shu-make-qualified-name
;;
(defun shu-make-qualified-name (gather &optional level)
  "GATHER is a list of the parts of a C++ qualified name created by a function
such as SHU-GATHER-QUALIFIED-NAMES.  LEVEL is the optional number of name parts
to be returned as a single string.  If the list is \"a, b, c\" and the level is
one, the returned name is \"a\".  If the level is two, the returned name is
\"a::b\".  If the level is three, the returned name is \"a::b::c\".  If the
level is four, the returned name is nil because the name does not have four
parts.  If LEVEL is not specified, the entire name is returned."
  (let ((tlevel 0)
        (waiting)
        (qname))
    (when (not level)
      (setq level (length gather)))
    (when (>= (length gather) level)
      (setq qname "")
      (while (< tlevel level)
        (when waiting
          (setq qname (concat qname "::"))
          (setq waiting nil))
        (setq qname (concat qname (car gather)))
        (setq waiting t)
        (setq tlevel (1+ tlevel))
        (setq gather (cdr gather))))
    qname
    ))



;;
;;  shu-make-reduced-name-list
;;
(defun shu-make-reduced-name-list (glist level)
  "GLIST is a list produced by the function SHU-GATHER-QUALIFIED-NAMES.  LEVEL is
the number of names in any given name list to use as part of the key.  This
function returns a new list in which the CAR of each item is the key and the CDR
of each item is a list of names that map to that key."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (gather)
        (ht (make-hash-table :test 'equal :size (length glist)))
        (qname)
        (qlist)
        (nlist)
        (nl)
        (kv)
        )
    (when glist
      (while glist
        (setq gather (car glist))
        (setq qname (shu-make-qualified-name gather level))
        (when qname
          (setq qlist (gethash qname ht))
          (if (not qlist)
              (setq qlist (list gather))
            (push gather qlist)
            )
          (puthash qname qlist ht)
          )
        (setq glist (cdr glist))
        )
      (maphash (lambda (k v) (push (cons k v) nlist)) ht)
      (setq nl nlist)
      (setq nlist nil)
      (while nl
        (setq kv (car nl))
        (setq qname (car kv))
        (setq qlist (cdr kv))
        (setq qlist (shu-make-unique-name-list qlist))
        (push (cons qname qlist) nlist)
        (setq nl (cdr nl))
        )
      )
    (setq nlist (sort nlist (lambda (lhs rhs) (string< (upcase (car lhs)) (upcase (car rhs))))))
    nlist
    ))



;;
;;  shu-make-unique-name-list
;;
(defun shu-make-unique-name-list (qlist)
  "QLIST is a list of gather that is unordered and may contain duplicates.
Return a sorted list with the duplicates removed."
  (let (
        (ht (make-hash-table :test 'equal :size (length qlist)))
        (gather)
        (qname)
        (nlist)
        )
    (while qlist
      (setq gather (car qlist))
      (setq qname (shu-make-qualified-name gather))
      (puthash qname gather ht)
      (setq qlist (cdr qlist))
      )
    (maphash (lambda (k v) (push v nlist)) ht)
    (setq nlist (sort nlist (lambda (lhs rhs)
                              (string< (upcase (shu-make-qualified-name lhs))
                                       (upcase (shu-make-qualified-name rhs))))))
    nlist
    ))




;;
;;  shu-test-shu-make-qualified-name-1
;;
(ert-deftest shu-test-shu-make-qualified-name-1 ()
  (let ((gather (list "a" "b"))
        (qname))
    (setq qname (shu-make-qualified-name gather 1))
    (should qname)
    (should (stringp qname))
    (should (string= qname "a"))
    ))



;;
;;  shu-test-shu-make-qualified-name-2
;;
(ert-deftest shu-test-shu-make-qualified-name-2 ()
  (let ((gather (list "a" "b"))
        (qname))
    (setq qname (shu-make-qualified-name gather 2))
    (should qname)
    (should (stringp qname))
    (should (string= qname "a::b"))
    ))



;;
;;  shu-test-shu-make-qualified-name-3
;;
(ert-deftest shu-test-shu-make-qualified-name-3 ()
  (let ((gather (list "a" "b"))
        (qname))
    (setq qname (shu-make-qualified-name gather 3))
    (should (not qname))
    ))



;;
;;  shu-test-shu-make-qualified-name-4
;;
(ert-deftest shu-test-shu-make-qualified-name-4 ()
  (let ((gather (list "a" "b" "c" "d"))
        (qname))
    (setq qname (shu-make-qualified-name gather 4))
    (should qname)
    (should (stringp qname))
    (should (string= qname "a::b::c::d"))
    ))



;;
;;  shu-test-shu-make-qualified-name-5
;;
(ert-deftest shu-test-shu-make-qualified-name-45()
  (let ((gather (list "a" "b" "c" "d"))
        (qname))
    (setq qname (shu-make-qualified-name gather))
    (should qname)
    (should (stringp qname))
    (should (string= qname "a::b::c::d"))
    ))



;;
;;  shu-test-shu-gather-qualified-names-1
;;
1(ert-deftest shu-test-shu-gather-qualified-names-1 ()
   "This should create a GLIST that holds the following lists:
        1. my
           name
           Fred
        2. std
           string
        3. std
           numeric_limits"
  (let ((token-list)
        (rlist)
        (ret-val)
        (glist)
        (gather)
        (data
         (concat
          "// this is some stuff\n"
          "\n"
          "std::numeric_limits<int>  limit1;\n"
          "\n"
          "std::string   it;\n"
          "\n"
          "\n"
          "my::name::Fred  thing;\n"
          "\n"
          "Datetime  it;\n"
          "\n"
          "// Some sort of comment\n"
          "\n"
          "mumble  it\n"
          "\n"
          "std::tuple  y;\n"
          "/* */\n"
          )))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (shu-cpp-tokenize-show-list token-list)
    (setq ret-val (shu-cpp-all-search-match-tokens rlist shu-cpp-match-classname-forms token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (setq rlist (nreverse rlist))
    (setq glist (shu-gather-qualified-names rlist))
    (should glist)
    (should (listp glist))
    (should (= 3 (length glist)))
    (setq gather (car glist))
    (should gather)
    (should (listp gather))
    (should (= 3 (length gather)))
    (should (car gather))
    (should (string= "my" (car gather)))
    (should (cadr gather))
    (should (string= "name" (cadr gather)))
    (should (caddr gather))
    (should (string= "Fred" (caddr gather)))
    (setq glist (cdr glist))
    (should glist)
    (should (listp glist))
    (setq gather (car glist))
    (should gather)
    (should (listp gather))
    (should (= 2 (length gather)))
    (should (car gather))
    (should (string= "std" (car gather)))
    (should (cadr gather))
    (should (string= "string" (cadr gather)))
    (setq glist (cdr glist))
    (should glist)
    (should (listp glist))
    (setq gather (car glist))
    (should gather)
    (should (listp gather))
    (should (= 2 (length gather)))
    (should (car gather))
    (should (string= "std" (car gather)))
    (should (cadr gather))
    (should (string= "numeric_limits" (cadr gather)))
    ))



;;; shu-find-classes.el ends here
