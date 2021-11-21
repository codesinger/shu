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
            )
          )

    ))



;;
;;  shu-show-glist
;;
(defun shu-show-glist (glist)
  "Doc string."
  (interactive)
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (gather)
        (token)
        (name)
        )
    (princ "\n\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n" gb)
;;;    (setq glist (sort glist (lambda(lgather rgather)
;;;                             (let (
;;;                                    (less)
;;;                                    )
;;;                              (if (string= (car lgather) (car rgather))
;;;                                  (setq less (string< (cadr lgather) (cadr rgather)))
;;;                                (setq less (string< (car lgather) (car rgather)))
;;;                                  )
;;;                             less
;;;                              )
;;;                              )
;;;                      )
;;;          )
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
