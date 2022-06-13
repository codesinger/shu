;;; shu-bde-new.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2022 Stewart L. Palmer
;;
;; Package: shu-date
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


;;; Code:



;;
;;  try
;;
(defun try ()
  "Doc string."
  (interactive)
  (let (
        (a-val)
        (b-val)
        )
    (setq a-val (read-string "a? "))
    (setq b-val (read-string "b? "))
    (message "a-val '%s', b-val: '%s'" a-val b-val)
    ))




;;
;;  ccc
;;
(defun ccc ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**goo**"))
        (str "a, b, c, d")
        (str2 "a b c d")
        (aa)
        (a)
        )
    (setq aa (split-string str "[,]+" t (concat shu-all-whitespace-regexp "+")))
    (princ aa gb)(princ "\n" gb)
    (while aa
      (setq a (car aa))
      (princ (concat "'" a "'\n") gb)
      (setq aa (cdr aa))
      )
    (setq aa (split-string str2 "[,]+" t (concat shu-all-whitespace-regexp "+")))
    (princ aa gb)(princ "\n" gb)
    (while aa
      (setq a (car aa))
      (princ (concat "'" a "'\n") gb)
      (setq aa (cdr aa))
      )

    ))



;;
;;  shu-gen-bde-create-prompt
;;
(defun shu-gen-bde-create-prompt ()
  "This function fetches the prompt string from SHU-GEN-BDE-MAKE-PROMPT-STRING
issues the query, and returns the result."
  (let ((query (shu-gen-bde-make-prompt-string)))
    (read-string query)
    ))



;;
;;  shu-gen-bde-create-prompt-template
;;
(defun shu-gen-bde-create-prompt-template ()
  "This function fetches the prompt string from SHU-GEN-BDE-MAKE-PROMPT-STRING
issues the query, and then issues a query for the comma separated list of
template parameter names.  It returns a list with twoitens on it:

    1. The name of the new component

    2. The list of template parameter names

If the comma separated list of template parameter names is empty, the list of
template parameter names (Item 2 above) is nil"
  (let (
        (query (shu-gen-bde-make-prompt-string))
        (tquery "Comma separated template parameter names? ")
        (a1)
        (a2)
        (plist)
        (answers)
        )
    (setq a1 (read-string query))
    (setq a2 (read-string tquery))
    (setq plist (shu-cpp-split-template-parameter-list a2))
    (push plist answers)
    (push a1 answers)
    answers
    ))



;;
;;  shu-cpp-split-template-parameter-list
;;
(defun shu-cpp-split-template-parameter-list (tp-string)
  "TP-STRING is a comma separated list of template parameter names.  This
function splits the string into a list of names and returns that list."
  (let ((tp-list  (split-string tp-string "[,]+" t (concat shu-all-whitespace-regexp "+"))))
    tp-list
    ))



;;
;;  shu-gen-bde-make-prompt-string
;;
(defun shu-gen-bde-make-prompt-string ()
  "This function creates the prompt for the interactive special form of the
function SHU-GEN-BDE-COMPONENT.  The prompt includes the namespace in which the
new class will be created or the string \"NO NAMESPACE\" if there is no default
namespace set.  If the name of the current directory does not match the default
namespace, the prompt also includes the directory name to remind the user that
the current directory name does not match the namespace."
  (let ((query)
        (namespace (if shu-cpp-default-namespace shu-cpp-default-namespace "NO NAMESPACE"))
        (prefix (shu-get-directory-prefix)))
    (setq query (concat "Class name in namespace " namespace "? "))
    (when (and shu-cpp-default-namespace (not (string= prefix namespace)))
      (setq query (concat "Class name in namespace " namespace  " in directory '" prefix "'? ")))
    query
    ))




;;
;;  shu-cpp-decl-cpp-print-self
;;
(defun shu-cpp-decl-cpp-print-self (class-name template-list)
  "Generate the skeleton code for the printSelf() function.
CLASS-NAME is the name of the containing C++ class."
  (let (
        (std-name (if shu-cpp-use-bde-library shu-cpp-std-namespace "std"))
        (qualified-class-name (shu-cpp-make-qualified-class-name class-name template-list))
        (ipad (make-string shu-cpp-indent-length ? ))
        )
    (insert "\n")
    (shu-cpp-insert-template-decl template-list)
    (insert
     (concat
      std-name "::ostream &" qualified-class-name "::printSelf(\n"
      ipad std-name "::ostream    &os)\n"
      "const\n"
      "{\n"
      ipad "os << \"Instance of '" class-name "'\";\n"
      "\n"
      ipad "return os;\n"
      "}\n"))
    ))



;;
;;  shu-cpp-make-qualified-class-name
;;
(defun shu-cpp-make-qualified-class-name (class-name template-list)
  "The input is a CLASS-NAME and TEMPLATE-LIST.  The output is a class name
followed by the comma separated list of template parameter names.  If the
template parameter names are T and S and the class name is MumbleMar, the
returned value is MumbleBar<T, S>.  If TEMPLATE-LIST is nil or empty,
the original class name is returned."
  (let ((tlist (shu-cpp-make-template-list template-list)))
    (concat class-name tlist)
    ))



;;
;;  shu-cpp-insert-template-decl
;;
(defun shu-cpp-insert-template-decl (template-list)
  "If TEMPLATE-LIST holds a list of template parameter names, insert into the
buffer the declaration

        template<typename A, typename B>
        inline

If TEMPLATE-LIST  is nil, do nothing."
  (let (
        )
    (when template-list
      (insert
       (concat
        (shu-cpp-make-decl-template template-list))
       "\n"
       "inline"
       "\n"
       )
      )
    ))



;;
;;  shu-cpp-make-decl-template
;;
(defun shu-cpp-make-decl-template (template-list)
  "Create the declaration

        template<typename A, typename B>

from the list of template parameter names"
  (let ((pname)
        (tdecl "")
        (sep ""))
    (when template-list
      (setq tdecl "template<")
      (while template-list
        (setq pname (car template-list))
        (setq tdecl (concat tdecl sep "typename " pname))
        (setq sep ", ")
        (setq template-list (cdr template-list)))
      (setq tdecl (concat tdecl ">")))
    tdecl
    ))



;;
;;  shu-cpp-make-template-list
;;
(defun shu-cpp-make-template-list (template-list)
  "Create the declaration

        <A, B>

from the list of template parameter names.
An empty string is returned if TEMPLATE-LIST is nil or empty."
  (let ((pname)
        (count 0)
        (tlist "<")
        (sep "")
        (result ""))
    (when template-list
      (while template-list
        (setq pname (car template-list))
        (setq tlist (concat tlist sep pname))
        (setq sep ", ")
        (setq count (1+ count))
        (setq template-list (cdr template-list)))
      (when (/= count 0)
        (setq result (concat tlist ">"))))
    result
    ))



;;
;;  shu-test-shu-cpp-insert-template-decl-1
;;
(ert-deftest shu-test-shu-cpp-insert-template-decl-1 ()
  (let (
        (template-list)
        (actual)
        (expected "")
        )
    (with-temp-buffer
      (shu-cpp-insert-template-decl template-list)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      )
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-insert-template-decl-2
;;
(ert-deftest shu-test-shu-cpp-insert-template-decl-2 ()
  (let (
        (template-list (list "S" "T"))
        (actual "")
        (expected "template<typename S, typename T>\ninline\n")
        )
    (with-temp-buffer
      (shu-cpp-insert-template-decl template-list)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      )
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-1
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-1 ()
  (let ((template-list (list "T"))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar<T>"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-2
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-2 ()
  (let ((template-list (list "T" "S" "Q"))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar<T, S, Q>"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-3
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-3 ()
  (let ((template-list (list))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-4
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-4 ()
  (let ((template-list)
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-1
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-1 ()
  (let ((tp-string "a")
        (tp-list))
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should tp-list)
    (should (listp tp-list))
    (should (= 1 (length tp-list)))
    (should (string= "a" (car tp-list)))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-2
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-2 ()
  (let (
        (tp-string "a, b")
        (tp-list)
        )
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should tp-list)
    (should (listp tp-list))
    (should (= 2 (length tp-list)))
    (should (string= "a" (car tp-list)))
    (should (string= "b" (cadr tp-list)))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-3
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-3 ()
  (let ((tp-string "")
        (tp-list))
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should (not tp-list))
    ))





;;
;;  shu-test-shu-cpp-make-decl-template-1
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-1 ()
  (let ((template-list (list "T"))
        (actual)
        (expected "template<typename T>"))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-decl-template-2
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-2 ()
  (let ((template-list (list "T" "P" "Q"))
        (actual)
        (expected "template<typename T, typename P, typename Q>"))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-decl-template-3
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-3 ()
  (let ((template-list)
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-1
;;
(ert-deftest shu-test-shu-cpp-make-template-list-1 ()
  (let ((template-list (list "T"))
        (actual)
        (expected "<T>"))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-2
;;
(ert-deftest shu-test-shu-cpp-make-template-list-2 ()
  (let ((template-list (list "T" "P" "Q"))
        (actual)
        (expected "<T, P, Q>"))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-3
;;
(ert-deftest shu-test-shu-cpp-make-template-list-3 ()
  (let ((template-list (list))
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-4
;;
(ert-deftest shu-test-shu-cpp-make-template-list-4 ()
  (let ((template-list)
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;; shu-bde-new.el ends here
