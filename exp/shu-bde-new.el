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
;;  shu-gen-bde-create-prompt
;;
(defun shu-gen-bde-create-prompt ()
  "This function fetches the prompt string from SHU-GEN-BDE-MAKE-PROMPT-STRING
issues the query, and returns the result."
  (let (
        (query (shu-gen-bde-make-prompt-string))
        )
    (read-string query)
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
  (let ((std-name (if shu-cpp-use-bde-library shu-cpp-std-namespace "std"))
        (ipad (make-string shu-cpp-indent-length ? )))
    (insert
     (concat
      "\n"
      std-name "::ostream &" class-name "::printSelf(\n"
      ipad std-name "::ostream    &os)\n"
      "const\n"
      "{\n"
      ipad "os << \"Instance of '" class-name "'\";\n"
      "\n"
      ipad "return os;\n"
      "}\n"))
    ))



;;
;;  shu-cpp-make-decl-template
;;
(defun shu-cpp-make-decl-template (template-list)
  "Create the declaration

        template<typename A, typename B>

from the list of template parameter names"
  (let ((pname)
        (tdecl "template<")
        (sep ""))
    (while template-list
      (setq pname (car template-list))
      (setq tdecl (concat tdecl sep "typename " pname))
      (setq sep ", ")
      (setq template-list (cdr template-list)))
    (concat tdecl ">")
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
