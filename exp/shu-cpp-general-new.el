;;; shu-cpp-general-new.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-cpp-general-new
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

;; This is experimental code for manipulating and using sections of C++

;;; Code:



;;
;;  shu-cpp-keywords
;;
(defconst shu-cpp-keywords
  (list
   (cons "alignas" 0)
   (cons "alignof" 0)
   (cons "and" 0)
   (cons "and_eq" 0)
   (cons "asm" 0)
   (cons "atomic_cancel" 0)
   (cons "atomic_commit" 0)
   (cons "atomic_noexcept" 0)
   (cons "auto" 0)
   (cons "bitand" 0)
   (cons "bitor" 0)
   (cons "bool" 0)
   (cons "break" 0)
   (cons "case" 0)
   (cons "catch" 0)
   (cons "char" 0)
   (cons "char8_t" 0)
   (cons "char16_t" 0)
   (cons "char32_t" 0)
   (cons "class" 0)
   (cons "compl" 0)
   (cons "concept" 0)
   (cons "const" 0)
   (cons "consteval" 0)
   (cons "constexpr" 0)
   (cons "const_cast" 0)
   (cons "continue" 0)
   (cons "co_await" 0)
   (cons "co_return" 0)
   (cons "co_yield" 0)
   (cons "decltype" 0)
   (cons "default" 0)
   (cons "delete" 0)
   (cons "do" 0)
   (cons "double" 0)
   (cons "dynamic_cast" 0)
   (cons "else" 0)
   (cons "enum" 0)
   (cons "explicit" 0)
   (cons "export" 0)
   (cons "extern" 0)
   (cons "false" 0)
   (cons "float" 0)
   (cons "for" 0)
   (cons "friend" 0)
   (cons "goto" 0)
   (cons "if" 0)
   (cons "import" 0)
   (cons "inline" 0)
   (cons "int" 0)
   (cons "long" 0)
   (cons "module" 0)
   (cons "mutable" 0)
   (cons "namespace" 0)
   (cons "new" 0)
   (cons "noexcept" 0)
   (cons "not" 0)
   (cons "not_eq" 0)
   (cons "nullptr" 0)
   (cons "operator" 0)
   (cons "or" 0)
   (cons "or_eq" 0)
   (cons "private" 0)
   (cons "protected" 0)
   (cons "public" 0)
   (cons "reflexpr" 0)
   (cons "register" 0)
   (cons "reinterpret_cast" 0)
   (cons "requires" 0)
   (cons "return" 0)
   (cons "short" 0)
   (cons "signed" 0)
   (cons "sizeof" 0)
   (cons "static" 0)
   (cons "static_assert" 0)
   (cons "static_cast" 0)
   (cons "struct" 0)
   (cons "switch" 0)
   (cons "synchronized" 0)
   (cons "template" 0)
   (cons "this" 0)
   (cons "thread_local" 0)
   (cons "throw" 0)
   (cons "true" 0)
   (cons "try" 0)
   (cons "typedef" 0)
   (cons "typeid" 0)
   (cons "typename" 0)
   (cons "union" 0)
   (cons "unsigned" 0)
   (cons "using" 0)
   (cons "virtual" 0)
   (cons "void" 0)
   (cons "volatile" 0)
   (cons "wchar_t" 0)
   (cons "while" 0)
   (cons "xor" 0)
   (cons "xor_eq" 0))
   "alist of C++ key words up to approximately C++17")


;;
;;  shu-get-cpp-keywords-hash
;;
(defun shu-get-cpp-keywords-hash ()
  "Return a hash table containing all of the C++ key words."
  (interactive)
  (let ((ht (make-hash-table :test 'equal :size (length shu-cpp-keywords)))
        (kl shu-cpp-keywords)
        (kc))
    (while kl
      (setq kc (car kl))
      (puthash (car kc) (cdr kc) ht)
      (setq kl (cdr kl)))
    ht
    ))



;;
;;  shu-=test-shu-get-cpp-keywords-hash
;;
(ert-deftest shu-=test-shu-get-cpp-keywords-hash ()
  (let ((kl shu-cpp-keywords)
        (ht (shu-get-cpp-keywords-hash))
        (kc)
        (kw)
        (nkw))
    (while kl
      (setq kc (car kl))
      (setq kw (car kc))
      (should (gethash kw ht))
      (setq nkw (concat kw "something-or-other"))
      (should (not (gethash nkw ht)))
      (setq kl (cdr kl)))
    ))


;;; shu-cpp-general-new.el ends here
