;;; shu-match.t.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-match
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

;; Unit tests for the functions in shu-cpp-match.el

;;; Code:

(require 'ert)
(require 'shu-cpp-match)






;;
;;  shu-cpp-match-funs-test-search-1
;;
;; This tests the match lists (and the underlying match code) used by
;; shu-cpp-match-funs.el
;;
(ert-deftest shu-cpp-match-funs-test-search-1 ()
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace alice;\n /* Hello */"  ;; 1
          "using std::string;\n"                  ;; 2
          "using namespace alice;\n"              ;; 3
          "// Hello\n"
          "using namespace bob;\n"                ;; 4
          "// using namespace nonsense\n"
          " using namespace a::b:: ; \n"          ;; 5
          "using namespace /* Hello */ component::SomeClass;\n"   ;; 6
          "using namespace Whammo::other::OtherClass;\n"          ;; 7
          " using namespace ::std;\n"                             ;; 8
          " using namespace a::b::c::d::e::f::g::h::i;\n"         ;; 9
          "\n"
          "namespace Whammo\n"
          "{\n"
          "\n"
          "namespace something\n"
          "{\n"
          "\n"
          "}\n"
          "}\n"
          ))
        (token-list)
        (rlist)
        (nlist)
        (tlist)
        (advance-tlist)
        (token-info)
        (ret-val)
        (token)
        (token-type)
        (item-number 0))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq tlist token-list)
    (shu-cpp-tokenize-show-list tlist)

    ;; 1
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-namespace-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (shu-cpp-tokenize-show-list rlist "FNZ-RLIST-1")
    (shu-cpp-tokenize-show-list tlist "FNZ-TLIST-1")

    ;; 3
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-namespace-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (shu-cpp-tokenize-show-list rlist "FNZ-RLIST-3")
    (shu-cpp-tokenize-show-list tlist "FNZ-TLIST-3")

    ;; 4
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-namespace-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (shu-cpp-tokenize-show-list rlist "FNZ-RLIST-4")
    (shu-cpp-tokenize-show-list tlist "FNZ-TLIST-4")

    ;; 6
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-namespace-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (shu-cpp-tokenize-show-list rlist "FNZ-RLIST-6")
    (shu-cpp-tokenize-show-list tlist "FNZ-TLIST-6")

    ;; 7
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-namespace-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (shu-cpp-tokenize-show-list rlist "FNZ-RLIST-7")
    (shu-cpp-tokenize-show-list tlist "FNZ-TLIST-7")

    ;; 8
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-namespace-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (shu-cpp-tokenize-show-list rlist "FNZ-RLIST-8")
    (shu-cpp-tokenize-show-list tlist "FNZ-TLIST-8")

    ;; 9
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-namespace-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (shu-cpp-tokenize-show-list rlist "FNZ-RLIST-9")
    (shu-cpp-tokenize-show-list tlist "FNZ-TLIST-9")

    (setq nlist (nreverse rlist))

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "alice"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "alice"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "bob"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "component"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "SomeClass"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "Whammo"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "other"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "OtherClass"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "std"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "a"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "b"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "c"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "d"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "e"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "f"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "g"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "h"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "i"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))
    ))





;;
;;  shu-cpp-match-funs-test-search-2
;;
;; This is a snippet of code that can find each occurrence of
;; \"using namespace\" and accumulate a list of such occurrences."
;;
(ert-deftest shu-cpp-match-funs-test-search-2 ()
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace alice;\n /* Hello */"  ;; 1
          "using std::string;\n"                  ;; 2
          "using namespace alice;\n"              ;; 3
          "// Hello\n"
          "using namespace bob;\n"                ;; 4
          "// using namespace nonsense\n"
          " using namespace a::b:: ; \n"          ;; 5
          "using namespace /* Hello */ component::SomeClass;\n"   ;; 6
          "using namespace Whammo::other::OtherClass;\n"          ;; 7
          " using namespace ::std;\n"                             ;; 8
          " using namespace a::b::c::d::e::f::g::h::i;\n"         ;; 9
          "\n"
          "namespace Whammo\n"
          "{\n"
          "\n"
          "namespace something\n"
          "{\n"
          "\n"
          "}\n"
          "}\n"
          ))
        (token-list)
        (rlist)
        (rlists)
        (tlist)
        (advance-tlist)
        (token-info)
        (ret-val)
        (token)
        (token-type)
        (item-number 0)
        (something t)
        (lcount 0))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq tlist token-list)
    (shu-cpp-tokenize-show-list tlist)
    (while something
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-namespace-list-single tlist))
      (if (not ret-val)
          (setq something nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq rlist (nreverse rlist))
        (push rlist rlists)
        (setq rlist nil)))
    (setq rlists (nreverse rlists))
    (while rlists
      (setq lcount (1+ lcount))
      (princ (format "\nRLIST %d:\n" lcount) gb)
      (setq rlist (car rlists))
      (shu-cpp-tokenize-show-list rlist)
      (setq rlists (cdr rlists)))
))







;;
;;  shu-cpp-match-funs-test-search-3
;;
;; This is a snippet of code that can find each occurrence of
;; \"using namespace\" and accumulate a list of such occurrences."
;;
(ert-deftest shu-cpp-match-funs-test-search-3 ()
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace alice;\n /* Hello */"  ;; 1
          "using std::string;\n"                  ;; 2
          "using namespace alice;\n"              ;; 3
          "// Hello\n"
          "using namespace bob;\n"                ;; 4
          "// using namespace nonsense\n"
          " using namespace a::b:: ; \n"          ;; 5
          "using namespace /* Hello */ component::SomeClass;\n"   ;; 6
          "using namespace Whammo::other::OtherClass;\n"          ;; 7
          " using namespace ::std;\n"                             ;; 8
          " using namespace a::b::c::d::e::f::g::h::i;\n"         ;; 9
          " using fred::AndyBob; \n"
          " using aa::bb::cc::dd::ee;\n"
          "\n"
          "namespace Whammo\n"
          "{\n"
          "\n"
          "namespace something\n"
          "{\n"
          "\n"
          "}\n"
          "}\n"
          ))
        (token-list)
        (rlist)
        (rlists)
        (tlist)
        (advance-tlist)
        (token-info)
        (ret-val)
        (token)
        (token-type)
        (item-number 0)
        (something t)
        (lcount 0))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq tlist token-list)
    (shu-cpp-tokenize-show-list tlist)
    (while something
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
      (if (not ret-val)
          (setq something nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq rlist (nreverse rlist))
        (push rlist rlists)
        (setq rlist nil)))
    (setq rlists (nreverse rlists))
    (while rlists
      (setq lcount (1+ lcount))
      (princ (format "\nRLIST %d:\n" lcount) gb)
      (setq rlist (car rlists))
      (shu-cpp-tokenize-show-list rlist)
      (setq rlists (cdr rlists)))
))






;;
;;  shu-cpp-match-funs-test-search-4
;;
;; This is a snippet of code that can find each occurrence of
;; \"using namespace\" and accumulate a list of such occurrences."
;;
(ert-deftest shu-cpp-match-funs-test-search-4 ()
  (let ((gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace alice;\n /* Hello */"  ;; 1
          "using std::string;\n"                  ;; 2
          "// Hello\n"
          "using namespace bob;\n"                ;; 4
          "// using namespace nonsense\n"
          " using namespace a::b:: ; \n"          ;; 5
          "using namespace /* Hello */ component::SomeClass;\n"   ;; 6
          "using namespace Whammo::other::OtherClass;\n"          ;; 7
          " using namespace ::std;\n"                             ;; 8
          " using fred::AndyBob; \n"
          "using zz::qq::\n"
          " using aa::bb::cc::dd;\n"
          "\n"
          "namespace Whammo\n"
          "{\n"
          "\n"
          "namespace something\n"
          "{\n"
          "\n"
          "}\n"
          "}\n"
          ))
        (token-list)
        (rlist)
        (nlist)
        (rlists)
        (tlist)
        (advance-tlist)
        (token-info)
        (ret-val)
        (token)
        (token-type)
        (item-number 0)
        (something t)
        (lcount 0))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq tlist token-list)
    (shu-cpp-tokenize-show-list tlist)
    (while something
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
      (if (not ret-val)
          (setq something nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq rlist (nreverse rlist))
        (push rlist rlists)
        (setq rlist nil)))
    (setq rlists (nreverse rlists))
    (while rlists
      (setq lcount (1+ lcount))
      (princ (format "\nRLIST %d:\n" lcount) gb)
      (setq rlist (car rlists))
      (shu-cpp-tokenize-show-list rlist)
      (setq rlists (cdr rlists)))

    (setq tlist token-list)

    ;; 1
    (setq rlist nil)
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))

    (setq nlist (nreverse rlist))
    (shu-cpp-tokenize-show-list nlist "QNQ-RLIST-1")
    (shu-cpp-tokenize-show-list tlist "QNQ-TLIST-1")

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "alice"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    ;; 2
    (setq rlist nil)
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))
    (shu-cpp-tokenize-show-list rlist "QNQ-RLIST-2")

    (setq nlist (nreverse rlist))
    (shu-cpp-tokenize-show-list nlist "QNQ-NLIST-2")
    (shu-cpp-tokenize-show-list tlist "QNQ-TLIST-2")

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "std"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "string"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    ;; 3
    (setq rlist nil)
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))

    (setq nlist (nreverse rlist))
    (shu-cpp-tokenize-show-list nlist "QNQ-RLIST-3")
    (shu-cpp-tokenize-show-list tlist "QNQ-TLIST-3")

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "bob"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    ;; 4
    (setq rlist nil)
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))

    (setq nlist (nreverse rlist))
    (shu-cpp-tokenize-show-list nlist "QNQ-RLIST-4")
    (shu-cpp-tokenize-show-list tlist "QNQ-TLIST-4")

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "component"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "SomeClass"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    ;; 5
    (setq rlist nil)
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))

    (setq nlist (nreverse rlist))
    (shu-cpp-tokenize-show-list nlist "QNQ-RLIST-5")
    (shu-cpp-tokenize-show-list tlist "QNQ-TLIST-5")

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "Whammo"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "other"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "OtherClass"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    ;; 6
    (setq rlist nil)
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))

    (setq nlist (nreverse rlist))
    (shu-cpp-tokenize-show-list nlist "QNQ-RLIST-6")
    (shu-cpp-tokenize-show-list tlist "QNQ-TLIST-6")

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "std"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    ;; 7
    (setq rlist nil)
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))

    (setq nlist (nreverse rlist))
    (shu-cpp-tokenize-show-list nlist "QNQ-RLIST-7")
    (shu-cpp-tokenize-show-list tlist "QNQ-TLIST-7")

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "fred"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "AndyBob"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    ;; 8
    (setq rlist nil)
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-match-using-list-single tlist))
    (should ret-val)
    (should (consp ret-val))
    (setq tlist (car ret-val))
    (should tlist)
    (should (listp tlist))
    (setq rlist (cdr ret-val))
    (should rlist)
    (should (listp rlist))

    (setq nlist (nreverse rlist))
    (shu-cpp-tokenize-show-list nlist "QNQ-RLIST-8")
    (shu-cpp-tokenize-show-list tlist "QNQ-TLIST-8")

    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-kw))
    (should (string= token "using"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "aa"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "bb"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "cc"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-uq))
    (should (string= token "dd"))

    (setq nlist (cdr nlist))
    (setq token-info (car nlist))
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (= token-type shu-cpp-token-type-op))
    (should (string= token ";"))
))





;;
;;  shu-test-shu-match-find-all-using-internal-1
;;
(ert-deftest shu-test-shu-match-find-all-using-internal-1 ()
  "Test SHU-MATCH-FIND-ALL-USING-INTERNAL with a buffer that contains no
C++ code at all."
  (let ((data
         (concat
          "Whan that Aprille with his shoures soote\n"
          "The droghte of Marche hath perced to the roote,\n"
          "And bathed every veyne in swich licour,\n"
          "Of which vertu engendred is the flour;\n"
          "Whan Zephirus eek with his swete breeth\n"
          "Inspired hath in every holt and heeth\n"
          "The tendre croppes, and the yonge sonne\n"
          "Hath in the Ram his halfe cours y-ronne,\n"
          "And smale fowles maken melodye,\n"
          "That slepen al the night with open ye,\n"
          "(So priketh hem nature in hir corages:\n"
          "Than longen folk to goon on pilgrimages,\n"
          ))
        (token-list)
        (ret-val))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should (not ret-val))
    ))



;;
;;  shu-test-shu-match-find-all-using-internal-2
;;
(ert-deftest shu-test-shu-match-find-all-using-internal-2 ()
  "Test SHU-MATCH-FIND-ALL-USING-INTERNAL with a buffer that contains only
using namespace statements."
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace alice;\n /* Hello */"
          "using namespace b::c;\n"
          "// Hello\n"
         ))
        (token-list)
        (ret-val)
        (uns-list)
        (un-list)
        (rlist)
        (token-info)
        (token)
        (token-type))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should uns-list)
    (should (listp uns-list))
    (setq un-list (cdr ret-val))
    (should (not un-list))
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    ;;;
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "alice"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-op))
    (should (string= token ";"))
    ;;;
    (setq uns-list (cdr uns-list))
    (should uns-list)
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    ;;;
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "b"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "c"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-op))
    (should (string= token ";"))
    ))





;;
;;  shu-test-shu-match-find-all-using-internal-3
;;
(ert-deftest shu-test-shu-match-find-all-using-internal-3 ()
  "Test SHU-MATCH-FIND-ALL-USING-INTERNAL with a buffer that contains only
using statememts."
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using std::string;\n /* Hello */"
          "using b::c;\n"
          "// Hello\n"
         ))
        (token-list)
        (ret-val)
        (uns-list)
        (un-list)
        (rlist)
        (token-info)
        (token)
        (token-type))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should (not uns-list))
    (setq un-list (cdr ret-val))
    (should un-list)
    (should (listp un-list))
    (setq rlist (car un-list))
    (should rlist)
    (should (listp rlist))
    ;;;
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "std"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "string"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-op))
    (should (string= token ";"))
    ;;;
    (setq un-list (cdr un-list))
    (should un-list)
    (setq rlist (car un-list))
    (should rlist)
    (should (listp rlist))
    ;;;
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "b"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "c"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-op))
    (should (string= token ";"))
    ))





;;
;;  shu-test-shu-match-find-all-using-internal-4
;;
(ert-deftest shu-test-shu-match-find-all-using-internal-4 ()
  "Test SHU-MATCH-FIND-ALL-USING-INTERNAL with a buffer that contains only
using namespace statements."
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace alice;\n /* Hello */"
          "using std::string;\n"
          "using namespace b::c;\n"
          "using d::e;\n"
          "// Hello\n"
         ))
        (token-list)
        (ret-val)
        (uns-list)
        (un-list)
        (rlist)
        (token-info)
        (token)
        (token-type))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should uns-list)
    (should (listp uns-list))
    (setq un-list (cdr ret-val))
    (should un-list)
    (should (listp un-list))
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    ;;;
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "alice"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-op))
    (should (string= token ";"))
    ;;;
    (setq uns-list (cdr uns-list))
    (should uns-list)
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    ;;;
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "namespace"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "b"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "c"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-op))
    (should (string= token ";"))

    (setq rlist (car un-list))
    (should rlist)
    (should (listp rlist))
    ;;;
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "std"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "string"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-op))
    (should (string= token ";"))
    ;;;
    (setq un-list (cdr un-list))
    (should un-list)
    (setq rlist (car un-list))
    (should rlist)
    (should (listp rlist))
    ;;;
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-kw))
    (should (string= token "using"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "d"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-uq))
    (should (string= token "e"))
    ;;;
    (setq rlist (cdr rlist))
    (should rlist)
    (setq token-info (car rlist))
    (should token-info)
    (setq token-type (shu-cpp-token-extract-type token-info))
    (setq token (shu-cpp-token-extract-token token-info))
    (should (eq token-type shu-cpp-token-type-op))
    (should (string= token ";"))
    ))




;;
;;  shu-test-shu-match-using-namespace-string-1
;;
(ert-deftest shu-test-shu-match-using-namespace-string-1 ()
  "Simple, one level namespace name"
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace alice;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (uns-list)
        (rlist)
        (ns-name))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should uns-list)
    (should (listp uns-list))
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    (setq ns-name (shu-match-using-namespace-string rlist))
    (should ns-name)
    (should (stringp ns-name))
    (should (string= "alice" ns-name))
    ))




;;
;;  shu-test-shu-match-using-namespace-string-2
;;
(ert-deftest shu-test-shu-match-using-namespace-string-2 ()
  "Multi-level namespace name"
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace fred::andy::bob;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (uns-list)
        (rlist)
        (ns-name))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should uns-list)
    (should (listp uns-list))
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    (setq ns-name (shu-match-using-namespace-string rlist))
    (should ns-name)
    (should (stringp ns-name))
    (should (string= "fred::andy::bob" ns-name))
    ))



;;
;;  shu-test-shu-match-using-namespace-string-3
;;
(ert-deftest shu-test-shu-match-using-namespace-string-3 ()
  "Multi-level namespace name with a top level name that does not match anything."
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace fred::andy::bob;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (uns-list)
        (rlist)
        (top-name "WhammoCorp")
        (ns-name))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should uns-list)
    (should (listp uns-list))
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    (setq ns-name (shu-match-using-namespace-string rlist top-name))
    (should ns-name)
    (should (stringp ns-name))
    (should (string= "fred::andy::bob" ns-name))
7    ))



;;
;;  shu-test-shu-match-using-namespace-string-4
;;
(ert-deftest shu-test-shu-match-using-namespace-string-4 ()
  "multi-level namespace name with a top level name that matches the beginning."
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace WhammoCorp::fred::andy::bob;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (uns-list)
        (rlist)
        (top-name "WhammoCorp")
        (ns-name))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should uns-list)
    (should (listp uns-list))
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    (setq ns-name (shu-match-using-namespace-string rlist top-name))
    (should ns-name)
    (should (stringp ns-name))
    (should (string= "fred::andy::bob" ns-name))
    ))



;;
;;  shu-test-shu-match-using-namespace-string-5
;;
(ert-deftest shu-test-shu-match-using-namespace-string-5 ()
  "Single level namespace name with a top level name that matches the beginning."
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace WhammoCorp::alice;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (uns-list)
        (rlist)
        (top-name "WhammoCorp")
        (ns-name))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should uns-list)
    (should (listp uns-list))
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    (setq ns-name (shu-match-using-namespace-string rlist top-name))
    (should ns-name)
    (should (stringp ns-name))
    (should (string= "alice" ns-name))
    ))




;;
;;  shu-test-shu-match-using-string-1
;;
(ert-deftest shu-test-shu-match-using-string-1 ()
  "Simple, one level namespace name"
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using std::string;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (un-list)
        (rlist)
        (ret-val)
        (ns-name)
        (class-name))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq un-list (cdr ret-val))
    (should un-list)
    (should (listp un-list))
    (setq rlist (car un-list))
    (should rlist)
    (should (listp rlist))
    (setq ret-val (shu-match-using-string rlist))
    (should ret-val)
    (should (consp ret-val))
    (setq ns-name (car ret-val))
    (should ns-name)
    (should (stringp ns-name))
    (setq class-name (cdr ret-val))
    (should class-name)
    (should (stringp class-name))
    (should (string= "std" ns-name))
    (should (string= "string" class-name))
    ))




;;
;;  shu-test-shu-match-using-string-2
;;
(ert-deftest shu-test-shu-match-using-string-2 ()
  "Multi level namespace name"
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using abc::std::string;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (un-list)
        (rlist)
        (ret-val)
        (ns-name)
        (class-name))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq un-list (cdr ret-val))
    (should un-list)
    (should (listp un-list))
    (setq rlist (car un-list))
    (should rlist)
    (should (listp rlist))
    (setq ret-val (shu-match-using-string rlist))
    (should ret-val)
    (should (consp ret-val))
    (setq ns-name (car ret-val))
    (should ns-name)
    (should (stringp ns-name))
    (setq class-name (cdr ret-val))
    (should class-name)
    (should (stringp class-name))
    (should (string= "abc::std" ns-name))
    (should (string= "string" class-name))
    ))




;;
;;  shu-test-shu-match-using-string-3
;;
(ert-deftest shu-test-shu-match-using-string-3 ()
  "Multi level namespace name with top name that does not match anything"
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using abc::std::string;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (un-list)
        (rlist)
        (ret-val)
        (ns-name)
        (class-name)
        (top-name "WhammoCorp"))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq un-list (cdr ret-val))
    (should un-list)
    (should (listp un-list))
    (setq rlist (car un-list))
    (should rlist)
    (should (listp rlist))
    (setq ret-val (shu-match-using-string rlist top-name))
    (should ret-val)
    (should (consp ret-val))
    (setq ns-name (car ret-val))
    (should ns-name)
    (should (stringp ns-name))
    (setq class-name (cdr ret-val))
    (should class-name)
    (should (stringp class-name))
    (should (string= "abc::std" ns-name))
    (should (string= "string" class-name))
    ))




;;
;;  shu-test-shu-match-using-string-4
;;
(ert-deftest shu-test-shu-match-using-string-4 ()
  "Multi level namespace name with top name that matches the beginning"
  (let ((data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using WhammoCorp::abc::std::string;\n /* Hello */"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (un-list)
        (rlist)
        (ret-val)
        (ns-name)
        (class-name)
        (top-name "WhammoCorp"))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq un-list (cdr ret-val))
    (should un-list)
    (should (listp un-list))
    (setq rlist (car un-list))
    (should rlist)
    (should (listp rlist))
    (setq ret-val (shu-match-using-string rlist top-name))
    (should ret-val)
    (should (consp ret-val))
    (setq ns-name (car ret-val))
    (should ns-name)
    (should (stringp ns-name))
    (setq class-name (cdr ret-val))
    (should class-name)
    (should (stringp class-name))
    (should (string= "abc::std" ns-name))
    (should (string= "string" class-name))
    ))




;;
;;  shu-test-shu-match-get-start-end-pos-1
;;
(ert-deftest shu-test-shu-match-get-start-end-pos-1 ()
  (let ((data
         (concat
          "\n"
          "using namespace fred::andy::bob;\n /* Hello */"
          "2345678901234567890123456789012345"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (uns-list)
        (rlist)
        (ret-val)
        (spoint)
        (epoint))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (should (consp ret-val))
    (setq uns-list (car ret-val))
    (should uns-list)
    (should (listp uns-list))
    (setq rlist (car uns-list))
    (should rlist)
    (should (listp rlist))
    (setq ret-val (shu-match-get-start-end-pos rlist))
    (should ret-val)
    (should (consp ret-val))
    (setq spoint (car ret-val))
    (setq epoint (cdr ret-val))
    (should spoint)
    (should (numberp spoint))
    (should epoint)
    (should (numberp epoint))
    (should (eq 2 spoint))
    (should (eq 33 epoint))
    ))




;;
;;  shu-test-shu-match-get-start-end-pos-2
;;
(ert-deftest shu-test-shu-match-get-start-end-pos-2 ()
  (let ((data
         (concat
          "\n /* Hello */\n"
          "\n"
          "    using namespace fred::andy::bob; /* Hello */\n"
          "2345678901234567890123456789012345"
          "// Hello\n"
          ))
        (token-list)
        (ret-val)
        (uns-list)
        (rlist)
        (ret-val)
        (spoint)
        (epoint))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (setq ret-val (shu-match-find-all-using-internal token-list))
      (should ret-val)
      (should (consp ret-val))
      (setq uns-list (car ret-val))
      (should uns-list)
      (should (listp uns-list))
      (setq rlist (car uns-list))
      (should rlist)
      (should (listp rlist))
      (setq ret-val (shu-match-get-start-end-pos rlist t)))
    (should ret-val)
    (should (consp ret-val))
    (setq spoint (car ret-val))
    (setq epoint (cdr ret-val))
    (should spoint)
    (should (numberp spoint))
    (should epoint)
    (should (numberp epoint))
    (should (eq 16 spoint))
    (should (eq 64 epoint))
    ))





;;
;;  shu-test-shu-match-make-count-alist-from-hash
;;
(ert-deftest shu-test-shu-match-make-count-alist-from-hash ()
  (let ((class-ht (make-hash-table :test 'equal :size 12))
        (count-alist)
        (count)
        (cp1)
        (cp2)
        (cp3)
        (cp4)
        (cp5))
    (puthash "set" "std" class-ht)
    (puthash "string" "std" class-ht)
    (puthash "map" "std" class-ht)
    (puthash "Mumble" "abcde" class-ht)
    (setq count-alist (shu-match-make-count-alist-from-hash class-ht))
    (setq cp1 (assoc "set" count-alist))
    (should cp1)
    (should (consp cp1))
    (setq count (cdr cp1))
    (should (numberp count))
    (should (= count 0))
    (setq cp2 (assoc "string" count-alist))
    (should cp2)
    (should (consp cp2))
    (setq count (cdr cp2))
    (should (numberp count))
    (should (= count 0))
    (setq cp3 (assoc "map" count-alist))
    (should cp3)
    (should (consp cp3))
    (setq count (cdr cp3))
    (should (numberp count))
    (should (= count 0))
    (setq cp4 (assoc "Mumble" count-alist))
    (should cp4)
    (should (consp cp4))
    (setq count (cdr cp4))
    (should (numberp count))
    (should (= count 0))
    (setq cp5 (assoc "Wheeeee!" count-alist))
    (should (not cp5))
    ))



;;
;;  shu-test-shu-match-increment-class-count
;;
(ert-deftest shu-test-shu-match-increment-class-count ()
  (let ((count-alist
         (list
          (cons "set" 0)
          (cons "map" 2)
          (cons "string" 6)
          (cons "Mumble" 1)))
        (cp)
        (count))
    (shu-match-increment-class-count count-alist "map")
    (setq cp (assoc "map" count-alist))
    (should cp)
    (should (consp cp))
    (setq count (cdr cp))
    (should count)
    (should (numberp count))
    (should (= 3 count))
    ))



;;
;;  shu-test-shu-match-rmv-show-class-count
;;
(ert-deftest shu-test-shu-match-rmv-show-class-count ()
  (let ((count-alist
         (list
          (cons "set" 0)
          (cons "map" 2)
          (cons "string" 6)
          (cons "Mumble" 1)))
        (class-ht (make-hash-table :test 'equal :size 12))
        (expected
         (concat
          "              1: abcde::Mumble\n"
          "              2: std::map\n"
          "              0: std::set\n"
          "              6: std::string\n"))
        (actual))
    (puthash "set" "std" class-ht)
    (puthash "string" "std" class-ht)
    (puthash "map" "std" class-ht)
    (puthash "Mumble" "abcde" class-ht)
    (with-temp-buffer
      (shu-match-rmv-show-class-count count-alist class-ht (current-buffer))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-match-internal-rmv-using-1
;;
(ert-deftest shu-test-shu-match-internal-rmv-using-1 ()
  (let ((log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using std::string;\n"
          "    using namespace fred; /* Hello */\n"
          "    using abc::std::deque;\n /* Hello */"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))))
        (token-list))
    (with-temp-buffer
      (insert data)
      (shu-match-internal-rmv-using class-list log-buf))
    ))






;;
;;  shu-test-shu-match-internal-rmv-using-2
;;
(ert-deftest shu-test-shu-match-internal-rmv-using-2 ()
  (let ((log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using std::deque;\n"
          "    using namespace fred; /* Hello */\n"
          "    using abc::std::string;\n /* Hello */"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))))
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "                          \n"
          "    x = x + 1;\n"
          "                          \n"
          "                     \n"
          "    using namespace fred; /* Hello */\n"
          "                           \n"
          " /* Hello */    /* xxx */\n"
          "    abc::std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          )))
    (with-temp-buffer
      (insert data)
      (shu-match-internal-rmv-using class-list log-buf)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))





;;
;;  shu-test-shu-match-internal-rmv-using-3
;;
(ert-deftest shu-test-shu-match-internal-rmv-using-3 ()
  (let ((log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using namespace std;\n"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"      (list
                            "string"
                            "deque"
                            ))))
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "                          \n"
          "    x = x + 1;\n"
          "                          \n"
          "                        \n"
          "    /* xxx */\n"
          "    std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          )))
    (with-temp-buffer
      (insert data)
      (shu-match-internal-rmv-using class-list log-buf)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))





;;
;;  shu-test-shu-match-internal-rmv-using-4
;;
(ert-deftest shu-test-shu-match-internal-rmv-using-4 ()
  (let ((log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "    using abcde::AClass1;\n"
          "    using xyrzk::Xclass2;\n"
          "    using std::string;\n"
          "    using std::deque;\n"
          "    x = x + 1;\n"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"      (list
                            "string"
                            "deque"
                            ))))
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "                         \n"
          "                         \n"
          "                      \n"
          "                     \n"
          "    x = x + 1;\n"
          "    /* xxx */\n"
          "    std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          )))
    (with-temp-buffer
      (insert data)
      (shu-match-internal-rmv-using class-list log-buf)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))





;;
;;  shu-test-shu-match-internal-rmv-using-5
;;
(ert-deftest shu-test-shu-match-internal-rmv-using-5 ()
  (let ((log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace WhammoCorp::xyrzk;\n"
          "    using namespace std;\n"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"      (list
                            "string"
                            "deque"
                            ))))
        (top-name "WhammoCorp")
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "                          \n"
          "    x = x + 1;\n"
          "                                      \n"
          "                        \n"
          "    /* xxx */\n"
          "    std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          )))
    (with-temp-buffer
      (insert data)
      (shu-match-internal-rmv-using class-list log-buf top-name)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))





;;
;;  shu-test-shu-match-internal-rmv-using-6
;;
(ert-deftest shu-test-shu-match-internal-rmv-using-6 ()
  (let ((log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "    using WhammoCorp::abcde::AClass1;\n"
          "    using xyrzk::Xclass2;\n"
          "    using std::string;\n"
          "    using std::deque;\n"
          "    x = x + 1;\n"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"      (list
                            "string"
                            "deque"
                            ))))
        (top-name "WhammoCorp")
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "                                     \n"
          "                         \n"
          "                      \n"
          "                     \n"
          "    x = x + 1;\n"
          "    /* xxx */\n"
          "    std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          )))
    (with-temp-buffer
      (insert data)
      (shu-match-internal-rmv-using class-list log-buf top-name)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )
    ))




;;
;;  shu-test-shu-match-internal-rmv-using-7
;;
(ert-deftest shu-test-shu-match-internal-rmv-using-7 ()
  (let ((log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "    using /* Hello */ \n"
          "  namespace /* There */ abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using /* Hello */ std /* There */ \n"
          "   :: /* Bye */ deque;\n"
          "    using namespace fred; /* Hello */\n"
          "    using abc::std::string;\n /* Hello */"
          "    /* xxx */\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))))
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "\n"
          "                                                     \n"
          "    x = x + 1;\n"
          "                          \n"
          "                                                             \n"
          "    using namespace fred; /* Hello */\n"
          "                           \n"
          " /* Hello */    /* xxx */\n"
          "    abc::std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          )))
    (with-temp-buffer
      (insert data)
      (shu-match-internal-rmv-using class-list log-buf)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-match-internal-rmv-using-8
;;
(ert-deftest shu-test-shu-match-internal-rmv-using-8 ()
  (let ((log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "#include <set>\n"
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using namespace std;\n"
          "    /* xxx */\n"
          "    a->set();\n"
          "    x.set();\n"
          "    std::set    gg;\n"
          "    x = set[i]\n"
          "    string      x;\n"
          "    AClass1     z;\n"
          "    Xclass2     p;\n"
          "    deque       jj;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"      (list
                            "string"
                            "deque"
                            "set"
                            "map"
                            ))))
        (token-list)
        (actual)
        (expected
         (concat
          "/*!\n"
          " * file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <string>\n"
          "#include <set>\n"
          "\n"
          "                          \n"
          "    x = x + 1;\n"
          "                          \n"
          "                        \n"
          "    /* xxx */\n"
          "    a->set();\n"
          "    x.set();\n"
          "    std::set    gg;\n"
          "    x = set[i]\n"
          "    std::string      x;\n"
          "    abcde::AClass1     z;\n"
          "    xyrzk::Xclass2     p;\n"
          "    std::deque       jj;\n"
          "// Hello\n"
          )))
    (with-temp-buffer
      (insert data)
      (shu-match-internal-rmv-using class-list log-buf)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    (princ (concat "\n\n\nSOMETHING:\n" actual "\n") log-buf)
    ))



;;
;;  shu-test-remove-class-duplicates
;;
(ert-deftest shu-test-remove-class-duplicates ()
  (let (
        (gb (get-buffer-create shu-unit-test-buffer))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"     (list
                           "string"
                           "string"
                           "set"
                           "set"
                           "map"
                           ))))
        (proc-class)
        (cp1)
        (cp2)
        (cp3)
        (cl)
        )
    (setq proc-class (remove-class-duplicates class-list gb))
    (setq cp1 (assoc "abcde" class-list))
    (should cp1)
    (should (consp cp1))
    (setq cl (cdr cp1))
    (should cl)
    (should (listp cl))
    (should (= 2 (length cl)))
    (setq cp2 (assoc "xyrzk" class-list))
    (should cp2)
    (should (consp cp2))
    (setq cl (cdr cp2))
    (should cl)
    (should (listp cl))
    (should (= 2 (length cl)))
    (setq cp3 (assoc "std" class-list))
    (should cp3)
    (should (consp cp3))
    (setq cl (cdr cp3))
    (should cl)
    (should (listp cl))
    (should (= 3 (length cl)))
    ))


;;
;;  shu-test-shu-match-make-count-alist-from-hash-1
;;
(ert-deftest shu-test-shu-match-make-count-alist-from-hash-1 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"    (list
                          "string"
                          "set"
                          "map"
                          ))))
        (ht)
        (av1)
        (av2)
        (av3)
        (av4)
        (av5)
        (av6)
        (av7)
        (av8)
        (count-alist))
    (setq ht (shu-match-make-class-hash-table-internal class-list gb))
    (should ht)
    (should (hash-table-p ht))
    (setq count-alist (shu-match-make-count-alist-from-hash ht))
    (setq av1 (assoc "AClass1" count-alist))
    (should av1)
    (should (consp av1))
    (should (car av1))
    (should (stringp (car av1)))
    (should (string= "AClass1" (car av1)))
    (should (cdr av1))
    (should (numberp (cdr av1)))
    (should (= 0 (cdr av1)))
    (setq av2 (assoc "AClass2" count-alist))
    (should av2)
    (should (consp av2))
    (should (car av2))
    (should (stringp (car av2)))
    (should (string= "AClass2" (car av2)))
    (should (cdr av2))
    (should (numberp (cdr av2)))
    (should (= 0 (cdr av2)))
    (setq av3 (assoc "Xclass1" count-alist))
    (should av3)
    (should (consp av3))
    (should (car av3))
    (should (stringp (car av3)))
    (should (string= "Xclass1" (car av3)))
    (should (cdr av3))
    (should (numberp (cdr av3)))
    (should (= 0 (cdr av3)))
    (setq av4(assoc "Xclass2" count-alist))
    (should av4)
    (should (consp av4))
    (should (car av4))
    (should (stringp (car av4)))
    (should (string= "Xclass2" (car av4)))
    (should (cdr av4))
    (should (numberp (cdr av4)))
    (should (= 0 (cdr av4)))
    (setq av5 (assoc "string" count-alist))
    (should av5)
    (should (consp av5))
    (should (car av5))
    (should (stringp (car av5)))
    (should (string= "string" (car av5)))
    (should (cdr av5))
    (should (numberp (cdr av5)))
    (should (= 0 (cdr av5)))
    (setq av6 (assoc "set" count-alist))
    (should av6)
    (should (consp av6))
    (should (car av6))
    (should (stringp (car av6)))
    (should (string= "set" (car av6)))
    (should (cdr av6))
    (should (numberp (cdr av6)))
    (should (= 0 (cdr av6)))
    (setq av7 (assoc "map" count-alist))
    (should av7)
    (should (consp av7))
    (should (car av7))
    (should (stringp (car av7)))
    (should (string= "map" (car av7)))
    (should (cdr av7))
    (should (numberp (cdr av7)))
    (should (= 0 (cdr av7)))
    (setq av8 (assoc "Wheeeeeeeee" count-alist))
    (should (not av8))
    ))


;;
;;  shu-test-shu-match-make-class-hash-table-internal-1
;;
(ert-deftest shu-test-shu-match-make-class-hash-table-internal-1 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          (cons "std"    (list
                          "string"
                          "set"
                          "map"
                          ))))
        (ht)
        (hv))
    (setq ht (shu-match-make-class-hash-table-internal class-list gb))
    (should ht)
    (should (hash-table-p ht))
    (setq hv (gethash "AClass1" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "abcde" hv))
    (setq hv (gethash "AClass2" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "abcde" hv))
    (setq hv (gethash "Xclass1" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "xyrzk" hv))
    (setq hv (gethash "Xclass2" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "xyrzk" hv))
    (setq hv (gethash "string" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "std" hv))
    (setq hv (gethash "set" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "std" hv))
    (setq hv (gethash "map" ht))
    (should hv)
    (should (stringp hv))
    (should (string= "std" hv))
    ))




;;
;;  shu-test-shu-match-make-class-hash-table-internal-3
;;
(ert-deftest shu-test-shu-match-make-class-hash-table-internal-3 ()
  (let ((gb (get-buffer-create shu-unit-test-buffer))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "Xclass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "set"
                            "Xclass2"
                            ))
          (cons "std"    (list
                          "string"
                          "set"
                          "map"
                          ))))
        (ht)
        (hv))
    (setq ht (shu-match-make-class-hash-table-internal class-list gb))
    (should (not ht))
    ))



;;
;;  shu-test-shu-match-remove-proc-rlists-1
;;
(ert-deftest shu-test-shu-match-remove-proc-rlists-1 ()
  (let (
        (log-buf (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "\n"
          "    using namespace abcde;\n"
          "    x = x + 1;\n"
          "    using namespace xyrzk;\n"
          "    using std::string;\n"
          "    Xclass2     p;\n"
          "// Hello\n"
          ))
        (class-list
         (list
          (cons "abcde"    (list
                            "AClass1"
                            "AClass2"
                            ))
          (cons "xyrzk"    (list
                            "Xclass1"
                            "Xclass2"
                            ))
          )
         )
        (top-name "WhammoCorp")
        (token-list)
        (ret-val)
        (token-list)
        (proc-rlists)
        (prl)
        (rlist)
        )
    (with-temp-buffer
      (insert data)
      (setq ret-val (shu-test-shu-setup-proc-rlists-1 class-list log-buf top-name))
      (should ret-val)
      (should (consp ret-val))
      (setq token-list (car ret-val))
      (should token-list)
      (should (listp token-list))
      (setq proc-rlists (cdr ret-val))
      (should proc-rlists)
      (shu-cpp-tokenize-show-list-buffer token-list log-buf "\ntoken-list (setup):")
      (setq prl proc-rlists)
      (while prl
        (setq rlist (car prl))
        (shu-cpp-tokenize-show-list-buffer rlist log-buf "\nrlist (setup):")
        (setq prl (cdr prl))
        )
      (setq ret-val (shu-match-remove-proc-rlists token-list proc-rlists log-buf))
      (should ret-val)
      (should (consp ret-val))
      (setq token-list (car ret-val))
      (should token-list)
      (should (listp token-list))
      (shu-cpp-tokenize-show-list-buffer token-list log-buf "\ntoken-list (setup):")
      )
    ))


;;
;;  shu-test-shu-setup-proc-rlists-1
;;
(defun shu-test-shu-setup-proc-rlists-1 (class-list log-buf &optional top-name)
  "Create from the current buffer, an instance of proc-classes and an instance
of proc-rlist.  Return a cons cell in which the cdr is the instance of
proc-classes and the car is the instance of proc-rlists.

This is used to set up for a unit test that needs these two lists to
be in place."
  (let ((token-list)
        (ret-val)
        (something)
        (uns-list)
        (un-list)
        (proc-rlists)
        (proc-classes))
    (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
    (setq ret-val (shu-match-find-all-using-internal token-list))
    (should ret-val)
    (setq uns-list (car ret-val))
    (setq un-list (cdr ret-val))
    (should uns-list)
    (setq something (shu-match-merge-namespaces-with-class-list class-list uns-list log-buf top-name))
    (should something)
    (setq proc-classes (car something))
    (setq proc-rlists (cdr something))
    (setq something (shu-match-add-names-to-class-list un-list proc-classes proc-rlists log-buf top-name))
    (setq proc-classes (car something))
    (setq proc-rlists (cdr something))
    (setq proc-classes (remove-class-duplicates proc-classes log-buf))
    (cons token-list proc-rlists)
    ))




;;; shu-match.t.el ends here
