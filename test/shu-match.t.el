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



;;; shu-match.t.el ends here
