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
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
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
        (item-number 0)
        )
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      )
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
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
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
        (lcount 0)
        )
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      )
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
        (setq rlist nil)
      )
      )
    (setq rlists (nreverse rlists))
    (while rlists
      (setq lcount (1+ lcount))
      (princ (format "\nRLIST %d:\n" lcount) gb)
      (setq rlist (car rlists))
      (shu-cpp-tokenize-show-list rlist)
      (setq rlists (cdr rlists))
      )
))







;;
;;  shu-cpp-match-funs-test-search-3
;;
;; This is a snippet of code that can find each occurrence of
;; \"using namespace\" and accumulate a list of such occurrences."
;;
(ert-deftest shu-cpp-match-funs-test-search-3 ()
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
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
        (lcount 0)
        )
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      )
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
        (setq rlist nil)
      )
      )
    (setq rlists (nreverse rlists))
    (while rlists
      (setq lcount (1+ lcount))
      (princ (format "\nRLIST %d:\n" lcount) gb)
      (setq rlist (car rlists))
      (shu-cpp-tokenize-show-list rlist)
      (setq rlists (cdr rlists))
      )
))






;;
;;  shu-cpp-match-funs-test-search-4
;;
;; This is a snippet of code that can find each occurrence of
;; \"using namespace\" and accumulate a list of such occurrences."
;;
(ert-deftest shu-cpp-match-funs-test-search-4 ()
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
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
        (lcount 0)
        )
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      )
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
        (setq rlist nil)
      )
      )
    (setq rlists (nreverse rlists))
    (while rlists
      (setq lcount (1+ lcount))
      (princ (format "\nRLIST %d:\n" lcount) gb)
      (setq rlist (car rlists))
      (shu-cpp-tokenize-show-list rlist)
      (setq rlists (cdr rlists))
      )

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





;;; shu-match.t.el ends here
