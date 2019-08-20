;;; shu-cpp-mch-funs.t.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-mch-funs
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

;; A collection of useful functions for dealing with C++ code.
;;
;;

;;; Code:



(require 'ert)
(require 'shu-cpp-mch-funs)



;;
;;  shu-cpp-mch-funs-test-data-1
;;
;; This tests the match lists (and the underlying match code) used by
;; shu-cpp-mch-funs.el
;;
(ert-deftest shu-cpp-mch-funs-test-data-1 ()
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
    (while tlist
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq item-number (1+ item-number))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ (format "item-number: %d\n" item-number) gb)
        (if (or
            (= item-number 2)
            (= item-number 5))
            (progn
;;              (should (not ret-val))
              (setq rlist nil)
              )
          (setq advance-tlist nil)
          (should ret-val)
          (should (consp ret-val))
          (setq tlist (car ret-val))
          (should tlist)
          (should (listp tlist))
          (setq rlist (cdr ret-val))
          (should rlist)
          (should (listp rlist))
          )
        (shu-cpp-tokenize-show-list rlist "FNS-RLIST")
        (shu-cpp-tokenize-show-list tlist "FNS-TLIST")
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )


    ))



;;
;;  shu-cpp-mch-funs-test-data-2
;;
;; This tests the match lists (and the underlying match code) used by
;; shu-cpp-mch-funs.el
;;
(ert-deftest shu-cpp-mch-funs-test-data-2 ()
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
        (running)
        )
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      )
    (setq tlist token-list)

    ;;;  Item number 1

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 1\n" gb)
        (setq advance-tlist nil)
        (setq running nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq nlist (nreverse rlist))

        (shu-cpp-tokenize-show-list nlist "FNS-NLIST1")
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
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST2")

    ;;;  Item number 2

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 2\n" gb)
        (setq advance-tlist t)
        (setq running nil)
        (should (not ret-val))
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST3")

    ;;;  Item number 3

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 3\n" gb)
        (setq advance-tlist nil)
        (setq running nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq nlist (nreverse rlist))

        (shu-cpp-tokenize-show-list nlist "FNS-NLIST1")
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
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST4")

    ;;;  Item number 4

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 4\n" gb)
        (setq advance-tlist nil)
        (setq running nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq nlist (nreverse rlist))

        (shu-cpp-tokenize-show-list nlist "FNS-NLIST1")
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
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST5")


    ;;;  Item number 5

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 5\n" gb)
        (setq advance-tlist t)
        (setq running nil)
        (princ "ret-val: " gb)(princ ret-val gb)(princ "\n" gb)
        ;;;(should (not ret-val))
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST6")


    ;;;  Item number 6

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 6\n" gb)
        (setq advance-tlist nil)
        (setq running nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq nlist (nreverse rlist))

        (shu-cpp-tokenize-show-list nlist "FNS-NLIST1")
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
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST7")


    ;;;  Item number 7

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 7\n" gb)
        (setq advance-tlist nil)
        (setq running nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq nlist (nreverse rlist))

        (shu-cpp-tokenize-show-list nlist "FNS-NLIST1")
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
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST8")


    ;;;  Item number 8

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 8\n" gb)
        (setq advance-tlist nil)
        (setq running nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq nlist (nreverse rlist))

        (shu-cpp-tokenize-show-list nlist "FNS-NLIST1")
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
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST9")


    ;;;  Item number 9

    (setq running t)
    (while (and tlist running)
      (setq token-info (car tlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq advance-tlist t)
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq rlist nil)
        (setq ret-val (shu-cpp-match-tokens rlist shu-cpp-mch-namespace-list tlist))
        (princ "item-number: 9\n" gb)
        (setq advance-tlist nil)
        (setq running nil)
        (should ret-val)
        (should (consp ret-val))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq nlist (nreverse rlist))

        (shu-cpp-tokenize-show-list nlist "FNS-NLIST1")
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
        )
      (when advance-tlist
        (setq tlist (cdr tlist))
        )
      )

    (shu-cpp-tokenize-show-list tlist "FNS-NLTIST10")






    ))





;;
;;  shu-cpp-mch-funs-test-search-1
;;
;; This tests the match lists (and the underlying match code) used by
;; shu-cpp-mch-funs.el
;;
(ert-deftest shu-cpp-mch-funs-test-search-1 ()
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-namespace-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-namespace-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-namespace-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-namespace-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-namespace-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-namespace-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-namespace-list-single tlist))
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
;;  shu-cpp-mch-funs-test-search-2
;;
;; This is a snippet of code that can find each occurrence of
;; \"using namespace\" and accumulate a list of such occurrences."
;;
(ert-deftest shu-cpp-mch-funs-test-search-2 ()
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
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-namespace-list-single tlist))
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
;;  shu-cpp-mch-funs-test-search-3
;;
;; This is a snippet of code that can find each occurrence of
;; \"using namespace\" and accumulate a list of such occurrences."
;;
(ert-deftest shu-cpp-mch-funs-test-search-3 ()
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
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
;;  shu-cpp-mch-funs-test-search-4
;;
;; This is a snippet of code that can find each occurrence of
;; \"using namespace\" and accumulate a list of such occurrences."
;;
(ert-deftest shu-cpp-mch-funs-test-search-4 ()
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
      (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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
    (setq ret-val (shu-cpp-search-match-tokens rlist shu-cpp-mch-using-list-single tlist))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst shu-match-inverse-op
  [0                       ;;   0  NUL (null)
   0                       ;;   1  SOH (start of heading)
   0                       ;;   2  STX (start of text)
   0                       ;;   3  ETX (end of text)
   0                       ;;   4  EOT (end of transmission)
   0                       ;;   5  ENQ (enquiry)
   0                       ;;   6  ACK (acknowledge)
   0                       ;;   7  BEL (bell)
   0                       ;;   8  BS  (backspace)
   0                       ;;   9  TAB (horizontal tab)
   0                       ;;  10  LF  (NL line feed  new line)
   0                       ;;  11  VT  (vertical tab)
   0                       ;;  12  FF  (NP form feed  new page)
   0                       ;;  13  CR  (carriage return)
   0                       ;;  14  SO  (shift out)
   0                       ;;  15  SI  (shift in)
   0                       ;;  16  DLE (data link escape)
   0                       ;;  17  DC1 (device control 1)
   0                       ;;  18  DC2 (device control 2)
   0                       ;;  19  DC3 (device control 3)
   0                       ;;  20  DC4 (device control 4)
   0                       ;;  21  NAK (negative acknowledge)
   0                       ;;  22  SYN (synchronous idle)
   0                       ;;  23  ETB (end of trans. block)
   0                       ;;  24  CAN (cancel)
   0                       ;;  25  EM  (end of medium)
   0                       ;;  26  SUB (substitute)
   0                       ;;  27  ESC (escape)
   0                       ;;  28  FS  (file separator)
   0                       ;;  29  GS  (group separator)
   0                       ;;  30  RS  (record separator)
   0                       ;;  31  US  (unit separator)
   0                       ;;  32  SPACE
   0                       ;;  33  !
   0                       ;;  34  "
   0                       ;;  35  #
   0                       ;;  36  $
   0                       ;;  37  %
   0                       ;;  38  &
   0                       ;;  39  '
   ?\)                     ;;  40  (
   ?\(                     ;;  41  )
   0                       ;;  42  *
   0                       ;;  43  +
   0                       ;;  44
   0                       ;;  45  -
   0                       ;;  46  .
   0                       ;;  47  /
   0                       ;;  48  0
   0                       ;;  49  1
   0                       ;;  50  2
   0                       ;;  51  3
   0                       ;;  52  4
   0                       ;;  53  5
   0                       ;;  54  6
   0                       ;;  55  7
   0                       ;;  56  8
   0                       ;;  57  9
   0                       ;;  58  :
   0                       ;;  59  ;
   ?>                      ;;  60  <
   0                       ;;  61  =
   ?<                      ;;  62  >
   0                       ;;  63  ?
   0                       ;;  64  @
   0                       ;;  65  A
   0                       ;;  66  B
   0                       ;;  67  C
   0                       ;;  68  D
   0                       ;;  69  E
   0                       ;;  70  F
   0                       ;;  71  G
   0                       ;;  72  H
   0                       ;;  73  I
   0                       ;;  74  J
   0                       ;;  75  K
   0                       ;;  76  L
   0                       ;;  77  M
   0                       ;;  78  N
   0                       ;;  79  O
   0                       ;;  80  P
   0                       ;;  81  Q
   0                       ;;  82  R
   0                       ;;  83  S
   0                       ;;  84  T
   0                       ;;  85  U
   0                       ;;  86  V
   0                       ;;  87  W
   0                       ;;  88  X
   0                       ;;  89  Y
   0                       ;;  90  Z
   ?\]                     ;;  91  [
   0                       ;;  92  (back slash)
   ?\[                     ;;  93  ]
   0                       ;;  94  ^
   0                       ;;  95  _
   0                       ;;  96  `
   0                       ;;  97  a
   0                       ;;  98  b
   0                       ;;  99  c
   0                       ;; 100  d
   0                       ;; 101  e
   0                       ;; 102  f
   0                       ;; 103  g
   0                       ;; 104  h
   0                       ;; 105  i
   0                       ;; 106  j
   0                       ;; 107  k
   0                       ;; 108  l
   0                       ;; 109  m
   0                       ;; 110  n
   0                       ;; 111  o
   0                       ;; 112  p
   0                       ;; 113  q
   0                       ;; 114  r
   0                       ;; 115  s
   0                       ;; 116  t
   0                       ;; 117  u
   0                       ;; 118  v
   0                       ;; 119  w
   0                       ;; 120  x
   0                       ;; 121  y
   0                       ;; 122  z
   ?}                      ;; 123  {
   0                       ;; 124  |
   ?{                      ;; 125  }
   0                       ;; 126  ~
   0                       ;; 127  DEL
   0                       ;; 128
   0                       ;; 129
   0                       ;; 130
   0                       ;; 131
   0                       ;; 132
   0                       ;; 133
   0                       ;; 134
   0                       ;; 135
   0                       ;; 136
   0                       ;; 137
   0                       ;; 138
   0                       ;; 139
   0                       ;; 140
   0                       ;; 141
   0                       ;; 142
   0                       ;; 143
   0                       ;; 144
   0                       ;; 145
   0                       ;; 146
   0                       ;; 147
   0                       ;; 148
   0                       ;; 149
   0                       ;; 150
   0                       ;; 151
   0                       ;; 152
   0                       ;; 153
   0                       ;; 154
   0                       ;; 155
   0                       ;; 156
   0                       ;; 157
   0                       ;; 158
   0                       ;; 159
   0                       ;; 160
   0                       ;; 161
   0                       ;; 162
   0                       ;; 163
   0                       ;; 164
   0                       ;; 165
   0                       ;; 166
   0                       ;; 167
   0                       ;; 168
   0                       ;; 169
   0                       ;; 170
   0                       ;; 171
   0                       ;; 172
   0                       ;; 173
   0                       ;; 174
   0                       ;; 175
   0                       ;; 176
   0                       ;; 177
   0                       ;; 178
   0                       ;; 179
   0                       ;; 180
   0                       ;; 181
   0                       ;; 182
   0                       ;; 183
   0                       ;; 184
   0                       ;; 185
   0                       ;; 186
   0                       ;; 187
   0                       ;; 188
   0                       ;; 189
   0                       ;; 190
   0                       ;; 191
   0                       ;; 192
   0                       ;; 193
   0                       ;; 194
   0                       ;; 195
   0                       ;; 196
   0                       ;; 197
   0                       ;; 198
   0                       ;; 199
   0                       ;; 200
   0                       ;; 201
   0                       ;; 202
   0                       ;; 203
   0                       ;; 204
   0                       ;; 205
   0                       ;; 206
   0                       ;; 207
   0                       ;; 208
   0                       ;; 209
   0                       ;; 210
   0                       ;; 211
   0                       ;; 212
   0                       ;; 213
   0                       ;; 214
   0                       ;; 215
   0                       ;; 216
   0                       ;; 217
   0                       ;; 218
   0                       ;; 219
   0                       ;; 220
   0                       ;; 221
   0                       ;; 222
   0                       ;; 223
   0                       ;; 224
   0                       ;; 225
   0                       ;; 226
   0                       ;; 227
   0                       ;; 228
   0                       ;; 229
   0                       ;; 230
   0                       ;; 231
   0                       ;; 232
   0                       ;; 233
   0                       ;; 234
   0                       ;; 235
   0                       ;; 236
   0                       ;; 237
   0                       ;; 238
   0                       ;; 239
   0                       ;; 240
   0                       ;; 241
   0                       ;; 242
   0                       ;; 243
   0                       ;; 244
   0                       ;; 245
   0                       ;; 246
   0                       ;; 247
   0                       ;; 248
   0                       ;; 249
   0                       ;; 250
   0                       ;; 251
   0                       ;; 252
   0                       ;; 253
   0                       ;; 254
   0]                      ;; 255
  "A vector of length 256 that can be indexed by a character value.  If the character is
an opening or closing delimeter, the value found at the index is the inverse delimeter.
e.g., If the character '{', then (aref shu-match-inverse-op char) yields '}'.  If the
character is '>', then (aref shu-match-inverse-op char) yields '<'.")

;;; NB: Use aref to extract the first character from a token-value as an integer.



;;
;;  shu-test-shu-match-inverse-op
;;
(ert-deftest shu-test-shu-match-inverse-op ()
  (let (
        (tless ?<)
        (tgreater ?>)
        (tlparen ?\()
        (trparen ?\))
        (tlbrace ?{)
        (trbrace ?})
        (tlbrack ?\[)
        (trbrack ?\])
        (x)
        )

    (setq x (aref shu-match-inverse-op tless))
    (should (= x tgreater))
    (setq x (aref shu-match-inverse-op tgreater))
    (should (= x tless))
    (setq x (aref shu-match-inverse-op tlparen))
    (should (= x trparen))
    (setq x (aref shu-match-inverse-op trparen))
    (should (= x tlparen))
    (setq x (aref shu-match-inverse-op tlbrace))
    (should (= x trbrace))
    (setq x (aref shu-match-inverse-op trbrace))
    (should (= x tlbrace))
    (setq x (aref shu-match-inverse-op trbrace))
    (should (= x tlbrace))
    (setq x (aref shu-match-inverse-op trbrack))
    (should (= x tlbrack))
    (setq x (aref shu-match-inverse-op trbrack))
    (should (= x tlbrack))
    ))


;;
;;  shu-test-something
;;
(ert-deftest shu-test-something ()
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          "bsl::vector<bsl::vector<std::string> >\n"
          ))
        (token-list)
        (tlist)
        (level 0)
        (token-info-start)
        (token-info)
        (token-info-end)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message)
        (sdelim)
        (edelim)
        (sstring)
        (estring)
        (looking)
        (sp1)
        (sp2)
        (x)
        )
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-reverse-tokenize-region-for-command  (point-min) (point-max)))
      (shu-cpp-tokenize-show-list token-list)
      (setq tlist token-list)
      (setq token-info-start (car tlist))
      (shu-cpp-token-extract-info token-info-start token token-type spoint epoint error-message)
      (setq sstring token)
      (setq sdelim (aref token 0))
      (setq edelim (aref shu-match-inverse-op sdelim))
      (setq estring (make-string 1 edelim))
      (princ (format "estring: \"%s\"\n" estring) gb)
      (setq looking t)
      (setq level 1)
      (setq tlist (cdr tlist))
      (while (and tlist looking)
        (setq token-info (car tlist))
        (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
        (shu-cpp-token-show-token-info token-info)
        (princ (format "level: %d\n" level) gb)
        (if (and
             (= token-type shu-cpp-token-type-op)
             (string= estring token)
             )
            (progn
              (setq level (1- level))
              (when (= level 0)
                (setq looking nil)
                (setq token-info-end token-info)
                )
              )
          (when (and
                 (= token-type shu-cpp-token-type-op)
                 (string= sstring token)
                 (setq level (1+ level))
                 )
            )
          )
        (setq tlist (cdr tlist))
        )
      (shu-cpp-token-show-token-info token-info-end "END")
      (setq sp1 (shu-cpp-token-extract-spoint token-info-start))
      (setq sp2 (shu-cpp-token-extract-spoint token-info-end))
      (if (< sp1 sp2)
          (progn
            (setq spoint (1+ (shu-cpp-token-extract-epoint token-info-start)))
            (setq epoint sp2)
            )
        (setq spoint (1+ (shu-cpp-token-extract-epoint token-info-end)))
        (setq epoint sp1)
        )
      (princ (format "spoint: %d, epoint: %d\n" spoint epoint) gb)
      (setq x (buffer-substring-no-properties spoint epoint))
      (princ (format "\"%s\"\n" x) gb)
      )


    ))



;;; shu-cpp-mch-funs.t.el ends here
