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

;;; shu-cpp-mch-funs.t.el ends here
