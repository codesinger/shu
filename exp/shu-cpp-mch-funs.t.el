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
        (setq ret-val (shu-cpp-match-tokens shu-cpp-mch-namespace-list tlist))
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




;;; shu-cpp-mch-funs.t.el ends here
