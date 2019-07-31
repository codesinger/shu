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
(ert-deftest shu-cpp-mch-funs-test-data-1 ()
  (let (
        (data
         (concat
          "/*!\n"
          " * \\file something_or_other.cpp\n"
          " */\n"
          "\n"
          "#include <strng>\n"
          "\n"
          "using namespace std;\n"
          "// using namespace nonsense\n"
          "using namespace /* Hello */ component::SomeClass;\n"
          "using namespace Whammo::other::OtherClass;\n"
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
        (token-info)
        (ret-val)
        (token)
        (token-type)
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
      (when (and
             (= token-type shu-cpp-token-type-kw)
             (string= token "using"))
        (setq ret-val (shu-cpp-match-tokens shu-cpp-mch-namespace-list tlist))
        (should ret-val)
        (should (consp ret-val))
        (setq rlist (cdr ret-val))
        (should rlist)
        (should (listp rlist))
        (setq tlist (car ret-val))
        (should tlist)
        (should (listp tlist))
        (shu-cpp-tokenize-show-list rlist "FNS-RLIST")
        (shu-cpp-tokenize-show-list tlist "FNS-TLIST")
        )
      (setq tlist (cdr tlist))
      )


    ))




;;; shu-cpp-mch-funs.t.el ends here
