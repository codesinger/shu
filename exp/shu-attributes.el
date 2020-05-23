;;; shu-attributes.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2020 Stewart L. Palmer
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

;; A collection of experimental functions for dealing with C++ code.
;;
;;

;;; Code:


;;Things to copy per attribute
;;
;;   - name
;;   - data type
;;   - full data type
;;   - nullable
;;   - comment
;;
;;
;;
;;
;;  attr-info
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> attr-ext
;;        |
;;        +-------------> name
;;
;;
;;
;;
;;  attr-ext
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> attr-other
;;        |
;;        +-------------> full-data-type
;;
;;
;;
;;
;;
;;  attr-other
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> attr-cmt
;;        |
;;        +-------------> data-type
;;
;;
;;
;;
;;
;;  attr-cmt
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> nullable
;;        |
;;        +-------------> comment
;;
;;
;;
;;
;;
;;
;;  shu-cpp-extract-attr-info
;;
(defmacro shu-cpp-extract-attr-info (attr-info name data-type full-data-type comment nullable)
  "Extract the information out of an attr-info"
  (let (
        (tattr-ext (make-symbol "attr-ext"))
        (tattr-other (make-symbol "attr-other"))
        (tattr-cmt (make-symbol "attr-cmt"))
        )
    `(let (
           (,tattr-ext)
           (,tattr-other)
           (,tattr-cmt)
           )
       (setq ,name (car ,attr-info))
       (setq ,tattr-ext (cdr ,attr-info))
       (setq ,full-data-type (car ,tattr-ext))
       (setq ,tattr-other (cdr ,tattr-ext))
       (setq ,data-type (car ,tattr-other))
       (setq ,tattr-cmt (cdr ,tattr-other))
       (setq ,comment (car ,tattr-cmt))
       (setq ,nullable (cdr ,tattr-cmt))
       )
    ))



;;
;;  shu-cpp-make-attr-info
;;
(defun shu-cpp-make-attr-info (name data-type full-data-type &optional comment nullable)
  "Return an attr-info created from the given arguments"
  (setq debug-on-error t)
  (cons name
        (cons full-data-type
              (cons data-type
                    (cons comment nullable))))
  )




;;
;;  shu-test-shu-cpp-make-attr-info-1
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-1 ()
  (let (
        (name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (nullable t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment nullable))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xnullable)
    (should xname)
    (should (stringp xname))
    (should (string= name xname))

    (should xdata-type)
    (should (stringp xdata-type))
    (should (string= data-type xdata-type))

    (should xfull-data-type)
    (should (stringp xfull-data-type))
    (should (string= full-data-type xfull-data-type))

    (should xcomment)
    (should (stringp xcomment))
    (should (string= comment xcomment))

    (should xnullable)
    ))




;;
;;  shu-test-shu-cpp-make-attr-info-2
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-2 ()
  (let (
        (name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (nullable t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment xnullable)
    (should xname)
    (should (stringp xname))
    (should (string= name xname))

    (should xdata-type)
    (should (stringp xdata-type))
    (should (string= data-type xdata-type))

    (should xfull-data-type)
    (should (stringp xfull-data-type))
    (should (string= full-data-type xfull-data-type))

    (should (not xcomment))

    (should (not xnullable))
    ))



;;; shu-attributes.el ends here
