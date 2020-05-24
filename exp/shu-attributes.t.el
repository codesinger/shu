;;; shu-attributes.t.el --- Shu project code unit tests
;;
;; Copyright (C) 2020 Stewart L. Palmer
;;
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

(require 'ert)
(require 'shu-attributes)






;;
;;  shu-test-shu-cpp-make-attr-info-1
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-1 ()
  (let ((name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (nullable t)
        (column-name "MumbleBar::otherThing")
        (reference t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xreference))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)
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

    (should xcolumn-name)
    (should (stringp xcolumn-name))
    (should (string= column-name xcolumn-name))

    (should xreference)

    (should xnullable)
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-2
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-2()
  (let ((name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference nil)
        (nullable t)
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xreference))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)

    (should (not xreference))

    (should xnullable)
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-3
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-3()
  (let ((name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference t)
        (nullable nil)
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xreference))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)

    (should xreference)

    (should (not xnullable))
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-4
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-4()
  (let ((name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference nil)
        (nullable nil)
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xreference))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)

    (should (not xreference))

    (should (not xnullable))
    ))




;;
;;  shu-test-shu-cpp-make-attr-info-5
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-5 ()
  (let ((name "fred")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (nullable t)
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xreference)
        (xnullable)
        (xcolumn-name))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name)
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

    (should (not xreference))
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-name
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-name ()
  (let ((name "frederick")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference)
        (nullable t)
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (setq xname (shu-cpp-extract-attr-info-name attr-info))
    (should xname)
    (should (stringp xname))
    (should (string= name xname))
    ))




;;
;;  shu-test-shu-upcase-first-letter-1
;;
(ert-deftest shu-test-shu-upcase-first-letter-1 ()
  (let ((phrase "now is the time")
        (expected "Now is the time")
        (actual))
    (setq actual (shu-upcase-first-letter phrase))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-downcase-first-letter-1
;;
(ert-deftest shu-test-shu-downcase-first-letter-1 ()
  (let ((phrase "Now is the time")
        (expected "now is the time")
        (actual))
    (setq actual (shu-downcase-first-letter phrase))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;; shu-attributes.t.el ends here
