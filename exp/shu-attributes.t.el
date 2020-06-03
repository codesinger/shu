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
        (column-count 8)
        (enum-base "int")
        (reset-value "8097")
        (reference t)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xcolumn-count)
        (xenum-base)
        (xreset-value)
        (xreference))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name column-count
                                            enum-base reset-value))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name xcolumn-count
                               xenum-base xreset-value)
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

    (should xcolumn-count)
    (should (numberp xcolumn-count))
    (should (= column-count xcolumn-count))

    (should xenum-base)
    (should (stringp xenum-base))
    (should (string= enum-base xenum-base))

    (should xreset-value)
    (should (stringp xreset-value))
    (should (string= reset-value xreset-value))
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
        (column-count 5)
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
        (xcomment)
        (xnullable)
        (xcolumn-name)
        (xcolumn-count)
        (xenum-base)
        (xreset-value)
        (xreference))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name column-count))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name xcolumn-count
                               xenum-base xreset-value)

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
        (xcolumn-count)
        (xenum-base)
        (xreset-value)
        (xreference))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name xcolumn-count
                               xenum-base xreset-value)

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
        (xcolumn-count)
        (xenum-base)
        (xreset-value)
        (xreference))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name xcolumn-count
                               xenum-base xreset-value)

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
        (xcolumn-name)
        (xcolumn-count)
        (xenum-base)
        (xreset-value))
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type))
    (shu-cpp-extract-attr-info attr-info xname xdata-type xfull-data-type xcomment
                               xreference xnullable xcolumn-name xcolumn-count
                               xenum-base xreset-value)
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
;;  shu-test-shu-cpp-extract-attr-info-type
;;
(ert-deftest shu-test-shu-cpp-extract-attr-info-type ()
  (let (
        (name "frederick")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference)
        (nullable t)
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
        (xdata-type)
        (xfull-data-type)
          )
    (setq attr-info (shu-cpp-make-attr-info name data-type full-data-type comment
                                            reference nullable column-name))
    (shu-cpp-extract-attr-info-type attr-info xname xdata-type xfull-data-type)
    (should xname)
    (should (stringp xname))
    (should (string= name xname))

    (should xdata-type)
    (should (stringp xdata-type))
    (should (string= data-type xdata-type))

    (should xfull-data-type)
    (should (stringp xfull-data-type))
    (should (string= full-data-type xfull-data-type))
    ))



;;
;;  shu-test-shu-cpp-make-attr-info-name
;;
(ert-deftest shu-test-shu-cpp-make-attr-info-name ()
  (let (
        (name "frederick")
        (data-type "std::string")
        (full-data-type "std::optional<std::string>")
        (comment "This is a comment")
        (reference)
        (nullable t)
        (column-name "MumbleBar::otherThing")
        (attr-info)
        (xname)
          )
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





;;
;;  shu-test-shu-cpp-attributes-header-type-1
;;
(ert-deftest shu-test-shu-cpp-attributes-header-type-1 ()
  (let ((class-name "farble")
        (data-type "Double")
        (unqualified-type))
    (setq unqualified-type (shu-cpp-attributes-header-type class-name data-type))
    (should unqualified-type)
    (should (stringp unqualified-type))
    (should (string= data-type unqualified-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-header-type-2
;;
(ert-deftest shu-test-shu-cpp-attributes-header-type-2 ()
  (let ((class-name "farble")
        (data-type "farble::Mumble")
        (unqualified-type)
        (expected-type "Mumble"))
    (setq unqualified-type (shu-cpp-attributes-header-type class-name data-type))
    (should unqualified-type)
    (should (stringp unqualified-type))
    (should (string= expected-type unqualified-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-aggregate-type-1
;;
(ert-deftest shu-test-shu-cpp-attributes-aggregate-type-1 ()
  (let (
        (data-type "bsl::string")
        (aggregate-type)
        (expected-aggregate-type "asString()")
        )
    (setq aggregate-type (shu-cpp-attributes-aggregate-type data-type))
    (should aggregate-type)
    (should (stringp aggregate-type))
    (should (string= expected-aggregate-type aggregate-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-aggregate-type-2
;;
(ert-deftest shu-test-shu-cpp-attributes-aggregate-type-2 ()
  (let (
        (data-type "bdlt::Datetime")
        (aggregate-type)
        (expected-aggregate-type "asDatetimeTz().utcDatetime()")
        )
    (setq aggregate-type (shu-cpp-attributes-aggregate-type data-type))
    (should aggregate-type)
    (should (stringp aggregate-type))
    (should (string= expected-aggregate-type aggregate-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-aggregate-type-3
;;
(ert-deftest shu-test-shu-cpp-attributes-aggregate-type-3 ()
  (let (
        (data-type "bdlt::DatetimeTz")
        (aggregate-type)
        (expected-aggregate-type "asDatetimeTz()")
        )
    (setq aggregate-type (shu-cpp-attributes-aggregate-type data-type))
    (should aggregate-type)
    (should (stringp aggregate-type))
    (should (string= expected-aggregate-type aggregate-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-aggregate-type-4
;;
(ert-deftest shu-test-shu-cpp-attributes-aggregate-type-4 ()
  (let (
        (data-type "bdlt::DatetimeInterval")
        (aggregate-type)
        (expected-aggregate-type "asInt()")
        )
    (setq aggregate-type (shu-cpp-attributes-aggregate-type data-type))
    (should aggregate-type)
    (should (stringp aggregate-type))
    (should (string= expected-aggregate-type aggregate-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-aggregate-type-5
;;
(ert-deftest shu-test-shu-cpp-attributes-aggregate-type-5 ()
  (let (
        (data-type "bsls::Types::Int64")
        (aggregate-type)
        (expected-aggregate-type "asInt()")
        )
    (setq aggregate-type (shu-cpp-attributes-aggregate-type data-type))
    (should aggregate-type)
    (should (stringp aggregate-type))
    (should (string= expected-aggregate-type aggregate-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-aggregate-type-6
;;
(ert-deftest shu-test-shu-cpp-attributes-aggregate-type-6 ()
  (let (
        (data-type "int")
        (aggregate-type)
        (expected-aggregate-type "asInt()")
        )
    (setq aggregate-type (shu-cpp-attributes-aggregate-type data-type))
    (should aggregate-type)
    (should (stringp aggregate-type))
    (should (string= expected-aggregate-type aggregate-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-aggregate-type-7
;;
(ert-deftest shu-test-shu-cpp-attributes-aggregate-type-7 ()
  (let (
        (data-type "double")
        (aggregate-type)
        (expected-aggregate-type "asDouble()")
        )
    (setq aggregate-type (shu-cpp-attributes-aggregate-type data-type))
    (should aggregate-type)
    (should (stringp aggregate-type))
    (should (string= expected-aggregate-type aggregate-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-bind-type-1
;;
(ert-deftest shu-test-shu-cpp-attributes-bind-type-1 ()
  (let (
        (data-type "bsl::string")
        (bind-type)
        (expected-bind-type "Text")
        )
    (setq bind-type (shu-cpp-attributes-bind-type data-type))
    (should bind-type)
    (should (stringp bind-type))
    (should (string= bind-type expected-bind-type))

    (setq data-type "bdlt::Datetime")
    (setq bind-type nil)
    (setq expected-bind-type "Datetime")
    (setq bind-type (shu-cpp-attributes-bind-type data-type))
    (should bind-type)
    (should (stringp bind-type))
    (should (string= bind-type expected-bind-type))

    (setq data-type "bdlt::DatetimeTz")
    (setq bind-type nil)
    (setq expected-bind-type "DatetimeTz")
    (setq bind-type (shu-cpp-attributes-bind-type data-type))
    (should bind-type)
    (should (stringp bind-type))
    (should (string= bind-type expected-bind-type))

    (setq data-type "bdlt::DatetimeInterval")
    (setq bind-type nil)
    (setq expected-bind-type "Int")
    (setq bind-type (shu-cpp-attributes-bind-type data-type))
    (should bind-type)
    (should (stringp bind-type))
    (should (string= bind-type expected-bind-type))

    (setq data-type "bsls::Types::Int64")
    (setq bind-type nil)
    (setq expected-bind-type "Int")
    (setq bind-type (shu-cpp-attributes-bind-type data-type))
    (should bind-type)
    (should (stringp bind-type))
    (should (string= bind-type expected-bind-type))

    (setq data-type "int")
    (setq bind-type nil)
    (setq expected-bind-type "Int")
    (setq bind-type (shu-cpp-attributes-bind-type data-type))
    (should bind-type)
    (should (stringp bind-type))
    (should (string= bind-type expected-bind-type))

    (setq data-type "double")
    (setq bind-type nil)
    (setq expected-bind-type "Double")
    (setq bind-type (shu-cpp-attributes-bind-type data-type))
    (should bind-type)
    (should (stringp bind-type))
    (should (string= bind-type expected-bind-type))
    ))




;;
;;  shu-test-shu-cpp-attributes-current-data-type-1
;;
(ert-deftest shu-test-shu-cpp-attributes-current-data-type-1 ()
  (let (
        (enum-base)
        (data-type "std::string")
        (current-data-type)
        )
    (setq current-data-type (shu-cpp-attributes-current-data-type data-type enum-base))
    (should current-data-type)
    (should (stringp current-data-type))
    (should (string= data-type current-data-type))
    ))




;;
;;  shu-test-shu-cpp-attributes-current-data-type-2
;;
(ert-deftest shu-test-shu-cpp-attributes-current-data-type-2 ()
  (let (
        (enum-base "int")
        (data-type "Mumble::Bar")
        (current-data-type)
        )
    (setq current-data-type (shu-cpp-attributes-current-data-type data-type enum-base))
    (should current-data-type)
    (should (stringp current-data-type))
    (should (string= enum-base current-data-type))
    ))



;;
;;  shu-test-shu-cpp-attributes-make-pad-1
;;
(ert-deftest shu-test-shu-cpp-attributes-make-pad-1 ()
  (let ((max-type-len (length "std::string"))
        (reference)
        (data-type "std::st")
        (pad)
        (expected-pad (make-string 8 ? )))
    (setq pad (shu-cpp-attributes-make-pad max-type-len reference data-type))
    (should pad)
    (should (stringp pad))
    (should (string= expected-pad pad))
    ))



;;
;;  shu-test-shu-cpp-attributes-make-pad-2
;;
(ert-deftest shu-test-shu-cpp-attributes-make-pad-2 ()
  (let ((max-type-len (length "std::string"))
        (reference t)
        (data-type "std::st")
        (pad)
        (expected-pad (concat (make-string 7 ? ) "&")))
    (setq pad (shu-cpp-attributes-make-pad max-type-len reference data-type))
    (should pad)
    (should (stringp pad))
    (should (string= expected-pad pad))
    ))



;;;  SAMPLE INPUT
;;;
;;;  class TenorRow
;;;
;;;  table TenorColumnNames::table
;;;
;;;  // The name of the tenor
;;;  TenorColumnNames::name             bsl::string     name    &
;;;
;;;  // The primary key of the cross parent
;;;  std(5)                               CrossRowPrimaryKey   key &
;;;
;;;  // Time that this definition was deleted
;;;  TenorColumnNames::deleteTime       bdlt::Datetime  deleteTime  &
;;;
;;;  // Pricing start time
;;;  TenorColumnNames::startTime        bdlt::Datetime  startTime   &  t
;;;
;;;  // Time zone for start time
;;;  TenorColumnNames::timezone         bsl::string     timezone    &  t
;;;
;;;  // Pricing frequency
;;;  TenorColumnNames::frequency        bdlt::DatetimeInterval  frequency    &
;;;
;;;  // Maximum age for first ISO
;;;  TenorColumnNames::ageout1          bdlt::DatetimeInterval  ageout1      &  t
;;;
;;;  // Maximum age for second ISO
;;;  TenorColumnNames::ageout2          bdlt::DatetimeInterval  ageout2      &  t
;;;
;;;  // Tick override
;;;  TenorColumnNames::tickerOverride   bsl::string     tickerOverride  &  t
;;;
;;;  // via ISO
;;;  TenorColumnNames::viaIso           bsl::string     viaIso   &  t
;;;
;;;  // Note for the tenor
;;;  TenorColumnNames::note             bsl::string     note     &  t
;;;

;;; shu-attributes.t.el ends here
