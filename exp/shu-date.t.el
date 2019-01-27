;;; shu-date.t.el --- Shu project code unit tests
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Author: Stewart L. Pslmer <stewart@stewartpalmer.com>
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
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;

(require 'ert)
;;(require 'shu-date)

;;; Code




;;
;;  shu-test-shu-date-make-date-1
;;
(ert-deftest shu-test-shu-date-make-date-1 ()
  "Doc string."
  (let ((date-cons)
        (serial-day 1234567)
        (expected-day))
    (shu-date-make-date date-cons serial-day)
    (should (consp date-cons))
    (should (numberp (cdr date-cons)))
    (should (= serial-day (cdr date-cons)))
    (shu-date-extract-date expected-day date-cons)
    (should expected-day)
    (should (numberp expected-day))
    (should (= expected-day serial-day))
    ))



;;
;;  shu-test-shu-date-make-time-1
;;
(ert-deftest shu-test-shu-date-make-time-1 ()
  "Doc string."
  (let ((time-cons)
        (seconds 1234567)
        (microseconds 654321)
        (actual-seconds)
        (actual-microseconds))
    (shu-date-make-time time-cons seconds microseconds)
    (should (consp time-cons))
    (should (numberp (car time-cons)))
    (shu-date-extract-time actual-seconds actual-microseconds time-cons)
    (should actual-seconds)
    (should (numberp actual-seconds))
    (should actual-microseconds)
    (should (numberp actual-microseconds))
    (should (= seconds actual-seconds))
    (should (= microseconds actual-microseconds))
    ))



;;
;;  shu-test-shu-date-make-datetime-1
;;
(ert-deftest shu-test-shu-date-make-datetime-1 ()
  "Doc string."
  (let ((datetime-cons)
        (serial-day 90283911)
        (seconds 1234567)
        (microseconds 654321)
        (actual-serial-day)
        (actual-seconds)
        (actual-microseconds))
    (shu-date-make-datetime datetime-cons serial-day seconds microseconds)
    (should (consp datetime-cons))
    (shu-date-extract-datetime actual-serial-day actual-seconds actual-microseconds datetime-cons)
    (should actual-serial-day)
    (should (numberp actual-serial-day))
    (should actual-seconds)
    (should (numberp actual-seconds))
    (should actual-microseconds)
    (should (numberp actual-microseconds))
    (should (= serial-day actual-serial-day))
    (should (= seconds actual-seconds))
    (should (= microseconds actual-microseconds))
    ))


;;
;;  shu-test-shu-convert-to-datetime-1-1
;;
(ert-deftest shu-test-shu-convert-to-datetime-1-1 ()
  (let (
        (data "2019-01-22T092123.321")
        (expected
         (cons 2458506          ;; Julian day
               (cons 33683      ;; Seconds since midnght
                     321000)))  ;; Microseconds
        (actual)
        (fmt)
        )
    (setq debug-on-error t)
    (setq actual (shu-convert-to-datetime-1 data))
    (should actual)
    (should (consp actual))
    (should (consp (cdr actual)))
    (should (equal expected actual))
    (setq fmt (shu-format-datetime-1 actual))
    (should fmt)
    (should (stringp fmt))
    (should (string= data fmt))
    ))



;;
;;  shu-test-shu-mon-to-number
;;
(ert-deftest shu-test-shu-mon-to-number ()
    (should (= 1 (shu-mon-to-number "jan")))
    (should (= 2 (shu-mon-to-number "feb")))
    (should (= 3 (shu-mon-to-number "mar")))
    (should (= 4 (shu-mon-to-number "apr")))
    (should (= 5 (shu-mon-to-number "may")))
    (should (= 6 (shu-mon-to-number "jun")))
    (should (= 7 (shu-mon-to-number "jul")))
    (should (= 8 (shu-mon-to-number "aug")))
    (should (= 9 (shu-mon-to-number "sep")))
    (should (= 10 (shu-mon-to-number "oct")))
    (should (= 11 (shu-mon-to-number "nov")))
    (should (= 12 (shu-mon-to-number "dec")))
    (should (= 0 (shu-mon-to-number "oops")))
    (should (= 1 (shu-mon-to-number "JAN")))
    (should (= 2 (shu-mon-to-number "FEB")))
    (should (= 3 (shu-mon-to-number "Mar")))
    (should (= 4 (shu-mon-to-number "APR")))
    (should (= 5 (shu-mon-to-number "May")))
    (should (= 6 (shu-mon-to-number "JUN")))
    (should (= 7 (shu-mon-to-number "Jul")))
    (should (= 8 (shu-mon-to-number "Aug")))
    (should (= 9 (shu-mon-to-number "SEP")))
    (should (= 10 (shu-mon-to-number "Oct")))
    (should (= 11 (shu-mon-to-number "NOV")))
    (should (= 12 (shu-mon-to-number "Dec")))
    (should (= 0 (shu-mon-to-number "Oops")))
    )



;;
;;  shu-test-shu-number-to-mon
;;
(ert-deftest shu-test-shu-number-to-mon ()
    (should (string= "jan" (shu-number-to-mon 1)))
    (should (string= "feb" (shu-number-to-mon 2)))
    (should (string= "mar" (shu-number-to-mon 3)))
    (should (string= "apr" (shu-number-to-mon 4)))
    (should (string= "may" (shu-number-to-mon 5)))
    (should (string= "jun" (shu-number-to-mon 6)))
    (should (string= "jul" (shu-number-to-mon 7)))
    (should (string= "aug" (shu-number-to-mon 8)))
    (should (string= "sep" (shu-number-to-mon 9)))
    (should (string= "oct" (shu-number-to-mon 10)))
    (should (string= "nov" (shu-number-to-mon 11)))
    (should (string= "dec" (shu-number-to-mon 12)))
    )



;;
;;  shu-test-shu-seconds-to-hhmmss-1
;;
(ert-deftest shu-test-shu-seconds-to-hhmmss-1 ()
  (let ((data (+ (* 5 60 60) (* 4 60) 3))
        (expected  (list 5 4 3))
        (actual))
    (setq actual (shu-seconds-to-hhmmss data))
    (should actual)
    (should (listp actual))
    (should (= 3 (length actual)))
    (should (equal expected actual))
    ))



;;
;;  shu-test-shu-seconds-to-hhmmss-2
;;
(ert-deftest shu-test-shu-seconds-to-hhmmss-2 ()
  (let ((data (+ (* 23 60 60) (* 59 60) 59))
        (expected  (list 23 59 59))
        (actual))
    (setq actual (shu-seconds-to-hhmmss data))
    (should actual)
    (should (listp actual))
    (should (= 3 (length actual)))
    (should (equal expected actual))
    ))




;;
;;  shu0-test-shu-micros-to-millis-1
;;
(ert-deftest shu0-test-shu-micros-to-millis-1 ()
  (let ((data (* 11 1000 1000))
        (expected (* 11 1000))
        (actual))
    (setq actual (shu-micros-to-millis data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu0-test-shu-micros-to-millis-2
;;
(ert-deftest shu0-test-shu-micros-to-millis-2 ()
  (let ((data (+ (* 11 1000 1000) 800))
        (expected (+ (* 11 1000) 1))
        (actual))
    (setq actual (shu-micros-to-millis data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu0-test-shu-micros-to-millis-3
;;
(ert-deftest shu0-test-shu-micros-to-millis-3 ()
  (let ((data (+ (* 11 1000 1000) 400))
        (expected (* 11 1000))
        (actual))
    (setq actual (shu-micros-to-millis data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu0-test-shu-micros-to-millis-4
;;
(ert-deftest shu0-test-shu-micros-to-millis-4 ()
  (let ((data (+ (* 11 1000 1000) 499))
        (expected (* 11 1000))
        (actual))
    (setq actual (shu-micros-to-millis data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu0-test-shu-micros-to-millis-5
;;
(ert-deftest shu0-test-shu-micros-to-millis-5 ()
  (let ((data (+ (* 11 1000 1000) 500))
        (expected (+ (* 11 1000) 1))
        (actual))
    (setq actual (shu-micros-to-millis data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu0-test-shu-micros-to-millis-6
;;
(ert-deftest shu0-test-shu-micros-to-millis-6 ()
  (let ((data (+ (* 11 1000 1000) 501))
        (expected (+ (* 11 1000) 1))
        (actual))
    (setq actual (shu-micros-to-millis data))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))


;;
;;  shu-test-shu-jday-1
;;
(ert-deftest shu-test-shu-jday-1 ()
  (let ((date (list 2019 1 22))
        (new-date)
        (j))
    (setq j (shu-jday date))
    (setq new-date (shu-jdate j))
    (should new-date)
    (should (listp date))
    (should (equal date new-date))
    ))




;;; shu-date.t.el ends here
