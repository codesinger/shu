;;; shu-misc.t.el --- Shu project unit tests for code in shu-misc.el
;;
;; Copyright (C) 2018 Stewart L. Palmer
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
;;

;;
;;  shu-misc.t.el
;;
;;  unit tests for code in shu-misc.el
;;


(require 'ert)
(require 'shu-misc)


;;
;;  shu-test-shu-git-number-commits
;;
(ert-deftest shu-test-shu-git-number-commits ()
  (let ((data
         (concat
          "commit 545267aaca37b309196cd6aeacf4a0d0c17af17c\n"
          "Some text here\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "This is a short commit\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591\n\n"
          "The following does not start at bol:\n"
          " commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "Another commit:\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"))
        (expected
         (concat
          "     0. commit 545267aaca37b309196cd6aeacf4a0d0c17af17c\n"
          "Some text here\n"
          "     1. commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "This is a short commit\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591\n\n"
          "The following does not start at bol:\n"
          " commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "Another commit:\n"
          "     2. commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-git-number-commits))
      (should (= 3 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))



;;
;;  shu-test-shu-split-range-string-1
;;
(ert-deftest shu-test-shu-split-range-string-1 ()
  (let ((range-string "99+4")
        (actual-start)
        (actual-end)
        (expected-start 99)
        (expected-end 103)
        (range))
    (setq range (shu-split-range-string range-string))
    (should (consp range))
    (setq actual-start (car-safe range))
    (setq actual-end (cdr-safe range))
    (should (numberp actual-start))
    (should (numberp actual-end))
    (should (= expected-start actual-start))
    (should (= expected-end actual-end))
    ))


;;
;;  shu-test-shu-split-range-string-2
;;
(ert-deftest shu-test-shu-split-range-string-2 ()
  (let ((range-string "99-4")
        (actual-start)
        (actual-end)
        (expected-start 99)
        (expected-end 95)
        (range))
    (setq range (shu-split-range-string range-string))
    (should (consp range))
    (setq actual-start (car-safe range))
    (setq actual-end (cdr-safe range))
    (should (numberp actual-start))
    (should (numberp actual-end))
    (should (= expected-start actual-start))
    (should (= expected-end actual-end))
    ))


;;
;;  shu-test-shu-split-range-string-3
;;
(ert-deftest shu-test-shu-split-range-string-3 ()
  (let ((range-string "99.107")
        (actual-start)
        (actual-end)
        (expected-start 99)
        (expected-end 107)
        (range))
    (setq range (shu-split-range-string range-string))
    (should (consp range))
    (setq actual-start (car-safe range))
    (setq actual-end (cdr-safe range))
    (should (numberp actual-start))
    (should (numberp actual-end))
    (should (= expected-start actual-start))
    (should (= expected-end actual-end))
    ))


;;
;;  shu-test-shu-split-range-string-4
;;
(ert-deftest shu-test-shu-split-range-string-4 ()
  (let ((range-string "107")
        (actual-start)
        (actual-end)
        (expected-start 107)
        (expected-end)
        (debug-on-error t)
        (range))
    (setq range (shu-split-range-string range-string))
    (should (consp range))
    (setq actual-start (car-safe range))
    (setq actual-end (cdr-safe range))
    (should (numberp actual-start))
    (should (not actual-end))
    (should (= expected-start actual-start))
    ))


;;
;;  shu-test-shu-split-range-string-5
;;
(ert-deftest shu-test-shu-split-range-string-5 ()
  (let ((range-string "hello")
        (actual-start)
        (actual-end)
        (expected-start)
        (expected-end)
        (debug-on-error t)
        (range))
    (setq range (shu-split-range-string range-string))
    (should (consp range))
    (setq actual-start (car-safe range))
    (setq actual-end (cdr-safe range))
    (should (not actual-start))
    (should (not actual-end))
    ))
