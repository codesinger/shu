;;; shu-misc.t.el --- Shu project unit tests for code in shu-misc.el
;;
;; Copyright (C) 2018 Stewart L. Palmer
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

;;; Code



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




;;
;;  shu-test-shu-fix-times
;;
(ert-deftest shu-test-shu-fix-times ()
  "Doc string."
  (let ((data
         (concat
          "2017-11-21T130344.568\n"
          "2018-07-08T030344.768"))
        (expected
         (concat
          "2017-11-21 13:03:44.568\n"
          "2018-07-08 03:03:44.768"))
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-fix-times)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-find-numbered-commit-1
;;
(ert-deftest shu-test-shu-find-numbered-commit-1 ()
  (let ((data
         (concat
         " 501. commit 6469db55adb3e05d68b88b087fabe892e3874321\n\n"
         " 304. commit 7469db55adb3e05d68b88b087fabe892e3874321\n\n"))
        (commit-number 304)
        (expected "7469db55adb3e05d68b88b087fabe892e3874321")
        (actual))
    (with-temp-buffer
      (insert data)
    (setq actual (shu-find-numbered-commit commit-number))
    (should (stringp actual))
    (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-find-numbered-commit-2
;;
(ert-deftest shu-test-shu-find-numbered-commit-2 ()
  (let ((data
         (concat
         " 501. commit 6469db55adb3e05d68b88b087fabe892e3874321\n\n"
         " 304. commit 7469db55adb3e05d68b88b087fabe892e3874321\n\n"))
        (commit-number 501)
        (expected "6469db55adb3e05d68b88b087fabe892e3874321")
        (actual))
    (with-temp-buffer
      (insert data)
    (setq actual (shu-find-numbered-commit commit-number))
    (should (stringp actual))
    (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-find-numbered-commit-3
;;
(ert-deftest shu-test-shu-find-numbered-commit-3 ()
  (let ((data
         (concat
         " 501. commit 6469db55adb3e05d68b88b087fabe892e3874321\n\n"
         " 304. commit 7469db55adb3e05d68b88b087fabe892e3874321\n\n"))
        (commit-number 329)
        (actual))
    (with-temp-buffer
      (insert data)
    (setq actual (shu-find-numbered-commit commit-number))
    (should (not actual)))
    ))



;;
;;  shu-test-shu-number-lines-1
;;
(ert-deftest shu-test-shu-number-lines-1 ()
  (let (
        (data
         (concat
          "Now is the time for all good men\n"
          "to come to the aid of the party.\n"
          "Now is the time for all good men\n"
          "to come to the aid of the party.\n"
          "Now is the time for all good men\n"
          "to come to the aid of the party.\n"
          "Now is the time for all good men\n"
          "to come to the aid of the party.\n"
          "Now is the time for all good men\n"
          "to come to the aid of the party.\n"
          "Now is the time for all good men\n"
          "to come to the aid of the party.\n"))
        (expected
         (concat
          "  1. Now is the time for all good men\n"
          "  2. to come to the aid of the party.\n"
          "  3. Now is the time for all good men\n"
          "  4. to come to the aid of the party.\n"
          "  5. Now is the time for all good men\n"
          "  6. to come to the aid of the party.\n"
          "  7. Now is the time for all good men\n"
          "  8. to come to the aid of the party.\n"
          "  9. Now is the time for all good men\n"
          " 10. to come to the aid of the party.\n"
          " 11. Now is the time for all good men\n"
          " 12. to come to the aid of the party.\n"))
        (actual)
        )
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-number-lines)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (stringp actual))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-tighten-lisp
;;
(ert-deftest shu-test-shu-tighten-lisp ()
  (let ((data
         (concat
          "(defun jjj ()\n"
          "  \"Doc string.\"\n"
          "  (interactive)\n"
          "  (let (\n"
          "        (bob)\n"
          "        (bar)\n"
          "        (boo)\n"
          "        )\n"
          "    (while bob\n"
          "      (setq bar (cdr bob))\n"
          "      (when bar\n"
          "        (setq bar nil)\n"
          "        (when boo\n"
          "          (setq boo nil)\n"
          "          )\n"
          "        )\n"
          "      (setq bob (cdr bob))\n"
          "      )\n"
          "    ))\n"))
        (expected
         (concat
          "(defun jjj ()\n"
          "  \"Doc string.\"\n"
          "  (interactive)\n"
          "  (let ((bob)\n"
          "        (bar)\n"
          "        (boo))\n"
          "    (while bob\n"
          "      (setq bar (cdr bob))\n"
          "      (when bar\n"
          "        (setq bar nil)\n"
          "        (when boo\n"
          "          (setq boo nil)))\n"
          "      (setq bob (cdr bob)))\n"
          "    ))\n"))
        (actual))
    (with-temp-buffer
      (insert data)
      (shu-tighten-lisp)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-loosen-lisp
;;
(ert-deftest shu-test-shu-loosen-lisp ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (data
         (concat
          "(defun jjj ()\n"
          "  \"Doc string.\"\n"
          "  (interactive)\n"
          "  (let ((bob)\n"
          "        (bar)\n"
          "        (boo))\n"
          "    (while bob\n"
          "      (setq bar (cdr bob))\n"
          "      (when bar\n"
          "        (setq bar nil)\n"
          "        (when boo\n"
          "          (setq boo nil)))\n"
          "      (setq bob (cdr bob)))\n"
          "    ))\n"
          ))
        (expected
         (concat
          "(defun jjj ()\n"
          "  \"Doc string.\"\n"
          "  (interactive)\n"
          "  (let (\n"
          "        (bob)\n"
          "        (bar)\n"
          "        (boo)\n"
          "          )\n"
          "    (while bob\n"
          "      (setq bar (cdr bob))\n"
          "      (when bar\n"
          "        (setq bar nil)\n"
          "        (when boo\n"
          "          (setq boo nil)\n"
          "          )\n"
          "        )\n"
          "      (setq bob (cdr bob))\n"
          "      )\n"
          "    ))\n"
          ))
        (actual))
    (with-temp-buffer
      (insert data)
      (shu-loosen-lisp)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (princ (format "expected:\n[%s]\n" expected) gb)
      (princ (format "actual:\n[%s]\n" actual) gb)
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))






;;
;;  shu-test-shu-erase-region-1
;;
(ert-deftest shu-test-shu-erase-region-1 ()
  (let ((data
         "12345678901234567890")
        (expected
         "1234   8901234567890")
        (actual)
        (spoint 5)
        (epoint 8))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-erase-region spoint epoint)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-erase-region-2
;;
(ert-deftest shu-test-shu-erase-region-2 ()
  (let ((data
         "12345678901234567890")
        (expected
         "1234   8901234567890")
        (actual)
        (spoint 8)
        (epoint 5))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-erase-region spoint epoint)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-erase-region-3
;;
(ert-deftest shu-test-shu-erase-region-3 ()
  (let ((data
         "1234567890")
        (expected
         "   4567890")
        (actual)
        (spoint 1)
        (epoint 4))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-erase-region spoint epoint)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-erase-region-4
;;
(ert-deftest shu-test-shu-erase-region-4 ()
  (let ((data
         "1234567890")
        (expected
         "12345678 0")
        (actual)
        (spoint 9)
        (epoint 10))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-erase-region spoint epoint)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-make-md-section-name
;;
(ert-deftest shu-test-shu-make-md-section-name ()
  (let ((name "## This is an Overviwew ##")
        (expected "This is an Overviwew")
        (actual))
    (setq actual (shu-make-md-section-name name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-md-index-name-1
;;
(ert-deftest shu-test-shu-make-md-index-name-1 ()
  (let ((name "This is an Overviwew")
        (expected "thisisanoverviwew")
        (actual))
    (setq actual (shu-make-md-index-name name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-md-index-name-2
;;
(ert-deftest shu-test-shu-make-md-index-name-2 ()
  (let ((name "This is an Overviwew (Round 2)")
        (expected "thisisanoverviwewround2")
        (actual))
    (setq actual (shu-make-md-index-name name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-md-index-name-3
;;
(ert-deftest shu-test-shu-make-md-index-name-3 ()
  (let ((name "This is an Overviwew `Mumble`")
        (expected "thisisanoverviwewmumble")
        (actual))
    (setq actual (shu-make-md-index-name name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-make-md-index-name-4
;;
(ert-deftest shu-test-shu-make-md-index-name-4 ()
  (let ((name "This is, an :Overviwew. `Mumble`")
        (expected "thisisanoverviwewmumble")
        (actual))
    (setq actual (shu-make-md-index-name name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;; shu-misc.t.el ends here
