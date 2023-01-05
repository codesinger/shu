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
;;  shu-test-shu-get-containing-function-1
;;
(ert-deftest shu-test-shu-get-containing-function-1 ()
  (let (
        (data
         (concat
          "(defun foo ()\n"
          "  \"Doc string.\"\n"
          "  (let ((ret-val)\n"
          "        (bof)\n"
          "        (eof))\n"
          "    (save-excursion\n"
          "      (if (not (re-search-backward shu-misc-rx-functions nil t))\n"
          "          (progn\n"
          "            (ding)\n"
          "            (message \"%s\" \"Not inside a macro or function\"))\n"
          "        (setq bof (match-beginning 0))\n"
          "        (setq eof (shu-point-at-sexp bof))\n"
          "        (setq ret-val (cons bof eof))))\n"
          "    ret-val\n"
          "    ))\n"))
        (ret-val)
        (bof)
        (eof)
        (expected-bof 1)
        (expected-eof 400)
        (gb (get-buffer-create "**boo**")))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "save-excur" nil t))
      (beginning-of-line)
      (setq ret-val (shu-get-containing-function))
      (should ret-val)
      (should (consp ret-val))
      (setq bof (car ret-val))
      (setq eof (cdr ret-val))
      (should bof)
      (should (numberp bof))
      (should eof)
      (should (numberp eof))
      (should (< bof eof))
      (should (= expected-bof bof))
      (should (= expected-eof eof))
      (princ (format "bof: %d, eof: %d\n" bof eof) gb))
    ))



;;
;;  shu-test-shu-get-containing-function-2
;;
(ert-deftest shu-test-shu-get-containing-function-2 ()
  (let (
        (data
         (concat
          "(dezun foo ()\n"
          "  \"Doc string.\"\n"
          "  (let ((ret-val)\n"
          "        (bof)\n"
          "        (eof))\n"
          "    (save-excursion\n"
          "      (if (not (re-search-backward shu-misc-rx-functions nil t))\n"
          "          (progn\n"
          "            (ding)\n"
          "            (message \"%s\" \"Not inside a macro or function\"))\n"
          "        (setq bof (match-beginning 0))\n"
          "        (setq eof (shu-point-at-sexp bof))\n"
          "        (setq ret-val (cons bof eof))))\n"
          "    ret-val\n"
          "    ))\n"))
        (ret-val)
        (bof)
        (eof)
        (expected-bof 1)
        (expected-eof 400)
        (gb (get-buffer-create "**boo**")))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (should (search-forward "save-excur" nil t))
      (beginning-of-line)
      (setq ret-val (shu-get-containing-function))
      (should (not ret-val)))
    ))



;;
;;  shu-test-shu-tighten-lisp-1
;;
(ert-deftest shu-test-shu-tighten-lisp-1 ()
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



;;
;;  shu-test-shu-get-markdown-prefix-1
;;
(ert-deftest shu-test-shu-get-markdown-prefix-1 ()
  (let ((section-heading "# This is a section")
        (expected-prefix "#")
        (actual-prefix))
    (setq actual-prefix (shu-get-markdown-prefix section-heading))
    (should actual-prefix)
    (should (stringp actual-prefix))
    (should (string= expected-prefix actual-prefix))
    ))



;;
;;  shu-test-shu-get-markdown-prefix-2
;;
(ert-deftest shu-test-shu-get-markdown-prefix-2 ()
  (let ((section-heading "## This is a section")
        (expected-prefix "##")
        (actual-prefix))
    (setq actual-prefix (shu-get-markdown-prefix section-heading))
    (should actual-prefix)
    (should (stringp actual-prefix))
    (should (string= expected-prefix actual-prefix))
    ))



;;
;;  shu-test-shu-get-markdown-prefix-3
;;
(ert-deftest shu-test-shu-get-markdown-prefix-3 ()
  (let ((section-heading "### This is a section")
        (expected-prefix "###")
        (actual-prefix))
    (setq actual-prefix (shu-get-markdown-prefix section-heading))
    (should actual-prefix)
    (should (stringp actual-prefix))
    (should (string= expected-prefix actual-prefix))
    ))



;;
;;  shu-test-shu-get-markdown-prefix-4
;;
(ert-deftest shu-test-shu-get-markdown-prefix-4 ()
  (let ((section-heading "#### This is a section")
        (expected-prefix "####")
        (actual-prefix))
    (setq actual-prefix (shu-get-markdown-prefix section-heading))
    (should actual-prefix)
    (should (stringp actual-prefix))
    (should (string= expected-prefix actual-prefix))
    ))



;;
;;  shu-test-shu-get-markdown-prefix-5
;;
(ert-deftest shu-test-shu-get-markdown-prefix-5 ()
  (let ((section-heading "##### This is a section")
        (expected-prefix "#####")
        (actual-prefix))
    (setq actual-prefix (shu-get-markdown-prefix section-heading))
    (should actual-prefix)
    (should (stringp actual-prefix))
    (should (string= expected-prefix actual-prefix))
    ))



;;
;;  shu-test-shu-get-markdown-prefix-6
;;
(ert-deftest shu-test-shu-get-markdown-prefix-6 ()
  (let ((section-heading "###### This is a section")
        (expected-prefix "######")
        (actual-prefix))
    (setq actual-prefix (shu-get-markdown-prefix section-heading))
    (should actual-prefix)
    (should (stringp actual-prefix))
    (should (string= expected-prefix actual-prefix))
    ))



;;
;;  shu-test-shu-get-markdown-prefix-7
;;
(ert-deftest shu-test-shu-get-markdown-prefix-7 ()
  (let ((section-heading " # This is a section")
        (actual-prefix))
    (setq actual-prefix (shu-get-markdown-prefix section-heading))
    (should (not actual-prefix))
    ))



;;
;;  shu-test-shu-get-markdown-prefix-8
;;
(ert-deftest shu-test-shu-get-markdown-prefix-8 ()
  (let ((section-heading "This is a section")
        (actual-prefix))
    (setq actual-prefix (shu-get-markdown-prefix section-heading))
    (should (not actual-prefix))
    ))



;;
;;  shu-test-shu-get-markdown-heading-1
;;
(ert-deftest shu-test-shu-get-markdown-heading-1 ()
  (let ((section-heading "# This is a heading")
        (expected-heading "This is a heading")
        (actual-heading))
    (setq actual-heading (shu-get-markdown-heading section-heading))
    (should actual-heading)
    (should (stringp actual-heading))
    (should (string= expected-heading actual-heading))
    ))



;;
;;  shu-test-shu-get-markdown-heading-2
;;
(ert-deftest shu-test-shu-get-markdown-heading-2 ()
  (let ((section-heading "## This is a heading")
        (expected-heading "This is a heading")
        (actual-heading))
    (setq actual-heading (shu-get-markdown-heading section-heading))
    (should actual-heading)
    (should (stringp actual-heading))
    (should (string= expected-heading actual-heading))
    ))



;;
;;  shu-test-shu-get-markdown-heading-3
;;
(ert-deftest shu-test-shu-get-markdown-heading-3 ()
  (let ((section-heading "### This is a heading")
        (expected-heading "This is a heading")
        (actual-heading))
    (setq actual-heading (shu-get-markdown-heading section-heading))
    (should actual-heading)
    (should (stringp actual-heading))
    (should (string= expected-heading actual-heading))
    ))



;;
;;  shu-test-shu-get-markdown-heading-4
;;
(ert-deftest shu-test-shu-get-markdown-heading-4 ()
  (let ((section-heading "#### This is a heading")
        (expected-heading "This is a heading")
        (actual-heading))
    (setq actual-heading (shu-get-markdown-heading section-heading))
    (should actual-heading)
    (should (stringp actual-heading))
    (should (string= expected-heading actual-heading))
    ))



;;
;;  shu-test-shu-get-markdown-heading-5
;;
(ert-deftest shu-test-shu-get-markdown-heading-5 ()
  (let ((section-heading "##### This is a heading")
        (expected-heading "This is a heading")
        (actual-heading))
    (setq actual-heading (shu-get-markdown-heading section-heading))
    (should actual-heading)
    (should (stringp actual-heading))
    (should (string= expected-heading actual-heading))
    ))



;;
;;  shu-test-shu-get-markdown-heading-6
;;
(ert-deftest shu-test-shu-get-markdown-heading-6 ()
  (let ((section-heading "######    This is a heading   ")
        (expected-heading "This is a heading")
        (actual-heading))
    (setq actual-heading (shu-get-markdown-heading section-heading))
    (should actual-heading)
    (should (stringp actual-heading))
    (should (string= expected-heading actual-heading))
    ))



;;
;;  shu-test-shu-get-markdown-heading-7
;;
(ert-deftest shu-test-shu-get-markdown-heading-7 ()
  (let ((section-heading " # This is a heading")
        (actual-heading))
    (setq actual-heading (shu-get-markdown-heading section-heading))
    (should (not actual-heading))
    ))



;;
;;  shu-test-shu-get-markdown-heading-8
;;
(ert-deftest shu-test-shu-get-markdown-heading-8 ()
  (let ((section-heading " # This is a heading")
        (actual-heading))
    (setq actual-heading (shu-get-markdown-heading section-heading))
    (should (not actual-heading))
    ))



;;
;;  shu-test-shu-get-markdown-level-1
;;
(ert-deftest shu-test-shu-get-markdown-level-1 ()
  (let ((section-name "# This is a heading")
        (expected-level 1)
        (actual-level 0))
    (setq actual-level (shu-get-markdown-level section-name))
    (should actual-level)
    (should (numberp actual-level))
    (should (= expected-level actual-level))
    ))



;;
;;  shu-test-shu-get-markdown-level-2
;;
(ert-deftest shu-test-shu-get-markdown-level-2 ()
  (let ((section-name "## This is a heading")
        (expected-level 2)
        (actual-level 0))
    (setq actual-level (shu-get-markdown-level section-name))
    (should actual-level)
    (should (numberp actual-level))
    (should (= expected-level actual-level))
    ))



;;
;;  shu-test-shu-get-markdown-level-3
;;
(ert-deftest shu-test-shu-get-markdown-level-3 ()
  (let ((section-name "### This is a heading")
        (expected-level 3)
        (actual-level 0))
    (setq actual-level (shu-get-markdown-level section-name))
    (should actual-level)
    (should (numberp actual-level))
    (should (= expected-level actual-level))
    ))



;;
;;  shu-test-shu-get-markdown-level-4
;;
(ert-deftest shu-test-shu-get-markdown-level-4 ()
  (let ((section-name "#### This is a heading")
        (expected-level 4)
        (actual-level 0))
    (setq actual-level (shu-get-markdown-level section-name))
    (should actual-level)
    (should (numberp actual-level))
    (should (= expected-level actual-level))
    ))



;;
;;  shu-test-shu-get-markdown-level-5
;;
(ert-deftest shu-test-shu-get-markdown-level-5 ()
  (let ((section-name "##### This is a heading")
        (expected-level 5)
        (actual-level 0))
    (setq actual-level (shu-get-markdown-level section-name))
    (should actual-level)
    (should (numberp actual-level))
    (should (= expected-level actual-level))
    ))



;;
;;  shu-test-shu-get-markdown-level-6
;;
(ert-deftest shu-test-shu-get-markdown-level-6 ()
  (let ((section-name "###### This is a heading")
        (expected-level 6)
        (actual-level 0))
    (setq actual-level (shu-get-markdown-level section-name))
    (should actual-level)
    (should (numberp actual-level))
    (should (= expected-level actual-level))
    ))



;;
;;  shu-test-shu-get-markdown-level-7
;;
(ert-deftest shu-test-shu-get-markdown-level-7 ()
  (let ((section-name "This is a line of text")
        (expected-level 0)
        (actual-level 0))
    (setq actual-level (shu-get-markdown-level section-name))
    (should actual-level)
    (should (numberp actual-level))
    (should (= expected-level actual-level))
    ))



;;
;;  shu-test-shu-get-markdown-level-8
;;
(ert-deftest shu-test-shu-get-markdown-level-8 ()
  (let ((section-name "#################### This is a strange heading ")
        (expected-level 20)
        (actual-level 0))
    (setq actual-level (shu-get-markdown-level section-name))
    (should actual-level)
    (should (numberp actual-level))
    (should (= expected-level actual-level))
    ))



;;
;;  shu-test-shu-fix-markdown-section-1
;;
(ert-deftest shu-test-shu-fix-markdown-section-1 ()
  (let* ((heading "## This is a heading")
         (section-heading (concat heading "   "))
         (max-depth 2)
         (actual-line))
    (with-temp-buffer
      (insert section-heading)
      (goto-char (point-min))
      (search-forward "##")
      (setq actual-line (shu-fix-markdown-section max-depth)))
    (should actual-line)
    (should (stringp actual-line))
    (should (string= heading actual-line))
    ))



;;
;;  shu-test-shu-fix-markdown-section-2
;;
(ert-deftest shu-test-shu-fix-markdown-section-2 ()
  (let* ((heading "# This is a heading")
         (section-heading (concat heading "   "))
         (max-depth 2)
         (actual-line))
    (with-temp-buffer
      (insert section-heading)
      (goto-char (point-min))
      (search-forward "#")
      (setq actual-line (shu-fix-markdown-section max-depth)))
    (should actual-line)
    (should (stringp actual-line))
    (should (string= heading actual-line))
    ))



;;
;;  shu-test-shu-fix-markdown-section-3
;;
(ert-deftest shu-test-shu-fix-markdown-section-3 ()
  (let* ((heading "## This is a heading")
         (section-heading (concat heading " ##"))
         (max-depth 2)
         (actual-line))
    (with-temp-buffer
      (insert section-heading)
      (goto-char (point-min))
      (search-forward "##")
      (setq actual-line (shu-fix-markdown-section max-depth)))
    (should actual-line)
    (should (stringp actual-line))
    (should (string= heading actual-line))
    ))



;;
;;  shu-test-shu-fix-markdown-section-4
;;
(ert-deftest shu-test-shu-fix-markdown-section-4 ()
  (let* (
         (heading "### This is a heading")
         (section-heading (concat heading " <a name=currentliveupdate></a>"))
         (max-depth 2)
         (actual-line)
         )
    (with-temp-buffer
      (insert section-heading)
      (goto-char (point-min))
      (search-forward "###")
      (setq actual-line (shu-fix-markdown-section max-depth))
      )
    (should (not actual-line))
    ))



;;
;;  shu-test-shu-fix-markdown-section-5
;;
(ert-deftest shu-test-shu-fix-markdown-section-5 ()
  (let* (
         (heading "## This is a heading")
         (section-heading (concat heading " <a name=currentliveupdate></a>"))
         (max-depth 2)
         (actual-line)
         )
    (with-temp-buffer
      (insert section-heading)
      (goto-char (point-min))
      (search-forward "##")
      (setq actual-line (shu-fix-markdown-section max-depth))
      )
    (should actual-line)
    (should (stringp actual-line))
    (should (string= heading actual-line))
    ))




;;
;;  shu-test-shu-fix-markdown-section-6
;;
(ert-deftest shu-test-shu-fix-markdown-section-6 ()
  (let* (
         (heading "## This is a heading")
         (section-heading (concat heading " <a name=aspecificexample-abcd></a> <a name=aspecificexampl></a>"))
         (max-depth 2)
         (actual-line)
         )
    (with-temp-buffer
      (insert section-heading)
      (goto-char (point-min))
      (search-forward "##")
      (setq actual-line (shu-fix-markdown-section max-depth))
      )
    (should actual-line)
    (should (stringp actual-line))
    (should (string= heading actual-line))
    ))




;;
;;  shu-test-shu-tocify-markdown-headings
;;
(ert-deftest shu-test-shu-tocify-markdown-headings ()
  (let ((entries
         (list
          (cons "# This is a heading"   "thisisah")
          (cons "## This is a level 2"  "somelink")
          (cons "## Another level 2"    "anotherlevel")
          (cons "### A level three"     "alevelth")
          (cons "#### A level four"     "alevelfour")
          (cons "## Back to level 2"    "backtolevel")
          (cons "# Back to level 1"    "backtolevelhwty")))
        (data
         (concat
          "\n"
          "# This is a heading\n"
          "blither\n"
          "## This is a level 2\n"
          "blither\n"
          "## Another level 2\n"
          "blither\n"
          "### A level three\n"
          "blither\n"
          "#### A level four\n"
          "blither\n"
          "## Back to level 2\n"
          "blither\n"
          "# Back to level 1\n"
          "blither\n"))
        (actual)
        (expected
         (concat
          "\n"
          "# This is a heading <a name=thisisah></a>\n"
          "blither\n"
          "## This is a level 2 <a name=somelink></a>\n"
          "blither\n"
          "## Another level 2 <a name=anotherlevel></a>\n"
          "blither\n"
          "### A level three <a name=alevelth></a>\n"
          "blither\n"
          "#### A level four <a name=alevelfour></a>\n"
          "blither\n"
          "## Back to level 2 <a name=backtolevel></a>\n"
          "blither\n"
          "# Back to level 1 <a name=backtolevelhwty></a>\n"
          "blither\n")))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (shu-tocify-markdown-headings entries)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-insert-markdown-toc
;;
(ert-deftest shu-test-shu-insert-markdown-toc ()
  (let ((entries
         (list
          (cons "# This is a heading"   "thisisah")
          (cons "## This is a level 2"  "somelink")
          (cons "## Another level 2"    "anotherlevel")
          (cons "### A level three"     "alevelth")
          (cons "#### A level four"     "alevelfour")
          (cons "## Back to level 2"    "backtolevel")
          (cons "# Back to level 1"    "backtolevelhwty")))
        (actual)
        (expected
         (concat
          " - [This is a heading](#thisisah)\n"
          "     - [This is a level 2](#somelink)\n"
          "     - [Another level 2](#anotherlevel)\n"
          "         - [A level three](#alevelth)\n"
          "             - [A level four](#alevelfour)\n"
          "     - [Back to level 2](#backtolevel)\n"
          " - [Back to level 1](#backtolevelhwty)\n")))
    (with-temp-buffer
      (shu-insert-markdown-toc entries)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-get-md-boundaries-1
;;
(ert-deftest shu-test-shu-get-md-boundaries-1 ()
  (let ((gb (get-buffer-create "**too**"))
        (data
         (concat
          "This is some line of stuff\n"
          "This is more stuff\n"
          "And yet more stuff\n"
          "And so on ...\n"
          ))
        (literals)
        (place)
        (open)
        (close))
    (with-temp-buffer
      (insert data)
      (setq literals (shu-get-md-boundaries))
      (should (not literals)))
    ))


;;
;;  shu-test-shu-get-md-boundaries-2
;;
(ert-deftest shu-test-shu-get-md-boundaries-2 ()
  (let ((gb (get-buffer-create "**too**"))
        (data
         (concat
          "This is some line of stuff\n"
          "```\n"
          "     Some sample code\n"
          "```\n"
          ))
        (literals)
        (place)
        (open)
        (close))
    (with-temp-buffer
      (insert data)
      (setq literals (shu-get-md-boundaries))
      (should literals)
      (should (listp literals))
      (should (= 1 (length literals)))
      (setq place (car literals))
      (should place)
      (should (consp place))
      (setq open (car place))
      (setq close (cdr place))
      (should (= 30 open))
      (should (= 54 close)))
    ))


;;
;;  shu-test-shu-get-md-boundaries-3
;;
(ert-deftest shu-test-shu-get-md-boundaries-3 ()
  (let ((gb (get-buffer-create "**too**"))
        (data
         (concat
          "This is some line of stuff\n"
          "```\n"
          "     Some sample code\n"
          "```\n"
          "This is some line of stuff\n"
          "```\n"
          "     Some sample code\n"
          "```\n"
          ))
        (literals)
        (place)
        (open)
        (close))
    (with-temp-buffer
      (insert data)
      (setq literals (shu-get-md-boundaries))
      (princ literals gb)
      (should literals)
      (should (listp literals))
      (should (= 2 (length literals)))
      (setq place (car literals))
      (should place)
      (should (consp place))
      (setq open (car place))
      (setq close (cdr place))
      (should (= 30 open))
      (should (= 54 close))
      (setq literals (cdr literals))
      (setq place (car literals))
      (should place)
      (should (consp place))
      (setq open (car place))
      (setq close (cdr place))
      (should (= 87 open))
      (should (= 111 close)))
    ))


;;
;;  shu-test-shu-get-md-boundaries-4
;;
(ert-deftest shu-test-shu-get-md-boundaries-24()
  (let ((gb (get-buffer-create "**too**"))
        (data
         (concat
          "This is some line of stuff\n"
          "```\n"
          "     Some sample code\n"
          "```\n"
          "This is some line of stuff\n"
          "```\n"
          "Yet more stuff\n"
          ))
        (literals)
        (place)
        (open)
        (close))
    (with-temp-buffer
      (insert data)
      (setq literals (shu-get-md-boundaries))
      (should literals)
      (should (listp literals))
      (should (= 1 (length literals)))
      (setq place (car literals))
      (should place)
      (should (consp place))
      (setq open (car place))
      (setq close (cdr place))
      (should (= 30 open))
      (should (= 54 close)))
    ))



;;
;;  shu-test-shu-md-in-literal-1
;;
(ert-deftest shu-test-shu-md-in-literal-1 ()
  (let ((literals
         (list
          (cons 10 20)
          (cons 30 40)
          (cons 50 60)))
        (inside))
    (setq inside (shu-md-in-literal literals 15))
    (should inside)
    ))



;;
;;  shu-test-shu-md-in-literal-2
;;
(ert-deftest shu-test-shu-md-in-literal-2 ()
  (let ((literals
         (list
          (cons 10 20)
          (cons 30 40)
          (cons 50 60)))
        (inside))
    (setq inside (shu-md-in-literal literals 55))
    (should inside)
    ))



;;
;;  shu-test-shu-md-in-literal-3
;;
(ert-deftest shu-test-shu-md-in-literal-3 ()
  (let ((literals
         (list
          (cons 10 20)
          (cons 30 40)
          (cons 50 60)))
        (inside))
    (setq inside (shu-md-in-literal literals 45))
    (should (not inside))
    ))



;;
;;  shu-test-shu-md-in-literal-4
;;
(ert-deftest shu-test-shu-md-in-literal-4 ()
  (let ((literals
         (list
          (cons 10 20)
          (cons 30 40)
          (cons 50 60)))
        (inside))
    (setq inside (shu-md-in-literal literals 4))
    (should (not inside))
    ))



;;
;;  shu-test-shu-md-in-literal-5
;;
(ert-deftest shu-test-shu-md-in-literal-5 ()
  (let ((literals
         (list
          (cons 10 20)
          (cons 30 40)
          (cons 50 60)))
        (inside))
    (setq inside (shu-md-in-literal literals 102))
    (should (not inside))
    ))



;;
;;  shu-test-shu-misc-split-string-1
;;
(ert-deftest shu-test-shu-misc-split-string-1 ()
  (let ((data "Now is the time for all good men to come to the aid of the party within these holy portals revenge remains unknown and to all erring mortals, their way by love is shown to all mankind.")
        (lines)
        (line)
        (fixed-width t)
        (line-limit 26)
        (expected
         (list
          "Now is the time for all go"
          "od men to come to the aid "
          "of the party within these "
          "holy portals revenge remai"
          "ns unknown and to all erri"
          "ng mortals, their way by l"
          "ove is shown to all mankin"
          "d."))
        (expected-line)
        (actual-line))
    (setq lines (shu-misc-split-string data line-limit fixed-width))
    (should lines)
    (should (listp lines))
    (should (= (length expected) (length lines)))
    (while (and lines expected)
      (setq expected-line (car expected))
      (setq actual-line (car lines))
      (should actual-line)
      (should (stringp actual-line))
      (should (string= expected-line actual-line))
      (setq lines (cdr lines))
      (setq expected (cdr expected)))
    ))




;;
;;  shu-test-shu-misc-split-string-2
;;
(ert-deftest shu-test-shu-misc-split-string-2 ()
  (let ((data "Now is the time for all good men to come to the aid of the party within these holy portals revenge remains unknown and to all erring mortals, their way by love is shown to all mankind.")
        (lines)
        (line)
        (fixed-width nil)
        (line-limit 26)
        (expected
         (list
          "Now is the time for all "
          "good men to come to the "
          "aid of the party within "
          "these holy portals "
          "revenge remains unknown "
          "and to all erring "
          "mortals, their way by "
          "love is shown to all "
          "mankind."))
        (expected-line)
        (actual-line))
    (setq lines (shu-misc-split-string data line-limit fixed-width))
    (should lines)
    (should (listp lines))
    (should (= (length expected) (length lines)))
    (while (and lines expected)
      (setq expected-line (car expected))
      (setq actual-line (car lines))
      (should actual-line)
      (should (stringp actual-line))
      (should (string= expected-line actual-line))
      (setq lines (cdr lines))
      (setq expected (cdr expected)))
    ))



;;
;;  shu-test-shu-misc-split-string-3
;;
(ert-deftest shu-test-shu-misc-split-string-3 ()
  (let ((data
         (concat
          "Whan that Aprille with his shoures soote "
          "The droghte of Marche hath perced to the roote, "
          "And bathed every veyne in swich licour, "
          "Of which vertu engendred is the flour; "
          "Whan Zephirus eek with his swete breeth "
          "Inspired hath in every holt and heeth "
          "The tendre croppes, and the yonge sonne "
          "Hath in the Ram his halfe cours y-ronne, "
          "And smale fowles maken melodye, "
          "That slepen al the night with open ye, "
          "(So priketh hem nature in hir corages: "
          "Than longen folk to goon on pilgrimages,"))
        (lines)
        (line)
        (fixed-width nil)
        (line-limit 26)
        (expected
         (list
          "Whan that Aprille with "
          "his shoures soote The "
          "droghte of Marche hath "
          "perced to the roote, And "
          "bathed every veyne in "
          "swich licour, Of which "
          "vertu engendred is the "
          "flour; Whan Zephirus eek "
          "with his swete breeth "
          "Inspired hath in every "
          "holt and heeth The tendre "
          "croppes, and the yonge "
          "sonne Hath in the Ram his "
          "halfe cours y-ronne, And "
          "smale fowles maken "
          "melodye, That slepen al "
          "the night with open ye, "
          "(So priketh hem nature in "
          "hir corages: Than longen "
          "folk to goon on "
          "pilgrimages,"))
        (expected-line)
        (actual-line))
    (setq lines (shu-misc-split-string data line-limit fixed-width))
    (should lines)
    (should (listp lines))
    (should (= (length expected) (length lines)))
    (while (and lines expected)
      (setq expected-line (car expected))
      (setq actual-line (car lines))
      (should actual-line)
      (should (stringp actual-line))
      (should (string= expected-line actual-line))
      (setq lines (cdr lines))
      (setq expected (cdr expected)))
    ))



;;
;;  shu-test-shu-misc-split-string-4
;;
(ert-deftest shu-test-shu-misc-split-string-4 ()
  (let ((data
         (concat
          "Whan that Aprille with his shoures soote "
          "The droghte of Marche hath perced to the roote, "
          "And bathed every veyne in swich licour, "
          "Of which vertu engendred is the flour; "
          "Whan Zephirus eek with his swete breeth "
          "Inspired hath in every holt and heeth "
          "The tendre croppes, and the yonge sonne "
          "Hath in the Ram his halfe cours y-ronne, "
          "And smale fowles maken melodye, "
          "That slepen al the night with open ye, "
          "(So priketh hem nature in hir corages: "
          "Than longen folk to goon on pilgrimages,"
          ))
        (lines)
        (line)
        (fixed-width nil)
        (line-limit 13)
        (expected
         (list
          "Whan that "
          "Aprille with "
          "his shoures "
          "soote The "
          "droghte of "
          "Marche hath "
          "perced to "
          "the roote, "
          "And bathed "
          "every veyne "
          "in swich "
          "licour, Of "
          "which vertu "
          "engendred is "
          "the flour; "
          "Whan "
          "Zephirus eek "
          "with his "
          "swete breeth "
          "Inspired "
          "hath in "
          "every holt "
          "and heeth "
          "The tendre "
          "croppes, and "
          "the yonge "
          "sonne Hath "
          "in the Ram "
          "his halfe "
          "cours "
          "y-ronne, And "
          "smale fowles "
          "maken "
          "melodye, "
          "That slepen "
          "al the night "
          "with open "
          "ye, (So "
          "priketh hem "
          "nature in "
          "hir corages: "
          "Than longen "
          "folk to goon "
          "on "
          "pilgrimages,"))
        (expected-line)
        (actual-line))
    (setq lines (shu-misc-split-string data line-limit fixed-width))
    (should lines)
    (should (listp lines))
    (should (= (length expected) (length lines)))
    (while (and lines expected)
      (setq expected-line (car expected))
      (setq actual-line (car lines))
      (should actual-line)
      (should (stringp actual-line))
      (should (string= expected-line actual-line))
      (setq lines (cdr lines))
      (setq expected (cdr expected)))
    ))




;;
;;  shu-test-shu-misc-get-phrase-1
;;
(ert-deftest shu-test-shu-misc-get-phrase-1 ()
  (let ((data "1234567890abcdefghijkl")
        ;;;    1234567890
        (expected "123456789")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-2
;;
(ert-deftest shu-test-shu-misc-get-phrase-2 ()
  (let ((data "1234567890abc  defghijkl")
        ;;;    1234567890
        (expected "123456789")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-3
;;
(ert-deftest shu-test-shu-misc-get-phrase-3()
  (let ((data "nownowist he time to")
        ;;;    1234567890
        (expected "nownowist")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-4
;;
(ert-deftest shu-test-shu-misc-get-phrase-4()
  (let ((data "nownowis the time to")
        ;;;    1234567890
        (expected "nownowis ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-5
;;
(ert-deftest shu-test-shu-misc-get-phrase-5()
  (let ((data "Now is the time for all")
        ;;;    1234567890
        (expected "Now is ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-6
;;
(ert-deftest shu-test-shu-misc-get-phrase-6()
  (let ((data "Now is the time for all")
        ;;;    1234567890123
        (expected "Now is the ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 13))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-7
;;
(ert-deftest shu-test-shu-misc-get-phrase-7()
  (let ((data "Now is the    time for all")
        ;;;    123456789012345678
        (expected "Now is the    ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 16))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-8
;;
(ert-deftest shu-test-shu-misc-get-phrase-8()
  (let ((data "Now is the     time for all")
        ;;;    123456789012345678
        (expected "Now is the     ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 16))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-9
;;
(ert-deftest shu-test-shu-misc-get-phrase-9()
  (let ((data "Now is the     time    for all")
        ;;;    1234567890123456788012345
        (expected "Now is the     time   ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 22))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-10
;;
(ert-deftest shu-test-shu-misc-get-phrase-10()
  (let ((data "")
        ;;;    1234567890123456788012345
        (expected "")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 22))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-11
;;
(ert-deftest shu-test-shu-misc-get-phrase-11()
  (let ((data " ")
        ;;;    1234567890123456788012345
        (expected " ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 22))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-12
;;
(ert-deftest shu-test-shu-misc-get-phrase-12()
  (let ((data "love is shown to all mankind.")
        ;;;    1234567890123456788012345
        (expected "love is shown to all ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 26))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-13
;;
(ert-deftest shu-test-shu-misc-get-phrase-13()
  (let ((data "     ")
        ;;;    1234567890123456788012345
        (expected "     ")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 22))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-phrase-14
;;
(ert-deftest shu-test-shu-misc-get-phrase-14()
  (let ((data "and something else.")
        ;;;    1234567890123456788012345
        (expected "and something else.")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (shu-misc-get-phrase 47))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-get-chunk-1
;;
(ert-deftest shu-test-shu-misc-get-chunk-1 ()
  (let ((data "1234567890abcdefghijklmno")
        (part)
        (expected-part "12345678")
        (expected-rest "90abcdefghijklmno")
        (actual-rest))
    (with-temp-buffer
      (insert data)
      (setq part (shu-misc-get-chunk 8))
      (should part)
      (should (stringp part))
      (should (string= expected-part part))
      (setq actual-rest (buffer-substring-no-properties (point-min) (point-max)))
      (should actual-rest)
      (should (stringp actual-rest))
      (should (string= expected-rest actual-rest)))
    ))



;;
;;  shu-test-shu-misc-get-chunk-2
;;
(ert-deftest shu-test-shu-misc-get-chunk-2 ()
  (let ((data "12345678")
        (part)
        (expected-part "12345678")
        (expected-rest "")
        (actual-rest))
    (with-temp-buffer
      (insert data)
      (setq part (shu-misc-get-chunk 8))
      (should part)
      (should (stringp part))
      (should (string= expected-part part))
      (setq actual-rest (buffer-substring-no-properties (point-min) (point-max)))
      (should actual-rest)
      (should (stringp actual-rest))
      (should (string= expected-rest actual-rest)))
    ))



;;
;;  shu-test-shu-misc-get-chunk-3
;;
(ert-deftest shu-test-shu-misc-get-chunk-3 ()
  (let ((data "12345678")
        (part)
        (expected-part "12345678")
        (expected-rest "")
        (actual-rest))
    (with-temp-buffer
      (insert data)
      (setq part (shu-misc-get-chunk 9))
      (should part)
      (should (stringp part))
      (should (string= expected-part part))
      (setq actual-rest (buffer-substring-no-properties (point-min) (point-max)))
      (should actual-rest)
      (should (stringp actual-rest))
      (should (string= expected-rest actual-rest)))
    ))



;;
;;  shu-test-shu-misc-get-chunk-4
;;
(ert-deftest shu-test-shu-misc-get-chunk-4 ()
  (let ((data "12345678")
        (part)
        (expected-part "12345678")
        (expected-rest "")
        (actual-rest))
    (with-temp-buffer
      (insert data)
      (setq part (shu-misc-get-chunk 10))
      (should part)
      (should (stringp part))
      (should (string= expected-part part))
      (setq actual-rest (buffer-substring-no-properties (point-min) (point-max)))
      (should actual-rest)
      (should (stringp actual-rest))
      (should (string= expected-rest actual-rest)))
    ))



;;
;;  shu-test-shu-misc-get-chunk-5
;;
(ert-deftest shu-test-shu-misc-get-chunk-5 ()
  (let ((gb (get-buffer-create "**foo**"))
        (data "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t")
        (part)
        (expected-part "\\t\\t\\t\\t\\t")
        (expected-rest "")
        (actual-rest)
        (escape t))
    (with-temp-buffer
      (insert data)
      (setq part (shu-misc-get-chunk 10 escape))
      (should part)
      (should (stringp part))
      (princ (concat "\npart:\n [" part "]") gb)
      (should (string= expected-part part)))
    ))




;;
;;  shu-test-shu-misc-get-chunk-6
;;
(ert-deftest shu-test-shu-misc-get-chunk-6 ()
  (let ((gb (get-buffer-create "**foo**"))
        (data "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t")
        (part)
        (expected-part "\\t\\t\\t\\t")
        (expected-rest "")
        (actual-rest)
        (escape t))
    (with-temp-buffer
      (insert data)
      (setq part (shu-misc-get-chunk 9 escape))
      (should part)
      (should (stringp part))
      (princ (concat "\npart2:\n [" part "]") gb)
      (should (string= expected-part part)))
    ))



;;
;;  shu-test-shu-obfuscate-region-1
;;
(ert-deftest shu-test-shu-obfuscate-region-1 ()
  (let ((data
         "Now is the time for all good men to come to the aid of the Party 10 times.")
        (expected
         "Abc de fgh ijkl mno pqr stuv wxy za bcde fg hij klm no pqr Stuvw 10 xyzab.")
        (actual))
    (with-temp-buffer
      (insert data)
      (shu-obfuscate-region (point-min) (point-max))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-misc-random-ua-string
;;
(ert-deftest shu-test-shu-misc-random-ua-string ()
  (let ((rs (shu-misc-random-ua-string 8)))
    (should rs)
    (should (stringp rs))
    (should (= (length rs) 8))
    ))



;;
;;  shu-test-shu-misc-random-lad-string
;;
(ert-deftest shu-test-shu-misc-random-lad-string ()
  (let ((rs (shu-misc-random-lad-string 15)))
    (should rs)
    (should (stringp rs))
    (should (= (length rs) 15))
    ))



;;
;;  shu-test-shu-misc-make-unique-string-1
;;
(ert-deftest shu-test-shu-misc-make-unique-string-1 ()
  (let ((ht (make-hash-table :test 'equal :size 50))
        (string "HappyBirthday")
        (suffix-length 6)
        (nstring)
        (s1)
        (s2)
        (s3)
        (s4))
    (setq nstring (shu-misc-make-unique-string string suffix-length ht))
    (should nstring)
    (should (stringp nstring))
    (setq s1 nstring)
    ;;;
    (setq nstring (shu-misc-make-unique-string string suffix-length ht))
    (should nstring)
    (should (stringp nstring))
    (setq s2 nstring)
    (should (not (string= s1 s2)))
    ;;;
    (setq nstring (shu-misc-make-unique-string string suffix-length ht))
    (should nstring)
    (should (stringp nstring))
    (setq s3 nstring)
    (should (and (not (string= s1 s2)) (not (string= s1 s3))(not (string= s2 s3)) ))
    ;;;
    (setq nstring (shu-misc-make-unique-string string suffix-length ht))
    (should nstring)
    (should (stringp nstring))
    (setq s4 nstring)
    (should (and (not (string= s1 s2))
                 (not (string= s1 s3))
                 (not (string= s1 s4))
                 (not (string= s2 s3))
                 (not (string= s2 s4))
                 (not (string= s3 s4))
                 ))
    ))



;;
;;  shu-test-shu-get-git-name-1
;;
(ert-deftest shu-test-shu-get-git-name-1 ()
  (let (
        (git-url "https://github.com/codesinger/shu.git")
        (expected-path "codesinger/shu")
        (expected-name "shu")
        (ret-val)
        (actual-path)
        (actual-name)
        )
    (setq ret-val (shu-get-git-name git-url))
    (should ret-val)
    (should (consp ret-val))
    (setq actual-path (car ret-val))
    (should actual-path)
    (should (stringp actual-path))
    (should (string= expected-path actual-path))
    (setq actual-name (cdr ret-val))
    (should actual-name)
    (should (stringp actual-name))
    (should (string= expected-name actual-name))
    ))



;;
;;  shu-test-shu-get-git-name-2
;;
(ert-deftest shu-test-shu-get-git-name-2 ()
  (let (
        (git-url "https://git@github.dev.acme.com:group_name/repo_name")
        (expected-path "group_name/repo_name")
        (expected-name "repo_name")
        (ret-val)
        (actual-path)
        (actual-name)
        )
    (setq ret-val (shu-get-git-name git-url))
    (should ret-val)
    (should (consp ret-val))
    (setq actual-path (car ret-val))
    (should actual-path)
    (should (stringp actual-path))
    (should (string= expected-path actual-path))
    (setq actual-name (cdr ret-val))
    (should actual-name)
    (should (stringp actual-name))
    (should (string= expected-name actual-name))
    ))



;;
;;  shu-test-shu-get-git-name-3
;;
(ert-deftest shu-test-shu-get-git-name-3 ()
  (let (
        (git-url "https://git@github.dev.acme.edu:group_name/repo_name")
        (expected-path "group_name/repo_name")
        (expected-name "repo_name")
        (ret-val)
        (actual-path)
        (actual-name)
        )
    (setq ret-val (shu-get-git-name git-url))
    (should ret-val)
    (should (consp ret-val))
    (setq actual-path (car ret-val))
    (should actual-path)
    (should (stringp actual-path))
    (should (string= expected-path actual-path))
    (setq actual-name (cdr ret-val))
    (should actual-name)
    (should (stringp actual-name))
    (should (string= expected-name actual-name))
    ))



;;
;;  shu-test-shu-get-git-name-4
;;
(ert-deftest shu-test-shu-get-git-name-4 ()
  (let (
        (git-url "https://git@github.dev.acme.net:group_name/repo_name")
        (expected-path "group_name/repo_name")
        (expected-name "repo_name")
        (ret-val)
        (actual-path)
        (actual-name)
        )
    (setq ret-val (shu-get-git-name git-url))
    (should ret-val)
    (should (consp ret-val))
    (setq actual-path (car ret-val))
    (should actual-path)
    (should (stringp actual-path))
    (should (string= expected-path actual-path))
    (setq actual-name (cdr ret-val))
    (should actual-name)
    (should (stringp actual-name))
    (should (string= expected-name actual-name))
    ))




;;
;;  shu-test-shu-trim-git-end-1
;;
(ert-deftest shu-test-shu-trim-git-end-1 ()
  (let ((path "https://github.com/codesinger/shu.git")
        (expected "https://github.com/codesinger/shu")
        (actual))
    (setq actual (shu-trim-git-end path))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-git-end-2
;;
(ert-deftest shu-test-shu-trim-git-end-2 ()
  (let ((path "https://github.com/codesinger/shu")
        (expected "https://github.com/codesinger/shu")
        (actual))
    (setq actual (shu-trim-git-end path))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-trim-git-end-3
;;
(ert-deftest shu-test-shu-trim-git-end-3 ()
  (let ((path "  https://github.com/codesinger/shu.git  ")
        (expected "https://github.com/codesinger/shu")
        (actual))
    (setq actual (shu-trim-git-end path))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-internal-get-repo-1
;;
(ert-deftest shu-test-shu-internal-get-repo-1 ()
  (let (
        (data
         (concat
          "[core]\n"
          "	repositoryformatversion = 0\n"
          "	filemode = true\n"
          "	bare = false\n"
          "	logallrefupdates = true\n"
          "	ignorecase = true\n"
          "	precomposeunicode = true\n"
          "[remote \"origin\"]\n"
          "	url = https://github.com/codesinger/shu.git\n"
          "	fetch = +refs/heads/*:refs/remotes/origin/*\n"
          "[branch \"master\"]\n"
          "	remote = origin\n"
          "	merge = refs/heads/master\n"))
        (expected "https://github.com/codesinger/shu.git")
        (actual)
        )
    (with-temp-buffer
      (insert data)
      (setq actual (shu-internal-get-repo))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-shu-unbrace-1
;;
(ert-deftest shu-test-shu-unbrace-1 ()
  (let ((data
         (concat
          "{\n"
          "    {\"This is one thing\"},\n"
          "    {\"This is another thing\"},\n"
          "    {\"And this is a third thing\"};\n"
          "}\n"
          "\n"
          "{\n"
          "    {\"This is one thing\"},\n"
          "    {\"This is another thing\"},\n"
          "    {\"And this is a third thing\"};\n"
          "}\n"
          "\n"
          ))
        (actual)
        (expected 3))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq actual (shu-unbrace))
      (should actual)
      (should (numberp actual))
      (should (= expected actual)))
    ))




;;
;;  shu-test-shu-sitting-end-1
;;
(ert-deftest shu-test-shu-sitting-end-1 ()
  (let ((data "  something  \n")
        (found)
        (fpos))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "some" nil t))
      (should found)
      (setq fpos (shu-sitting-end shu-cpp-keyword 1))
      (should fpos)
      (should (numberp fpos))
      (should (= fpos 11)))
    ))



;;
;;  shu-test-shu-sitting-end-2
;;
(ert-deftest shu-test-shu-sitting-end-2 ()
  (let ((data "  something  \n")
        (found)
        (fpos))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "some" nil t))
      (should found)
      (setq fpos (shu-sitting-end shu-cpp-keyword -1))
      (should fpos)
      (should (numberp fpos))
      (should (= fpos 3)))
    ))



;;
;;  shu-test-shu-sitting-end-3
;;
(ert-deftest shu-test-shu-sitting-end-3 ()
  (let ((data "something  \n")
        (found)
        (fpos))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "some" nil t))
      (should found)
      (setq fpos (shu-sitting-end shu-cpp-keyword -1))
      (should fpos)
      (should (numberp fpos))
      (should (= fpos 1)))
    ))



;;
;;  shu-test-shu-sitting-end-4
;;
(ert-deftest shu-test-shu-sitting-end-4 ()
  (let ((data " something")
        (found)
        (fpos))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "some" nil t))
      (should found)
      (setq fpos (shu-sitting-end shu-cpp-keyword 1))
      (should fpos)
      (should (numberp fpos))
      (should (= fpos 10)))
    ))



;;
;;  shu-test-shu-sitting-end-5
;;
(ert-deftest shu-test-shu-sitting-end-5 ()
  (let ((data "  s ")
        (found)
        (fpos))
    (with-temp-buffer
      (insert data)
      (goto-char 3)
      (setq fpos (shu-sitting-end shu-cpp-keyword 1))
      (should fpos)
      (should (numberp fpos))
      (should (= fpos 3)))
    ))



;;
;;  shu-test-shu-sitting-end-6
;;
(ert-deftest shu-test-shu-sitting-end-6 ()
  (let ((data "s")
        (found)
        (fpos))
    (with-temp-buffer
      (insert data)
      (goto-char 1)
      (setq fpos (shu-sitting-end shu-cpp-keyword 1))
      (should fpos)
      (should (numberp fpos))
      (should (= fpos 1)))
    ))



;;
;;  shu-test-shu-sitting-end-7
;;
(ert-deftest shu-test-shu-sitting-end-7 ()
  (let ((data "s")
        (found)
        (fpos))
    (with-temp-buffer
      (insert data)
      (goto-char 1)
      (setq fpos (shu-sitting-end shu-cpp-keyword -1))
      (should fpos)
      (should (numberp fpos))
      (should (= fpos 1)))
    ))



;;
;;  shu-test-shu-sitting-end-8
;;
(ert-deftest shu-test-shu-sitting-end-8 ()
  (let ((data " somethingBIG ")
        (found)
        (fpos))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "some" nil t))
      (should found)
      (setq fpos (shu-sitting-end shu-cpp-keyword 1))
      (should (not fpos)))
    ))



;;
;;  shu-test-shu-sitting-end-9
;;
(ert-deftest shu-test-shu-sitting-end-9 ()
  (let ((fpos))
    (with-temp-buffer
      (goto-char (point-min))
      (setq fpos (shu-sitting-end shu-cpp-keyword 1))
      (should (not fpos)))
    ))



;;
;;  shu-test-shu-sitting-on-1
;;
(ert-deftest shu-test-shu-sitting-on-1 ()
  (let ((data "  something  \n")
        (found)
        (actual)
        (expected "something"))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "some" nil t))
      (should found)
      (setq actual (shu-sitting-on shu-cpp-keyword))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sitting-on-2
;;
(ert-deftest shu-test-shu-sitting-on-2 ()
  (let ((data "something")
        (found)
        (actual)
        (expected "something"))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "some" nil t))
      (should found)
      (setq actual (shu-sitting-on shu-cpp-keyword))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-sitting-on-3
;;
(ert-deftest shu-test-shu-sitting-on-3 ()
  (let ((data "somethingBIG")
        (found)
        (actual))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq found (search-forward "some" nil t))
      (should found)
      (setq actual (shu-sitting-on shu-cpp-keyword))
      (should (not actual)))
    ))



;;
;;  shu-test-shu-is-common-substring-1
;;
(ert-deftest shu-test-shu-is-common-substring-1 ()
  (let ((data
         (list
          "aaaa_bumble"
          "aaaa_stumble"
          "aaaa_mumble"
          ))
        (substring "aaaa")
        (expected "aaaa")
        (actual))
    (setq actual (shu-is-common-substring substring data))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-is-common-substring-2
;;
(ert-deftest shu-test-shu-is-common-substring-2 ()
  (let ((data
         (list
          "aaaa_bumble"
          "aaab_stumble"
          "aaaa_mumble"))
        (substring "aaaa")
        (expected "aaaa")
        (actual))
    (setq actual (shu-is-common-substring substring data))
    (should (not actual))
    ))



;;
;;  shu-test-shu-is-common-prefix-1
;;
(ert-deftest shu-test-shu-is-common-prefix-1 ()
  (let ((data
         (list
          "abcwqlohfa"
          "abcjfqoeriih"
          "abchqper"))
          (prefix "abc")
          (expected "abc")
          (actual))
    (setq actual (shu-is-common-prefix prefix data))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-is-common-prefix-2
;;
(ert-deftest shu-test-shu-is-common-prefix-2 ()
  (let ((data
         (list
          "wqlohfa"
          "abcjfqoeriih"
          "abchqper"))
          (prefix "abc")
          (actual))
    (setq actual (shu-is-common-prefix prefix data))
    (should (not actual))
    ))




;;
;;  shu-test-shu-longest-common-substring-1
;;
(ert-deftest shu-test-shu-longest-common-substring-1 ()
  (let (
        (strings
         (list
          "abcdemumb"
          "abcdebumb"
          "abcdestumb"))
        (expected "abcde")
        (actual)
        )
    (setq actual (shu-longest-common-substring strings))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-longest-common-substring-2
;;
(ert-deftest shu-test-shu-longest-common-substring-2 ()
  (let (
        (strings
         (list
          "abcd"
          "effghi"
          "jklmn"))
        (expected "abcde")
        (actual)
        )
    (setq actual (shu-longest-common-substring strings))
    (should (not actual))
    ))



;;
;;  shu-test-shu-prepare-for-rename-1
;;
(ert-deftest shu-test-shu-prepare-for-rename-1 ()
  (let ((data
         (concat
          "aaaa_fooble.cpp\n"
          "aaaa_fooble.h\n"
          "aaaa_fooble.t.cpp\n"))
        (expected
         (concat
          "mv aaaa_fooble.cpp    abcdef_fooble.cpp\n"
          "mv aaaa_fooble.h      abcdef_fooble.h\n"
          "mv aaaa_fooble.t.cpp  abcdef_fooble.t.cpp\n"))
        (actual))
    (with-temp-buffer
      (insert data)
      (shu-prepare-for-rename "aaaa" "abcdef")
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-prepare-for-rename-2
;;
(ert-deftest shu-test-shu-prepare-for-rename-2 ()
  (let ((data
         (concat
          "aaaa_fooble.cpp\n"
          "aaaa_fooble.h\n"
          "aaaa_fooble.t.cpp\n"
          "\n"
          "\n"))
        (expected
         (concat
          "mv aaaa_fooble.cpp    abcdef_fooble.cpp\n"
          "mv aaaa_fooble.h      abcdef_fooble.h\n"
          "mv aaaa_fooble.t.cpp  abcdef_fooble.t.cpp\n"))
        (actual))
    (with-temp-buffer
      (insert data)
      (shu-prepare-for-rename "aaaa" "abcdef")
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-prepare-for-rename-3
;;
(ert-deftest shu-test-shu-prepare-for-rename-3 ()
  (let ((data
         (concat
          "aaaa_fooble.h\n"
          "aaaa_fooble.cpp\n"
          "aaaa_fooble.t.cpp\n"))
        (expected
         (concat
          "mv aaaa_fooble.h      abcdef_fooble.h\n"
          "mv aaaa_fooble.cpp    abcdef_fooble.cpp\n"
          "mv aaaa_fooble.t.cpp  abcdef_fooble.t.cpp\n"))
        (actual))
    (with-temp-buffer
      (insert data)
      (shu-prepare-for-rename "aaaa" "abcdef")
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-prepare-for-rename-4
;;
(ert-deftest shu-test-shu-prepare-for-rename-4 ()
  (let ((data
         (concat
          "aaaa_fooble.t.cpp\n"
          "aaaa_fooble.h\n"
          "aaaa_fooble.cpp\n"))
        (expected
         (concat
          "mv aaaa_fooble.t.cpp  abcdef_fooble.t.cpp\n"
          "mv aaaa_fooble.h      abcdef_fooble.h\n"
          "mv aaaa_fooble.cpp    abcdef_fooble.cpp\n"))
        (actual))
    (with-temp-buffer
      (insert data)
      (shu-prepare-for-rename "aaaa" "abcdef")
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-prepare-for-rename-5
;;
(ert-deftest shu-test-shu-prepare-for-rename-5 ()
  (let ((data
         (concat
          "aaaa_fooble.t.cpp\n"
          "aaaa_fooble.h\n"
          "aaaa_fooble.cpp\n"))
        (expected
         (concat
          "mv aaaa_fooble.t.cpp  abc_fooble.t.cpp\n"
          "mv aaaa_fooble.h      abc_fooble.h\n"
          "mv aaaa_fooble.cpp    abc_fooble.cpp\n"))
        (actual))
    (with-temp-buffer
      (insert data)
      (shu-prepare-for-rename "aaaa" "abc")
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-get-name-and-version-1
;;
(ert-deftest shu-test-shu-get-name-and-version-1 ()
  (let ((actual)
        (actual)
        (expected "library=1.2.9"))
    (with-temp-buffer
      (insert "Published version 1.2.9 of library\n")
      (goto-char 8)
      (setq actual (shu-get-name-and-version))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-get-name-and-version-2
;;
(ert-deftest shu-test-shu-get-name-and-version-2 ()
  (let ((actual))
    (with-temp-buffer
      (insert "April is the cruellest month\n")
      (goto-char 8)
      (setq actual (shu-get-name-and-version))
      (should (not actual)))
    ))




;;
;;  shu-test-shu-get-name-and-version-3
;;
(ert-deftest shu-test-shu-get-name-and-version-3 ()
  (let ((actual))
    (with-temp-buffer
      (insert "\n")
      (goto-char (point-min))
      (setq actual (shu-get-name-and-version))
      (should (not actual)))
    ))



;;
;;  shu-test-shu-extract-replacement-strings-1
;;
(ert-deftest shu-test-shu-extract-replacement-strings-1 ()
  (let* ((expected1 "abc")
         (expected2 "defg")
         (dc "/")
         (data (concat dc expected1 dc expected2))
         (rval)
         (actual1)
         (actual2)
         (debug-on-error t))
    (setq rval (shu-extract-replacement-strings data))
    (should rval)
    (should (consp rval))
    (setq actual1 (car rval))
    (setq actual2 (cdr rval))
    (should actual1)
    (should (stringp actual1))
    (should actual2)
    (should (stringp actual2))
    (should (string= expected1 actual1))
    (should (string= expected2 actual2))
    ))



;;
;;  shu-test-shu-extract-replacement-strings-2
;;
(ert-deftest shu-test-shu-extract-replacement-strings-2 ()
  (let* ((expected1 "abc")
         (expected2 "defg")
         (dc "/")
         (data (concat dc expected1 dc expected2 dc))
         (rval)
         (actual1)
         (actual2))
    (setq rval (shu-extract-replacement-strings data))
    (should rval)
    (should (consp rval))
    (setq actual1 (car rval))
    (setq actual2 (cdr rval))
    (should actual1)
    (should (stringp actual1))
    (should actual2)
    (should (stringp actual2))
    (should (string= expected1 actual1))
    (should (string= expected2 actual2))
    ))



;;
;;  shu-test-shu-extract-replacement-strings-3
;;
(ert-deftest shu-test-shu-extract-replacement-strings-3 ()
  (let* ((expected1 "abc")
         (expected2 "defg")
         (dc "$")
         (data (concat dc expected1 dc expected2))
         (rval)
         (actual1)
         (actual2))
    (setq rval (shu-extract-replacement-strings data))
    (should rval)
    (should (consp rval))
    (setq actual1 (car rval))
    (setq actual2 (cdr rval))
    (should actual1)
    (should (stringp actual1))
    (should actual2)
    (should (stringp actual2))
    (should (string= expected1 actual1))
    (should (string= expected2 actual2))
    ))



;;
;;  shu-test-shu-extract-replacement-strings-4
;;
(ert-deftest shu-test-shu-extract-replacement-strings-4 ()
  (let* ((expected1 "abc")
         (expected2 "defg")
         (dc "$")
         (data (concat dc expected1 dc expected2 dc))
         (rval)
         (actual1)
         (actual2))
    (setq rval (shu-extract-replacement-strings data))
    (should rval)
    (should (consp rval))
    (setq actual1 (car rval))
    (setq actual2 (cdr rval))
    (should actual1)
    (should (stringp actual1))
    (should actual2)
    (should (stringp actual2))
    (should (string= expected1 actual1))
    (should (string= expected2 actual2))
    ))



;;
;;  shu-test-shu-extract-replacement-strings-5
;;
(ert-deftest shu-test-shu-extract-replacement-strings-5 ()
  (let* ((dc "$")
         (data "Just the worst time of year for a journey")
         (rval))
    (setq rval (shu-extract-replacement-strings data))
    (should (not rval))
    ))



;;
;;  shu-test-shu-extract-replacement-triple-1
;;
(ert-deftest shu-test-shu-extract-replacement-triple-1 ()
  (let ((data "$some thing$orother$with pepper")
        (expected
         (list
          "some thing"
          "orother"
          "with pepper"))
        (actual))
    (setq actual (shu-extract-replacement-triple data))
    (should actual)
    (should (listp actual))
    (should (= (length expected) (length actual)))
    ))



;;
;;  shu-test-shu-extract-replacement-triple-2
;;
(ert-deftest shu-test-shu-extract-replacement-triple-2 ()
  (let ((data "$some thing$orother$")
        (expected
         (list
          "some thing"
          "orother"
          ""))
        (actual))
    (setq actual (shu-extract-replacement-triple data))
    (should actual)
    (should (listp actual))
    (should (= (length expected) (length actual)))
    ))



;;
;;  shu-test-shu-extract-replacement-triple-3
;;
(ert-deftest shu-test-shu-extract-replacement-triple-3 ()
  (let ((data "$some thing$orother")
        (expected
         (list
          "some thing"
          "orother"
          ""))
        (actual))
    (setq actual (shu-extract-replacement-triple data))
    (should actual)
    (should (listp actual))
    (should (= (length expected) (length actual)))
    ))



;;
;;  shu-test-shu-extract-replacement-triple-4
;;
(ert-deftest shu-test-shu-extract-replacement-triple-4 ()
  (let ((data "$HappyBirthday")
        (actual))
    (setq actual (shu-extract-replacement-triple data))
    (should (not actual))
    ))



;;
;;  shu-test-shu-fix-header-line-1
;;
(ert-deftest shu-test-shu-fix-header-line-1 ()
  (let* ((file-name "something_orother.h")
         (open-line (concat (shu-make-padded-line
                             (concat "// " file-name) (- shu-cpp-comment-end (length shu-cpp-edit-sentinel)))
                            shu-cpp-edit-sentinel))
         (data
          (concat
           open-line "\n"
           "\n"
           "/*!\n"
           " * \file something_orother.h\n"
           " *\n"
           " * \brief Declaration of SomethingOrOther\n"
           " */\n"))
         (end-pos 0)
         (new-end-pos 0)
         (count 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq count (shu-fix-header-line))
      (should count)
      (should (numberp count))
      (should (= count 0))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (= new-end-pos end-pos)))
    ))



;;
;;  shu-test-shu-fix-header-line-2
;;
(ert-deftest shu-test-shu-fix-header-line-2 ()
  (let* ((file-name "something_orother.h")
         (open-line (concat (shu-make-padded-line
                             (concat "// " file-name)
                             (- shu-cpp-comment-end (- (length shu-cpp-edit-sentinel) 3)))
                            shu-cpp-edit-sentinel))
         (data
          (concat
           open-line "\n"
           "\n"
           "/*!\n"
           " * \file something_orother.h\n"
           " *\n"
           " * \brief Declaration of SomethingOrOther\n"
           " */\n"))
         (end-pos 0)
         (new-end-pos 0)
         (count 0)
         (diff 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq count (shu-fix-header-line))
      (should count)
      (should (numberp count))
      (should (= count -3))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> end-pos new-end-pos))
      (setq diff (- end-pos new-end-pos))
      (should (= diff 3)))
    ))



;;
;;  shu-test-shu-fix-header-line-3
;;
(ert-deftest shu-test-shu-fix-header-line-3 ()
  (let* ((file-name "something_orother.h")
         (open-line (concat (shu-make-padded-line
                             (concat "// " file-name)
                             (- shu-cpp-comment-end (+ (length shu-cpp-edit-sentinel) 3)))
                            shu-cpp-edit-sentinel))
         (data
          (concat
           open-line "\n"
           "\n"
           "/*!\n"
           " * \file something_orother.h\n"
           " *\n"
           " * \brief Declaration of SomethingOrOther\n"
           " */\n"))
         (end-pos 0)
         (new-end-pos 0)
         (count 0)
         (diff 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq count (shu-fix-header-line))
      (should count)
      (should (numberp count))
      (should (= count 3))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> new-end-pos end-pos))
      (setq diff (- new-end-pos end-pos))
      (should (= diff 3)))
    ))



;;
;;  shu-test-shu-expand-header-line-1
;;
(ert-deftest shu-test-shu-expand-header-line-1 ()
  (let ((data
         (concat
          "// something_orother.h                                      -*-C++-*-\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (end-pos 0)
        (new-end-pos 0)
        (expand-count 0)
        (diff))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq expand-count (shu-expand-header-line 8))
      (should expand-count)
      (should (numberp expand-count))
      (should (= expand-count 8))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> new-end-pos end-pos))
      (setq diff (- new-end-pos end-pos))
      (should (= diff expand-count)))
    ))



;;
;;  shu-test-shu-expand-header-line-2
;;
(ert-deftest shu-test-shu-expand-header-line-2 ()
  (let ((data
         (concat
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (expand-count 0))
    (with-temp-buffer
      (insert data)
      (setq expand-count (shu-expand-header-line 8))
      (should expand-count)
      (should (numberp expand-count))
      (should (= expand-count 0)))
    ))




;;
;;  shu-test-shu-trim-header-line-1
;;
(ert-deftest shu-test-shu-trim-header-line-1 ()
  (let ((data
         (concat
          "// something_orother.h                                      -*-C++-*-\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (end-pos 0)
        (new-end-pos 0)
        (trim-count 0)
        (diff 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq trim-count (shu-trim-header-line 8))
      (should trim-count)
      (should (numberp trim-count))
      (should (= trim-count 8))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> end-pos new-end-pos))
      (setq diff (- end-pos new-end-pos))
      (should (= diff trim-count)))
    ))



;;
;;  shu-test-shu-trim-header-line-2
;;
(ert-deftest shu-test-shu-trim-header-line-2 ()
  (let ((data
         (concat
          "// something_orother.h                               aaa   -*-C++-*-\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (end-pos 0)
        (new-end-pos 0)
        (trim-count 0)
        (diff 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq trim-count (shu-trim-header-line 8))
      (should trim-count)
      (should (numberp trim-count))
      (should (= trim-count 2))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> end-pos new-end-pos))
      (setq diff (- end-pos new-end-pos))
      (should (= diff trim-count)))
    ))



;;
;;  shu-test-shu-trim-header-line-3
;;
(ert-deftest shu-test-shu-trim-header-line-3 ()
  (let ((data
         (concat
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (trim-count 0))
    (with-temp-buffer
      (insert data)
      (setq trim-count (shu-trim-header-line 8))
      (should trim-count)
      (should (numberp trim-count))
      (should (= trim-count 0)))
    ))




;;
;;  shu-test-shu-replace-namespace-in-file-name-1
;;
(ert-deftest shu-test-shu-replace-namespace-in-file-name-1 ()
  (let* ((old-namespace "oldnamespace")
         (new-namespace "newernamespace")
         (file-name (concat old-namespace "_mumblebar.t.cpp"))
         (expected (concat new-namespace "_mumblebar.t.cpp"))
         (actual))
    (setq actual (shu-replace-namespace-in-file-name file-name old-namespace new-namespace))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-replace-namespace-in-file-name-2
;;
(ert-deftest shu-test-shu-replace-namespace-in-file-name-2 ()
  (let* ((old-namespace "oldnamespace")
         (new-namespace "newernamespace")
         (file-name (concat old-namespace "_/" old-namespace "_/" old-namespace"_mumblebar.t.cpp"))
         (expected (concat old-namespace "_/" old-namespace "_/" new-namespace "_mumblebar.t.cpp"))
         (actual))
    (setq actual (shu-replace-namespace-in-file-name file-name old-namespace new-namespace))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-move-string-1
;;
(ert-deftest shu-test-shu-move-string-1 ()
  (let* ((old-file "input_something.h")
        (new-file "newer_something.h")
        (expected (concat "mv " old-file " " new-file))
        (actual))
    (setq actual (shu-move-string old-file new-file))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-replace-namespace-in-buffer-1
;;
(ert-deftest shu-test-shu-replace-namespace-in-buffer-1 ()
  (let ((data
         (concat
          "// something_orother.h                                      -*-C++-*-\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " * \brief Declaration of SomethingOrOther\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"
          "namespace something;\n"
          "\n"))
        (old-namespace "something")
        (new-namespace "newersomething")
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-replace-namespace-in-buffer old-namespace new-namespace))
      (should count)
      (should (numberp count))
      (should (= count 4)))
    ))



;;
;;  shu-test-shu-replace-namespace-in-buffer-2
;;
(ert-deftest shu-test-shu-replace-namespace-in-buffer-2()
  (let ((data
         (concat
          "// something_orother.h                                      -*-C++-*-\n"
          "#if INCLUDED_SOMETHING_OROTHERH\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"
          "namespace something;\n"
          "\n"))
        (old-namespace "something")
        (new-namespace "newersomething")
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-replace-namespace-in-buffer old-namespace new-namespace))
      (should count)
      (should (numberp count))
      (should (= count 5)))
    ))



;;
;;  shu-test-shu-replace-namespace-in-buffer-3
;;
(ert-deftest shu-test-shu-replace-namespace-in-buffer-3()
  (let ((data
         (concat
          "// something_orother.h                                      -*-C++-*-\n"
          "#if INCLUDED_SOMETHING_OROTHER_H\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"
          "namespace something;\n"
          "\n"))
        (expected
         (concat
          (shu-make-file-header-line "newersomething_orother.h") "\n"
          "#if INCLUDED_NEWERSOMETHING_OROTHER_H\n"
          "\n"
          "/*!\n"
          " * \file newersomething_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"
          "namespace newersomething;\n"
          "\n"))
        (old-namespace "something")
        (new-namespace "newersomething")
        (count 0)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-replace-namespace-in-buffer old-namespace new-namespace))
      (should count)
      (should (numberp count))
      (should (= count 5))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))

;;; shu-misc.t.el ends here
