;;; shu-exp-md.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2021 Stewart L. Palmer
;;
;; Package: shu-exp-dpkg
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

(require 'shu-base)




;;
;;  shu-get-markdown-prefix
;;
(defun shu-get-markdown-prefix (section-heading)
  "Returns the pound sign prefix from a markdown section heading,
SECTION-HEADING.  The sring of pound signs must begin at the beginning of the
string.  If a section heading is

   \"### This is a section heading\"

then the string \"###\" is returned.  If the first character in the section
heading is not a pound sign, nil is returned."
  (let ((ss "\\`\\([#]+\\)")
        (prefix))
    (when (string-match ss section-heading)
      (setq prefix (match-string 1 section-heading)))
    prefix
    ))



;;
;;  shu-get-markdown-heading
;;
(defun shu-get-markdown-heading (section-heading)
  "Returns the heading text from a markdown section heading, SECTION-HEADING.
There must be at least one pound sign at the beginning of the string.  If a
section heading is

   \"### This is a section heading\"

then the string \"This is a section heading\" is returned.  If the first
character in the section heading is not a pound sign, nil is returned."
  (let ((sh section-heading)
        (nsh)
        (ss "\\`[#]+")
        (heading))
    (when (string-match ss sh)
      (setq nsh (replace-match "" t t sh))
      (setq heading (shu-trim nsh)))
    heading
    ))



;;
;;  shu-get-markdown-level
;;
(defun shu-get-markdown-level (section-heading)
  "Return the level of a markdown section heading.  The level is defined as
the number of leading pound signs that start at the beginning of the string.
A level 1 heading begins with \"#\".  A level 2 heading begins with \"##\".
If there are no leading pound signs at the beginning of the string, a level of
zero is returned."
  (let ((prefix (shu-get-markdown-prefix section-heading))
        (level 0))
    (when prefix
      (setq level (length prefix)))
    level
    ))



;;
;;  shu-fix-markdown-section
;;
(defun shu-fix-markdown-section (max-depth)
  "On entry, point is positioned after one or more pound signs that define the
beginning of a markdown section heading.  If the number of pound signs is
greater than MAX-DEPTH, ignore the line and return nil.  If the number of
pound signs is less than or equal to MAX-DEPTH, fix the line as described
below and return it.

If the line ends with an expression that looks like

      \"<a name=currentliveupdate></a>\",

remove it.

If the line ends with trailing pound signs, remove them as well.

Then return the repaired line."
  (let* ((ssa "\\s-*<a[ a-zA-Z0-9=-_]+>\\s-*</a>")
         (sot (point))
         (bol (line-beginning-position))
         (eol (line-end-position))
         (line (buffer-substring-no-properties bol eol))
         (level (shu-get-markdown-level line))
         (use-line (and (/= level 0) (<= level max-depth))))
    (if (not use-line)
        (setq line nil)
      (if (re-search-forward ssa eol t)
          (progn
            (replace-match "")
            (setq eol (line-end-position))
            (setq line (buffer-substring-no-properties bol eol)))
        (goto-char eol)
        (while (search-backward "#" sot t)
          (replace-match ""))
        (setq eol (line-end-position))
        (setq line (shu-trim (buffer-substring-no-properties bol eol)))))
    line
    ))




;;
;;  tt
;;
(defun tt ()
  "Doc string."
  (interactive)
  (let (
        (debug-on-error t)
        (heading)
        )
    (setq heading (shu-get-markdown-heading "### This is a heading"))
    (when heading
      (message "[%s]" heading)
      )
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


;;; shu-exp-md.el ends here
