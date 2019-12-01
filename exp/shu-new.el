;;; shu-new.el --- Shu project code for dealing wth C++ in Emacs
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

;; A collection of experimental functions for dealing with C++ code.
;;
;;

;;; Code:


;;
;;  csp
;;
(defun csp (prefix)
  "Doc string."
  (interactive "*P")
  (let (
        (gb (get-buffer-create "**boo**"))
        (xquote "[^\\]\"") ;; quote not preceded by escape
        (tstart (shu-point-in-string))
        (tend)
        (p)
        (m)
        (cc)
        (pad-count 0)
        (bpad "")
        (pad)
        (npad "")
        (line-limit 10)
        (original)
        (fixed-width prefix)
        (lines)
        (line)
        (nl "")
        )
    ;; String to be removed is (tstart - 1) to tend (includes the quotes
    ;; String to be split and re-inserted is from tstart to (tend - 1 (Not
    ;; including the quotes
    (if (not tstart)
        (progn
          (ding)
          (message "%s" "Not in string")
          )
      (goto-char tstart)
      (setq cc (current-column))
      (when (> cc 0)
        (setq pad-count (1- cc))
        (setq bpad (make-string pad-count ? ))
        )
      (setq pad (concat "\"\n" bpad "\""))
      (when (< pad-count shu-cpp-line-end)
        (setq line-limit (- shu-cpp-line-end pad-count 1))
        (when (< line-limit 10)
          (setq line-limit 10)
          )
        )
      (setq line-limit (1- line-limit))
      (princ (format "shu-cpp-line-end: %d, pad-count: %d, line-limit: %d\n" shu-cpp-line-end pad-count line-limit) gb)
      (setq tend (re-search-forward xquote nil t))
      (if (not tend)
          (message "%s" "No string end")
        (setq m (buffer-substring-no-properties tstart (1- tend)))
        (setq m (concat "[" m "]"))
        (message "%s" m)
        (princ (concat "[" pad "]\n") gb)
        (setq original (buffer-substring-no-properties tstart (1- tend)))
        (princ (concat "[" original "]\n") gb)
        (setq lines (shu-misc-split-string original line-limit fixed-width))
        (goto-char (1- tstart))
        (delete-region (1- tstart) tend)
        (while lines
          (setq line (car lines))
          (insert (concat nl npad "\"" line "\""))
          (setq nl "\n")
          (setq npad bpad)
          (setq lines (cdr lines))
          )

        )
      )

    ))

                           ;;; "This is a string of some sort within these holy portals, revenge remains unknown and to all erring mortals, their way by love is shown and something else."


;;
;;  ggg
;;
(defun ggg ()
  "Doc string."
  (interactive)
  (let (
        (ss (concat shu-all-whitespace-regexp "+"))
        (did)
        (m)
        (p)
        )
    (setq did (re-search-forward ss nil t))
    (if (not did)
        (setq m "No")
      (setq p (- did 6))
      (setq m (buffer-substring-no-properties p did))
      (setq m (concat "[" m "]"))
      )
    (message "%s" m)
        ))


;;
;;  kkk
;;
(defun kkk ()
  "Doc string."
  (interactive)
  (let (
        (ss (concat shu-all-whitespace-regexp "+"))
        (sn (concat shu-not-all-whitespace-regexp "+"))
        (did)
        (m)
        (p)
        )
    (setq did (re-search-forward ss nil t))
    (if (not did)
        (setq m "No")
      (setq did (re-search-backward sn (line-beginning-position) t))
      (if (not did)
          (setq m "No back")
         (setq p (- did 6))
         (setq m (buffer-substring-no-properties  p (1+ did)))
         (setq m (concat "[" m "]"))
         )
        )
    (message "%s" m)
    ))

;;;                                    is the time

;;;nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn



;;
;;  jjj
;;
(defun jjj ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**boo**"))
        (xquote "[^\\]\"") ;; quote not preceded by escape
        (tstart (shu-point-in-string))
        (tend)
        (p)
        (m)
        (cc)
        (pad-count 0)
        (bpad "")
        (pad)
        (line-limit)
        )
    ;; String to be removed is (tstart - 1) to tend (includes the quotes
    ;; String to be split and re-inserted is from tstart to (tend - 1 (Not
    ;; including the quotes
    (if (not tstart)
        (message "%s" "Not in string")
      (goto-char tstart)
      (setq cc (current-column))
      (when (> cc 0)
        (setq pad-count (1- cc))
        (setq bpad (make-string pad-count ? ))
        )
      (setq pad (concat "\"\n" bpad "\""))
      (setq line-limit 10)
      (when (< pad-count shu-cpp-line-end)
        (setq line-limit (- shu-cpp-line-end pad-count))
        (when (< 10 line-limit)
          (setq line-limit 10)
          )
        )
      (setq line-limit (1- line-limit))
      (setq tend (re-search-forward xquote nil t))
      (if (not tend)
          (message "%s" "No string end")
        (setq m (buffer-substring-no-properties tstart (1- tend)))
        (setq m (concat "[" m "]"))
        (message "%s" m)
        (princ (concat "[" pad "]\n") gb)
        (with-temp-buffer
          (insert m)
          (spl-buffer line-limit)
          )
          )
        )

    ))



;;
;;  spl-buffer
;;
(defun spl-buffer (line-limit)
  "Doc string."
  (let (
        (something t)
        (line)
        (lines)
        )
    (while something
      (setq line (get-phrase line-limit))
      (if (string= line "")
          (setq something nil)
        (push line lines)
          )
      )
    (setq lines (nreverse lines))
    lines
    ))



;;
;;  shu-test-spl-buffer-1
;;
(ert-deftest shu-test-spl-buffer-1 ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (data "Now is the time for all good men to come to the aid of the party within these holy portals revenge remains unknown and to all erring mortals, their way by love is shown to all mankind.")
        (lines)
        (line)
        )
    (with-temp-buffer
      (insert data)
      (setq lines (spl-buffer 26))
      )
    (while lines
      (setq line (car lines))
      (princ (concat "[" line "]\n") gb)
      (setq lines (cdr lines))
      )

    ))




;;
;;  shu-test-spl-buffer-2
;;
(ert-deftest shu-test-spl-buffer-2 ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (data "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus faucibus dictum venenatis. Nunc lobortis, nibh sed consequat vulputate, magna neque tincidunt lacus, non rutrum turpis libero quis orci. Curabitur eu pulvinar tellus, ut consectetur orci. In in nibh ornare, malesuada elit sed, tincidunt elit. Nullam eu mollis mi. Nullam congue fermentum nulla quis bibendum. Morbi ac orci viverra justo pharetra mollis vitae a sem. Phasellus suscipit rhoncus urna, eget facilisis justo pellentesque id. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. In orci magna, maximus sit amet magna sit amet, varius ullamcorper dolor. Donec et orci purus. Aliquam non neque ligula. In accumsan, magna quis auctor ultricies, metus lorem aliquam erat, eget aliquam massa elit eu elit. Sed facilisis, quam at facilisis dapibus, libero ante efficitur risus, sit amet posuere nulla metus ac mauris.")
        (lines)
        (line)
        )
    (with-temp-buffer
      (insert data)
      (setq lines (spl-buffer 26))
      )
    (while lines
      (setq line (car lines))
      (princ (concat "[" line "]\n") gb)
      (setq lines (cdr lines))
      )

    ))



;;
;;  get-phrase
;;
(defun get-phrase (line-limit)
  "Remove from the front of the current buffer and return the longest possible
string of whitespace separated things whose length does not exceed line-limit.
If there is at least one whitespace character before LINE-LIMIT, the string will
end with one or more whitespace characters.  i.e., the string will end on a word
boundary if that is possible.

Words will not be split unless there is no whitespace character before
LINE-LIMIT characters have been scanned, in which case a string of exactly
LINE-LIMIT length will be removed and returned.

This function is used to split a string of words into a set of smaller strings
such that words are not split."
  (let ((ss (concat shu-all-whitespace-regexp "+"))
        (sn (concat shu-not-all-whitespace-regexp "+"))
        (something t)
        (tpoint)
        (lpoint)
        (rpoint)
        (epoint)
        (part))
    (goto-char (point-min))
    (while something
      (setq tpoint (re-search-forward ss nil t))
      (if (not tpoint)
          (progn
            (if lpoint
                (setq epoint lpoint)
              (setq epoint line-limit))
            (setq part (get-hunk epoint))
            (setq something nil))
        (setq tpoint (1- tpoint))
        (when (> tpoint line-limit)
          (setq rpoint (re-search-backward sn nil t))
          (if (not rpoint)
              (setq epoint line-limit)
            (setq rpoint (1+ rpoint))
            (if (< rpoint line-limit)
                (setq epoint line-limit)
              (if lpoint
                  (setq epoint lpoint)
                (setq epoint line-limit))))
          (setq part (get-hunk epoint))
          (setq something nil)))
      (setq lpoint tpoint))
    part
    ))



;;
;;  get-hunk
;;
(defun get-hunk (line-limit)
  "Retrn a string that consists of the first LINE-LIMIT characters in the
current buffer.  If LINE-LIMIT is larger than the buffer size, return a
string that is the entire contents of the buffer.  Before returning, delete
from the buffer the returned string."
  (let* ((spoint (point-min))
        (xpoint (+ line-limit spoint))
        (epoint (if (< (point-max) xpoint) (point-max) xpoint))
        (part (buffer-substring-no-properties spoint epoint)))
    (delete-region spoint epoint)
    part
    ))



;;
;;  shu-test-get-phrase-1
;;
(ert-deftest shu-test-get-phrase-1 ()
  (let ((data "1234567890abcdefghijkl")
        ;;;    1234567890
        (expected "123456789")
        (actual))
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-get-phrase-2
;;
(ert-deftest shu-test-get-phrase-2 ()
  (let (
        (data "1234567890abc  defghijkl")
        ;;;    1234567890
        (expected "123456789")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-3
;;
(ert-deftest shu-test-get-phrase-3()
  (let (
        (data "nownowist he time to")
        ;;;    1234567890
        (expected "nownowist")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-4
;;
(ert-deftest shu-test-get-phrase-4()
  (let (
        (data "nownowis the time to")
        ;;;    1234567890
        (expected "nownowis ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-5
;;
(ert-deftest shu-test-get-phrase-5()
  (let (
        (data "Now is the time for all")
        ;;;    1234567890
        (expected "Now is ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 9))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-6
;;
(ert-deftest shu-test-get-phrase-6()
  (let (
        (data "Now is the time for all")
        ;;;    1234567890123
        (expected "Now is the ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 13))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-7
;;
(ert-deftest shu-test-get-phrase-7()
  (let (
        (data "Now is the    time for all")
        ;;;    123456789012345678
        (expected "Now is the    ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 16))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-8
;;
(ert-deftest shu-test-get-phrase-8()
  (let (
        (data "Now is the     time for all")
        ;;;    123456789012345678
        (expected "Now is the     ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 16))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-9
;;
(ert-deftest shu-test-get-phrase-9()
  (let (
        (data "Now is the     time    for all")
        ;;;    1234567890123456788012345
        (expected "Now is the     time   ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 22))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-10
;;
(ert-deftest shu-test-get-phrase-10()
  (let (
        (data "")
        ;;;    1234567890123456788012345
        (expected "")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 22))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-11
;;
(ert-deftest shu-test-get-phrase-11()
  (let (
        (data " ")
        ;;;    1234567890123456788012345
        (expected " ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 22))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-12
;;
(ert-deftest shu-test-get-phrase-12()
  (let (
        (data "love is shown to all mankind.")
        ;;;    1234567890123456788012345
        (expected "love is shown to all ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 26))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-phrase-13
;;
(ert-deftest shu-test-get-phrase-13()
  (let (
        (data "     ")
        ;;;    1234567890123456788012345
        (expected "     ")
        (actual)
    )
    (with-temp-buffer
      (insert data)
      (setq actual (get-phrase 22))
      (should actual)
      (should (stringp actual))
      (should (string= expected actual))
      )
    ))



;;
;;  shu-test-get-hunk-1
;;
(ert-deftest shu-test-get-hunk-1 ()
  (let ((data "1234567890abcdefghijklmno")
        (part)
        (expected-part "12345678")
        (expected-rest "90abcdefghijklmno")
        (actual-rest))
    (with-temp-buffer
      (insert data)
      (setq part (get-hunk 8))
      (should part)
      (should (stringp part))
      (should (string= expected-part part))
      (setq actual-rest (buffer-substring-no-properties (point-min) (point-max)))
      (should actual-rest)
      (should (stringp actual-rest))
      (should (string= expected-rest actual-rest)))
    ))



;;
;;  shu-test-get-hunk-2
;;
(ert-deftest shu-test-get-hunk-2 ()
  (let ((data "12345678")
        (part)
        (expected-part "12345678")
        (expected-rest "")
        (actual-rest))
    (with-temp-buffer
      (insert data)
      (setq part (get-hunk 8))
      (should part)
      (should (stringp part))
      (should (string= expected-part part))
      (setq actual-rest (buffer-substring-no-properties (point-min) (point-max)))
      (should actual-rest)
      (should (stringp actual-rest))
      (should (string= expected-rest actual-rest)))
    ))



;;
;;  shu-test-get-hunk-3
;;
(ert-deftest shu-test-get-hunk-3 ()
  (let ((data "12345678")
        (part)
        (expected-part "12345678")
        (expected-rest "")
        (actual-rest))
    (with-temp-buffer
      (insert data)
      (setq part (get-hunk 9))
      (should part)
      (should (stringp part))
      (should (string= expected-part part))
      (setq actual-rest (buffer-substring-no-properties (point-min) (point-max)))
      (should actual-rest)
      (should (stringp actual-rest))
      (should (string= expected-rest actual-rest)))
    ))



;;
;;  shu-test-get-hunk-4
;;
(ert-deftest shu-test-get-hunk-4 ()
  (let ((data "12345678")
        (part)
        (expected-part "12345678")
        (expected-rest "")
        (actual-rest))
    (with-temp-buffer
      (insert data)
      (setq part (get-hunk 10))
      (should part)
      (should (stringp part))
      (should (string= expected-part part))
      (setq actual-rest (buffer-substring-no-properties (point-min) (point-max)))
      (should actual-rest)
      (should (stringp actual-rest))
      (should (string= expected-rest actual-rest)))
    ))


;;; "Now is the time for all good men"




;;
;;  get-phrase-2
;;
(defun get-phrase-2 (line-limit)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (ss (concat shu-all-whitespace-regexp "+"))
        (sn (concat shu-not-all-whitespace-regexp "+"))
        (something t)
        (tpoint)
        (lpoint)
        (rpoint)
        (xpoint)
        (zpoint)
        (part)
        (lpoint-s)
        )
    (goto-char (point-min))
    (princ (format "\n\nget-phrase: line-limit: %d\n" line-limit )gb)
    (princ (format "   buffer: [%s]\n" (buffer-substring-no-properties (point-min) (point-max))) gb)
    (while something
      (setq tpoint (re-search-forward ss nil t))
      (if (not tpoint)
          (progn
            (setq part (get-hunk line-limit))
            (princ (concat "part1: [" part "]\n") gb)
            (setq something nil)
            )
        (princ (format "tpoint: %d\n" tpoint) gb)
        (when (> tpoint line-limit)
          (setq rpoint (re-search-backward sn nil t))
          (if (not rpoint)
              (progn
                (setq part (get-hunk line-limit))
                (princ (concat "part2: [" part "]\n") gb)
                (setq something nil)
                )
            (princ (format "rpoint: %d\n" rpoint) gb)
            (if lpoint
                (setq lpoint-s (number-to-string lpoint))
              (setq lpoint-s "nil")
                )
            (princ (format "lpoint: %s\n" lpoint-s) gb)
            (setq xpoint (local-nil-max lpoint rpoint))
            (princ (format "xpoint: %d\n" xpoint) gb)
            (if (> xpoint line-limit)
                (progn
                  (setq part (get-hunk line-limit))
                  (princ (concat "part3: [" part "]\n") gb)
                  )
              (setq part (get-hunk xpoint))
              (princ (concat "part4: [" part "]\n") gb)
                )
            )
          (setq something nil)
          )
        )
      (setq lpoint tpoint)
      )
    part
    ))




;;
;;  get-phrase-3
;;
(defun get-phrase-3 (line-limit)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (ss (concat shu-all-whitespace-regexp "+"))
        (sn (concat shu-not-all-whitespace-regexp "+"))
        (something t)
        (tpoint)
        (lpoint)
        (rpoint)
        (xpoint)
        (zpoint)
        (part)
        (lpoint-s)
        )
    (goto-char (point-min))
    (princ (format "\n\nget-phrase: line-limit: %d\n" line-limit )gb)
    (princ (format "   buffer: [%s]\n" (buffer-substring-no-properties (point-min) (point-max))) gb)
    (while something
      (setq tpoint (re-search-forward ss nil t))
      (if (not tpoint)
          (progn
            (setq part (get-hunk line-limit))
            (princ (concat "part1: [" part "]\n") gb)
            (setq something nil)
            )
        (setq tpoint (1- tpoint))
        (princ (format "tpoint: %d\n" tpoint) gb)
        (when (>= tpoint line-limit)
          (if (= tpoint line-limit)
              (progn
                (setq part (get-hunk line-limit))
                (princ (concat "part2: [" part "]\n") gb)
                )
            (setq rpoint (re-search-backward sn nil t))
            (if (not rpoint)
                (progn
                  (setq part (get-hunk line-limit))
                  (princ (concat "part3: [" part "]\n") gb)
                  (setq something nil)
                  )
              (setq rpoint (1+ rpoint))
              (princ (format "rpoint: %d\n" rpoint) gb)
              (if lpoint
                  (setq lpoint-s (number-to-string lpoint))
                (setq lpoint-s "nil")
                )
              (princ (format "lpoint: %s\n" lpoint-s) gb)
              (setq xpoint (local-nil-max lpoint rpoint))
              (princ (format "xpoint: %d\n" xpoint) gb)
              (when (> xpoint line-limit)
                (setq xpoint (local-nil-min lpoint rpoint))
                )
                (setq part (get-hunk xpoint))
                (princ (concat "part5: [" part "]\n") gb)

              )
            )
          (setq something nil)
          )
        )
      (setq lpoint tpoint)
      )
    part
    ))




;;
;;  get-phrase-5
;;
(defun get-phrase-5 (line-limit)
  "Doc string."
  (let (
        (gb (get-buffer-create "**boo**"))
        (ss (concat shu-all-whitespace-regexp "+"))
        (sn (concat shu-not-all-whitespace-regexp "+"))
        (something t)
        (tpoint)
        (lpoint)
        (rpoint)
        (xpoint)
        (zpoint)
        (part)
        (lpoint-s)
        )
    (goto-char (point-min))
    (princ (format "\n\nget-phrase: line-limit: %d\n" line-limit )gb)
    (princ (format "   buffer: [%s]\n" (buffer-substring-no-properties (point-min) (point-max))) gb)
    (while something
      (setq tpoint (re-search-forward ss nil t))
      (if (not tpoint)
          (progn
            (setq part (get-hunk line-limit))
            (princ (concat "part1: [" part "]\n") gb)
            (setq something nil)
            )
        (setq tpoint (1- tpoint))
        (princ (format "tpoint: %d\n" tpoint) gb)
        (when (> tpoint line-limit)
          (setq rpoint (re-search-backward sn nil t))
          (if (not rpoint)
              (progn
                (setq part (get-hunk line-limit))
                (princ (concat "part2: [" part "]\n") gb)
                (setq something nil)
                )
            (setq rpoint (1+ rpoint))
            (princ (format "rpoint: %d\n" rpoint) gb)
            (if lpoint
                (setq lpoint-s (number-to-string lpoint))
              (setq lpoint-s "nil")
                )
            (princ (format "lpoint: %s\n" lpoint-s) gb)
            (setq xpoint (local-nil-max lpoint rpoint))
            (princ (format "xpoint: %d\n" xpoint) gb)
            (if (> xpoint line-limit)
                (progn
                  (setq part (get-hunk line-limit))
                  (princ (concat "part3: [" part "]\n") gb)
                  )
              (setq part (get-hunk xpoint))
              (princ (concat "part4: [" part "]\n") gb)
                )
            )
          (setq something nil)
          )
        )
      (setq lpoint tpoint)
      )
    part
    ))




;;
;;  get-phrase-6
;;
(defun get-phrase-6 (line-limit)
  "Remove from the front of the current buffer and return the longest possible
string of whitespace separated things whose length does not exceed line-limit.
If there is at least one whitespace character before LINE-LIMIT, the string will
end with one or more whitespace characters.  i.e., the string will end on a word
boundary if that is possible.

Words will not be split unless there is no whitespace character before
LINE-LIMIT characters have been scanned, in which case a string of exactly
LINE-LIMIT length will be removed and returned.

This function is used to split a string of words into a set of smaller strings
such that words are not split."
  (let (
        (gb (get-buffer-create "**boo**"))
        (ss (concat shu-all-whitespace-regexp "+"))
        (sn (concat shu-not-all-whitespace-regexp "+"))
        (something t)
        (tpoint)
        (lpoint)
        (rpoint)
        (xpoint)
        (zpoint)
        (part)
        (lpoint-s)
        )
    (goto-char (point-min))
    (princ (format "\n\nget-phrase: line-limit: %d\n" line-limit )gb)
    (princ (format "   buffer: [%s]\n" (buffer-substring-no-properties (point-min) (point-max))) gb)
    (while something
      (setq tpoint (re-search-forward ss nil t))
      (if (not tpoint)
          (if lpoint
              (progn
                (setq part (get-hunk lpoint))
                (princ (concat "part0: [" part "]\n") gb)
                (setq something nil)
                )
            (setq part (get-hunk line-limit))
            (princ (concat "part1: [" part "]\n") gb)
            (setq something nil)
            )
        (setq tpoint (1- tpoint))
        (princ (format "tpoint: %d\n" tpoint) gb)
        (when (> tpoint line-limit)
          (setq rpoint (re-search-backward sn nil t))
          (if (not rpoint)
              (progn
                (setq part (get-hunk line-limit))
                (princ (concat "part2: [" part "]\n") gb)
                (setq something nil)
                )
            (setq rpoint (1+ rpoint))
            (princ (format "rpoint: %d\n" rpoint) gb)
            (if lpoint
                (setq lpoint-s (number-to-string lpoint))
              (setq lpoint-s "nil")
              )
            (princ (format "lpoint: %s\n" lpoint-s) gb)
            (if (< rpoint line-limit)
                (progn
                  (setq part (get-hunk line-limit))
                  (princ (concat "part3: [" part "]\n") gb)
                  )
              (if lpoint
                  (progn
                    (setq part (get-hunk lpoint))
                    (princ (concat "part4: [" part "]\n") gb)
                    )
                (setq part (get-hunk line-limit))
                (princ (concat "part5: [" part "]\n") gb)
                )
              )
            )
          (setq something nil)
          )
        )
      (setq lpoint tpoint)
      )
    part
    ))




;;
;;  local-nil-max
;;
(defun local-nil-max (x y)
  "Return the maximum of two objects, one of which may be nil.  If one
of X or Y is nil, return the other as the maximum.  If neither are nil,
return the maximum of the two."
  (let (
        (maximum)
        )
    (if (not x)
        (setq maximum y)
      (if (not y)
          (setq maximum x)
        (setq maximum (max x y))
          )
        )
    ))



;;
;;  local-nil-min
;;
(defun local-nil-min (x y)
  "Return the minimum of two objects, one of which may be nil.  If one
of X or Y is nil, return the other as the minimum.  If neither are nil,
return the minimum of the two."
  (let (
        (minimum)
        )
    (if (not x)
        (setq minimum y)
      (if (not y)
          (setq minimum x)
        (setq minimum (min x y))
          )
        )
    ))



;;
;;  shu-test-local-nil-max-1
;;
(ert-deftest shu-test-local-nil-max-1 ()
  (let ((x)
        (y 5)
        (expected 5)
        (actual))
    (setq actual (local-nil-max x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-max-2
;;
(ert-deftest shu-test-local-nil-max-2 ()
  (let (
        (x 5)
        (y)
        (expected 5)
        (actual)
        )
    (setq actual (local-nil-max x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-max-3
;;
(ert-deftest shu-test-local-nil-max-3 ()
  (let ((x 5)
        (y 3)
        (expected 5)
        (actual))
    (setq actual (local-nil-max x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-max-4
;;
(ert-deftest shu-test-local-nil-max-4 ()
  (let ((x 3)
        (y 5)
        (expected 5)
        (actual))
    (setq actual (local-nil-max x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-max-5
;;
(ert-deftest shu-test-local-nil-max-5 ()
  (let ((x 5)
        (y 5)
        (expected 5)
        (actual))
    (setq actual (local-nil-max x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-min-1
;;
(ert-deftest shu-test-local-nil-min-1 ()
  (let ((x)
        (y 5)
        (expected 5)
        (actual))
    (setq actual (local-nil-min x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-min-2
;;
(ert-deftest shu-test-local-nil-min-2 ()
  (let (
        (x 5)
        (y)
        (expected 5)
        (actual)
        )
    (setq actual (local-nil-min x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-min-3
;;
(ert-deftest shu-test-local-nil-min-3 ()
  (let ((x 5)
        (y 3)
        (expected 3)
        (actual))
    (setq actual (local-nil-min x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-min-4
;;
(ert-deftest shu-test-local-nil-min-4 ()
  (let ((x 3)
        (y 5)
        (expected 3)
        (actual))
    (setq actual (local-nil-min x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))



;;
;;  shu-test-local-nil-min-5
;;
(ert-deftest shu-test-local-nil-min-5 ()
  (let ((x 5)
        (y 5)
        (expected 5)
        (actual))
    (setq actual (local-nil-min x y))
    (should actual)
    (should (numberp actual))
    (should (= expected actual))
    ))

;;; shu-new.el ends here
