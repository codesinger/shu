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
        (ss (concat shu-all-whitespace-regexp "+"))
        (sn (concat shu-not-all-whitespace-regexp "+"))
        (something t)
        (pt 0)
        (tpoint)
        )
    (while something
      (goto-char (point-min))
      (setq tpoint (re-search-forward ss nil t))
      )

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
;;  get-phrase
;;
(defun get-phrase (line-limit)
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
          (setq tpoint (1- tpoint))
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
                    (princ (concat "part4: [" part "]\n") gb)
                    )
                (setq part (get-hunk xpoint))
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
;;  local-max
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
;;  shu-test-local-max-1
;;
(ert-deftest shu-test-local-max-1 ()
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
;;  shu-test-local-max-2
;;
(ert-deftest shu-test-local-max-2 ()
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
;;  shu-test-local-max-3
;;
(ert-deftest shu-test-local-max-3 ()
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
;;  shu-test-local-max-4
;;
(ert-deftest shu-test-local-max-4 ()
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
;;  shu-test-local-max-5
;;
(ert-deftest shu-test-local-max-5 ()
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


;;; shu-new.el ends here
