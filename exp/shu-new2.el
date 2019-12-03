;;; shu-new2.el --- Shu project code for dealing wth C++ in Emacs
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
;;  shu-csplit
;;
(defun shu-csplit (prefix)
  "Doc string."
  (interactive "*P")
  (shu-internal-csplit prefix)
  )


;;
;;  shu-internal-csplit
;;
(defun shu-internal-csplit (&optional fixed-width)
  "Doc string."
  (let (
        (xquote "[^\\]\"") ;; quote not preceded by escape
        (tstart (shu-point-in-string))
        (tend)
        (cc)
        (pad-count 0)
        (bpad "")
        (npad "")
        (line-limit 10)
        (original)
        (escape)
        (lines)
        (line)
        (nl "")
        )
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
      (when (< pad-count shu-cpp-line-end)
        (setq line-limit (- shu-cpp-line-end pad-count 1))
        (when (< line-limit 10)
          (setq line-limit 10)
          )
        )
      (setq line-limit (1- line-limit))
      (setq tend (re-search-forward xquote nil t))
      (if (not tend)
          (progn
            (ding)
            (message "%s" "No string end")
            )
        (setq original (buffer-substring-no-properties tstart (1- tend)))
        (when fixed-width
          (setq escape t))
        (setq lines (shu-misc-split-string original line-limit fixed-width escape))
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




;;
;;  shu-creplace
;;
(defun shu-creplace (prefix)
  "This function will replace the C++ string in which point is placed with the
C++ string in the kill ring.  The C++ string in the kill ring is expected to be
a single string with or without quotes.  The C++ string in which point is placed
may have been split into smaller substrings in order to avoid long lines.

Assume you have the sample string that is shown in SHU-CSPLIT

     static const std::string x(\"This is a very long line of text that look\"
                                \"s as though it will go on forever.\");

You wish to replace it with a slightly different line of text, perhaps something
that came from the output of a program.  Copy the new string into the kill ring.
Then put the cursor into any part of any line of the string to be replaced
and invoke this function.  This function will remove the old string, replace it
with the contents of the string in the kill ring, and then split it up into
shorter lines as in the following example.  The string in the kill ring may have
opening and closing quotes or not.

     static const std::string x(\"This is a very long line of text that look\"
                                \"s as though it will go on forever and prob\"
                                \"ably already has done so or is threatening\"
                                \" to do so.\");

This is especially useful if you have a a string constant in a unit test and you
have modified the code that creates the string.  gtest will complain that the
expected string did not match the actual string.  If the actual string is
correct, copy it into the kill ring, go into your unit test, find the old
string, place the cursor in the old string, and replace it with the new."
  (interactive "*P")
  (shu-internal-creplace prefix)
)



;;
;;  shu-internal-creplace
;;
(defun shu-internal-creplace (&optional fixed-width)
  "This function will replace the C++ string in which point is placed with the
C++ string in the kill ring.  The C++ string in the kill ring is expected to be
a single string with or without quotes.  The C++ string in which point is placed
may have been split into smaller substrings in order to avoid long lines.

Assume you have the sample string that is shown in SHU-CSPLIT

     static const std::string x(\"This is a very long line of text that look\"
                                \"s as though it will go on forever.\");

You wish to replace it with a slightly different line of text, perhaps something
that came from the output of a program.  Copy the new string into the kill ring.
Then put the cursor into any part of any line of the string to be replaced
and invoke this function.  This function will remove the old string, replace it
with the contents of the string in the kill ring, and then split it up into
shorter lines as in the following example.  The string in the kill ring may have
opening and closing quotes or not.

     static const std::string x(\"This is a very long line of text that look\"
                                \"s as though it will go on forever and prob\"
                                \"ably already has done so or is threatening\"
                                \" to do so.\");

This is especially useful if you have a a string constant in a unit test and you
have modified the code that creates the string.  gtest will complain that the
expected string did not match the actual string.  If the actual string is
correct, copy it into the kill ring, go into your unit test, find the old
string, place the cursor in the old string, and replace it with the new."
  (interactive)
  (let
      ((xquote "[^\\]\"") ;; quote not preceded by escape
       (start-quote-present)  ;; True if start quote in kill ring
       (end-quote-present)    ;; True if end quote in kill ring
       (have-quotes)          ;; True if kill ring string is quoted
       (unbalanced-quotes)    ;; True if kill ring quotes unbalanced
       (tstart)               ;; Start pos of quoted text in buffer
       (sos )                 ;; Start of string
       (eos )                 ;; End of string
       (del-count ))          ;; Count of chars we deleted in buffer
    (if (not kill-ring)
        (progn
          (ding)
          (message "%s" "Kill ring is empty"))
      ;;
      (with-temp-buffer
        (yank)
        (goto-char (point-min))
        (setq start-quote-present (looking-at "\""))
        (goto-char (1- (point-max)))
        (setq end-quote-present (looking-at "\"")))
      (when (or start-quote-present end-quote-present)
        ;; Have either start or end quote in kill ring
        (setq have-quotes t)
        (when (or (not start-quote-present) (not end-quote-present))
          ;; Missing either start or end quote in kill ring
          (setq unbalanced-quotes t)
          (when start-quote-present
            (ding)
            (message "%s" "String in kill-ring has quote at start but not at end"))
          (when end-quote-present
            (ding)
            (message "%s" "String in kill-ring has quote at end but not at start"))))
      (when (not unbalanced-quotes)
        ;; Find start of string in buffer
        (setq tstart (shu-point-in-string))
        (if (not tstart)
            ;; Point not positioned in a string in the buffer
            (progn
              (ding)
              (message "%s" "Not in string"))
          ;;
          (goto-char tstart) ; Position just after the quote
          (save-excursion  (shu-cunsplit)) ; Make it all one big string
          (setq sos (shu-point-in-string))
          (when have-quotes (setq sos (1- sos))) ;; Delete existing quote
          (re-search-forward xquote nil t)
          (forward-char -1) ;; Now sitting on top of closing quote
          (setq eos (point))
          (setq del-count (- eos sos))
          (when have-quotes (setq del-count (1+ del-count)))
          (goto-char sos)
          (delete-char del-count)
          (save-excursion (yank))
          (when have-quotes (forward-char 1))
          (shu-internal-csplit fixed-width))))
    ))

                           ;;; "This is a string of some sort within these holy portals, revenge remains unknown and to all erring mortals, their way by love is shown and something else."




;;
;;  shu-misc-split-chunk-buffer
;;
(defun shu-misc-split-chunk-buffer (line-limit &optional escape)
  "Split an entire buffer into multiple strings and return a list of the
strings.  Each returned string is LINE-LIMIT characters in length, except for
the last one, which may be shorter."
    (shu-misc-internal-split-buffer line-limit 'shu-misc-get-chunk escape)
    )



;;
;;  shu-misc-internal-split-buffer
;;
(defun shu-misc-internal-split-buffer (line-limit get-function &optional escape)
  "Split an entire buffer into multiple strings and return a list of the
strings.  GET-FUNCTION is the function to call to fetch each new string.
GET-FUNCTION is set to either SHU-MISC-GET-CHUNK or SHU-MISC-GET-PHRASE.

SHU-MISC-GET-CHUNK returns each string as a fixed length string of LINE-LIMIT
characters, except for the last one, which may be shorter.

SHU-MISC-GET-PHRASE returns the longest possible string that ends on a word
boundary and whose length is less than or equal to LINE-LIMIT."
  (let ((something t)
        (line)
        (lines))
    (while something
      (setq line (funcall get-function line-limit escape))
      (if (string= line "")
          (setq something nil)
        (push line lines)))
    (setq lines (nreverse lines))
    lines
    ))





;;
;;  shu-misc-get-chunk
;;
(defun shu-misc-get-chunk (line-limit &optional escape)
  "Return a string that consists of the first LINE-LIMIT characters in the
current buffer.  If LINE-LIMIT is larger than the buffer size, return a
string that is the entire contents of the buffer.  Before returning, delete
from the buffer the returned string."
  (let* (
         (gb (get-buffer-create "**foo**"))
         (spoint (point-min))
         (xpoint (+ line-limit spoint))
         (epoint (if (< (point-max) xpoint) (point-max) xpoint))
         (last-char)
         (part)
         (esc (if escape "t" "nil"))
         )
    (princ (format "\nest: %s, line-limit: %d, (point-max: %d, spoint: %d, epoint %d\n" esc line-limit (point-max) spoint epoint) gb)
;;    (goto-char (1- epoint))
    (when (and (> (point-max) 3) (> line-limit 3))
      (setq last-char (buffer-substring-no-properties (1- epoint) epoint))
      (princ (format "last-char1: %s\n" last-char) gb)
      (when (and escape (string= last-char "\\"))
        (setq epoint (1- epoint))
        (setq last-char (buffer-substring-no-properties (1- epoint) epoint))
        (princ (format "last-char2: %s\n" last-char) gb)
        (when (string= last-char "\\")
          (setq epoint (1- epoint))
          )
        )
      )
    (setq part (buffer-substring-no-properties spoint epoint))
    (delete-region spoint epoint)
    part
    ))





;;
;;  shu-test-shu-misc-get-chunk-5
;;
(ert-deftest shu-test-shu-misc-get-chunk-5 ()
  (let (
        (gb (get-buffer-create "**foo**"))
        (data "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t")
        (part)
        (expected-part "\\t\\t\\t\\t\\t")
        (expected-rest "")
        (actual-rest)
        (escape t)
          )
    (with-temp-buffer
      (insert data)
      (setq part (shu-misc-get-chunk 10 escape))
      (should part)
      (should (stringp part))
      (princ (concat "\npart:\n [" part "]") gb)
      (should (string= expected-part part))
      )
    ))




;;
;;  shu-test-shu-misc-get-chunk-6
;;
(ert-deftest shu-test-shu-misc-get-chunk-6 ()
  (let (
        (gb (get-buffer-create "**foo**"))
        (data "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t")
        (part)
        (expected-part "\\t\\t\\t\\t")
        (expected-rest "")
        (actual-rest)
        (escape t)
          )
    (with-temp-buffer
      (insert data)
      (setq part (shu-misc-get-chunk 9 escape))
      (should part)
      (should (stringp part))
      (princ (concat "\npart2:\n [" part "]") gb)
      (should (string= expected-part part))
      )
    ))



(defconst shu-test-cpp-general-base-string
  (concat
   "\"abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ"
   "RSTUVWXYZ0987654321!@#$%^&*()_+-={}[]|:;'<>,.?abcdefghijklmnopqrstuvwxyz12345678"
   "90!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ\"")
  ;; 12345678901234567890123456789012345678901234567890123456789012345678901234567890
  ;;          1         2         3         4         5         6         7         8
  "A test string for unit tests.")

(defconst shu-test-cpp-general-expected-split1
  (concat
"\"abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNO\"\n"
"\"PQRSTUVWXYZ0987654321!@#$%^&*()_+-={}[]|:;'<>,.?abcdefghijklmnopqrstuvwxyz1234\"\n"
"\"567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ\"")
  "A test string for unit tests.")



;;
;;  shu-test-shu-csplit-1
;;
(ert-deftest shu-test-shu-csplit-1 ()
  (let (
        (gb (get-buffer-create "**foo**"))
        (actual-split)
        (actual-unsplit)
        )
    ;; Split of one long line starting in column 1
    (setq shu-cpp-line-end 80)
    (setq debug-on-error t)
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move to char right after quote
      (shu-internal-csplit t)             ;; Now try to split
      (should (= 215 (point))) ;; Point should have moved
      ;; Make sure the result is what we expect
;;      (setq actual-split (buffer-substring-no-properties (point-min) (point-max)))
;;      (should (string= shu-test-cpp-general-expected-split1 actual-split))
;;      (goto-char 20)
;;      ;; This should restore it to its original state
;;      (shu-cunsplit)
;;      (setq actual-unsplit (buffer-substring-no-properties 1 (1+ (length shu-test-cpp-general-base-string))))
;;      (should (string= shu-test-cpp-general-base-string actual-unsplit))
      )
))



;;
;;  shu-test-shu-csplit-2
;;
(ert-deftest shu-test-shu-csplit-2 ()
  (let (
        (actual-split)
        (actual-unsplit)
        (base3 "\"abcdefg\"")
        )
      ;; Split of very short line starting in column 1
    (setq shu-cpp-line-end 80)
  (with-temp-buffer
      (insert base3)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 2)         ;; Move 2 chars right of quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 10 (point)))  ;; Point should have moved
      ;; But nothing should have been split
      (setq actual-split (buffer-substring-no-properties 1 10))
      (should (string= base3 actual-split))
      (goto-char 4)
      (shu-cunsplit)           ;; Unsplit should do nothing either
      (setq actual-unsplit (buffer-substring-no-properties 1 (1+ (length base3))))
      (should (string= base3 actual-unsplit)))
))



;;
;;  shu-test-shu-csplit-3
;;
(ert-deftest shu-test-shu-csplit-3 ()
  (let (
        (actual-split)
        (actual-unsplit)
        (expected-split2
         (concat "      \"abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHI\"\n"
                 "      \"JKLMNOPQRSTUVWXYZ0987654321!@#$%^&*()_+-={}[]|:;'<>,.?abcdefghijklmnopqr\"\n"
                 "      \"stuvwxyz1234567890!@#$%^&*()_+-={}[]|:;'<>,.?ABCDEFGHIJKLMNOPQ\""))
        )
      ;; Split of one long line with 6 blanks in front of
    (setq shu-cpp-line-end 80)
  (with-temp-buffer
      (insert (concat "      " shu-test-cpp-general-base-string))
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (shu-internal-csplit t)           ;; Now try to split
      (should (= 233 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 233))
      (should (string= expected-split2 actual-split))
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 7 (+ 7 (length shu-test-cpp-general-base-string))))
      (should (string= shu-test-cpp-general-base-string actual-unsplit)))
))



;;
;;  shu-test-shu-csplit-4
;;
(ert-deftest shu-test-shu-csplit-4 ()
  (let (
        (gb (get-buffer-create "**foo**"))
        (actual-split)
        (actual-unsplit)
        (base2
         (concat
          "\"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\""))
        ;;       12345678901234567890123456789012345678901234567890123456789012345678901234567890
        ;;                1         2         3         4         5         6         7         8
        (expected-split3
         (concat
          "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "      \"\\t\\t\\t\\t\""))
        )

    ;; Split a long string full of tab characters
    ;; Make sure there is no split between a "\" and a "t" in the "\t" sequence
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert (concat "      " base2))
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (shu-internal-csplit t)           ;; Now try to split
      (should (= 179 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 179))
      (should (string= expected-split3 actual-split))
      (princ (concat "\n\nactual-split:\n" actual-split) gb)
      (princ (concat "\n\nexpected-split3:\n" expected-split3) gb)
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 7 (+ 7 (length base2))))
      (should (string= base2 actual-unsplit)))
    ))



;;
;;  shu-test-shu-csplit-4a
;;
(ert-deftest shu-test-shu-csplit-4a ()
  (let (
        (gb (get-buffer-create "**foo**"))
        (actual-split)
        (actual-unsplit)
        (base2
         (concat
          "\"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t"
          "\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\""))
        ;;       12345678901234567890123456789012345678901234567890123456789012345678901234567890
        ;;                1         2         3         4         5         6         7         8
        (expected-split3
         (concat
;;          "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
;;          "      \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
;;          "      \"\\t\\t\\t\\t\""))
;;          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
;;          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
;;          "       \"\\t\\t\\t\\t\\t\\t\""))
;;
          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "       \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\"\n"
          "       \"\\t\\t\\t\\t\\t\\t\""))
        (z)

        )

    ;; Split a long string full of tab characters
    ;; Make sure there is no split between a "\" and a "t" in the "\t" sequence
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert (concat "       " base2))
      (setq z (buffer-substring-no-properties (point-min) (point-max)))
      (princ (concat "\nbase2:\n["  z "]\n") gb)
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-internal-csplit)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (shu-internal-csplit t)           ;; Now try to split
      (should (= 182 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 182))
      (princ (concat "\n\nactual-split:\n" actual-split) gb)
      (princ (concat "\n\nexpected-split3a:\n" expected-split3) gb)
      (should (string= expected-split3 actual-split))
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 8 (+ 8 (length base2))))
;;      (setq actual-unsplit (buffer-substring-no-properties (point-min) (point-max)))
      (princ (concat "\nbase2a:\n["  actual-unsplit "]\n") gb)
      (should (string= base2 actual-unsplit)))
    ))




;;
;;  shu-test-shu-csplit-5
;;
(ert-deftest shu-test-shu-csplit-5 ()
  (let (
        (actual-split)
        (actual-unsplit)
        (fail-case-unexpected
         (concat
          "\n"
          "        const std::string       expected(\"Lorem ipsum dolor sit amet, conse\"\n"
          "                                         \"ctetur adipiscing elit. Duis frin\"\n"
          "                                         \"gilla nunc eget ante ornare bland\"\n"
          "                                         \"it. Suspendis\");\n"
          "\n"
          "        EXPECT_EQ(expected, actual);\n"
          "        std::string x(\"unexpected stuff\");\n"
          "\n"
          "\n"))
        (fail-case-unexpected-after-split
         (concat
          "\n"
          "        const std::string       expected(\"Lorem ipsum dolor sit amet, consectetur "
          "adipiscing elit. Duis fringilla nunc eget ante ornare blandit. Suspendis\");\n"
          "\n"
          "        EXPECT_EQ(expected, actual);\n"
          "        std::string x(\"unexpected stuff\");\n"
          "\n"
          "\n"))
        )
    (with-temp-buffer
      (insert fail-case-unexpected)
      (goto-char (point-min))
      (search-forward "Lorem" nil t)
      (shu-cunsplit)
      (setq actual-split (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= fail-case-unexpected-after-split actual-split))
      )
    ))



;;
;;  shu-test-shu-csplit-6
;;
(ert-deftest shu-test-shu-csplit-6 ()
  (let (
        (gb (get-buffer-create "**foo**"))
        (actual-split)
        (actual-unsplit)
        (base2
         (concat
          "\"Whan that Aprille with his shoures soote "
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
          "Than longen folk to goon on pilgrimages,\""
          ))
        (expected-split4
         (concat
          "      \"Whan that Aprille with his shoures soote The droghte of Marche hath \"\n"
          "      \"perced to the roote, And bathed every veyne in swich licour, Of which \"\n"
          "      \"vertu engendred is the flour; Whan Zephirus eek with his swete breeth \"\n"
          "      \"Inspired hath in every holt and heeth The tendre croppes, and the yonge \"\n"
          "      \"sonne Hath in the Ram his halfe cours y-ronne, And smale fowles maken \"\n"
          "      \"melodye, That slepen al the night with open ye, (So priketh hem nature \"\n"
          "      \"in hir corages: Than longen folk to goon on pilgrimages,\""
          ))
        (z)
        )

    ;; Split a long string full of tab characters
    ;; Make sure there is no split between a "\" and a "t" in the "\t" sequence
    (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert (concat "      " base2))
      (setq z (buffer-substring-no-properties (point-min) (point-max)))
      (princ (concat "\nz:\n" z "\n") gb)
      (goto-char (point-min))  ;; Sitting in front of string
      (shu-internal-csplit)    ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 10)        ;; Move inside the string
      (goto-char (point-min))
      (should (search-forward "that" nil t))
      (shu-internal-csplit)    ;; Now try to split
    ;;  (should (= 179 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties (point-min) (point-max)))
      (princ (concat "\n\nactual-split:\n" actual-split) gb)
      (princ (concat "\n\nexpected-split4:\n" expected-split4) gb)
      (should (string= expected-split4 actual-split))
      (goto-char 30)           ;; Go inside of top string
      (shu-cunsplit)           ;; Unsplit should restore to the original form
      (setq actual-unsplit (buffer-substring-no-properties 7 (+ 7 (length base2))))
      (should (string= base2 actual-unsplit))
      )
    ))





;;
;;  shu-test-shu-creplace-1
;;
(ert-deftest shu-test-shu-creplace-1 ()
  (let (
    (actual-split)
    (actual-replace)
    (replace1
      (concat
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
        "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
        "tincidunt gravida interdum et, egestas quis dolor. Quisque"
        "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
        "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
        "finibus lorem at elit varius, volutpat semper arcu"
        "interdum. Quisque egestas tristique velit vel varius. In nisi"
        "nulla, mollis quis mauris sit amet, dictum molestie"
        "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
        "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
        "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
        "vulputate orci facilisis at."))
    (expected-replace1
      (concat
       "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
       "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
       "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
       "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
       "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
       "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
       "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
       "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
       "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\""))
    )
    ;; Do a shu-creplace of a split string with a long, unquoted string
    (setq shu-cpp-line-end 80)
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a different string in the kill ring
          (insert replace1)
          (copy-region-as-kill (point-min) (point-max)))
      (goto-char 30)           ;; Go to first line of split string
      (shu-internal-creplace t)           ;; Replace with contents of kill ring
      ;; Buffer must hold the expected result
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
))



;;
;;  shu-test-shu-creplace-2
;;
(ert-deftest shu-test-shu-creplace-2 ()
  (let (
    (actual-split)
    (actual-replace)
    (replace1
      (concat
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
        "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
        "tincidunt gravida interdum et, egestas quis dolor. Quisque"
        "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
        "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
        "finibus lorem at elit varius, volutpat semper arcu"
        "interdum. Quisque egestas tristique velit vel varius. In nisi"
        "nulla, mollis quis mauris sit amet, dictum molestie"
        "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
        "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
        "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
        "vulputate orci facilisis at."))
    (replace2)
    (expected-replace1
      (concat
       "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
       "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
       "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
       "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
       "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
       "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
       "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
       "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
       "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\""))
    )
  ;; Do a shu-creplace with a quoted string in the kill ring
  (setq replace2 (concat "\"" replace1 "\""))
  (setq shu-cpp-line-end 80)
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a different, quoted string in the kill ring
          (insert replace2)    ;; in the kill ring
          (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)
      (shu-internal-creplace t)
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
))



;;
;;  shu-test-shu-creplace-3
;;
(ert-deftest shu-test-shu-creplace-3 ()
  (let (
    (actual-split)
    (actual-replace)
    (replace1
      (concat
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
        "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
        "tincidunt gravida interdum et, egestas quis dolor. Quisque"
        "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
        "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
        "finibus lorem at elit varius, volutpat semper arcu"
        "interdum. Quisque egestas tristique velit vel varius. In nisi"
        "nulla, mollis quis mauris sit amet, dictum molestie"
        "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
        "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
        "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
        "vulputate orci facilisis at."))
    (replace2)
    )
  ;; Do a shu-creplace with a string in the kill ring that has a quote
  ;; at the beginning but not the end
  (setq replace2 (concat "\"" replace1))
  (setq shu-cpp-line-end 80)
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a quote in the kill ring that
          (insert replace2)    ;; has a quote at beginning but not end
          (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)           ;; Go to first line of string in buffer
      (shu-internal-creplace t)           ;; Try to do a replace
      (should (= 10 (point)))  ;; Nothing should have happened
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      ;; Buffer should remain unchanged
      (should (string= shu-test-cpp-general-expected-split1 actual-replace)))
))



;;
;;  shu-test-shu-creplace-4
;;
(ert-deftest shu-test-shu-creplace-4 ()
  (let (
    (actual-split)
    (actual-replace)
    (replace1
      (concat
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
        "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
        "tincidunt gravida interdum et, egestas quis dolor. Quisque"
        "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
        "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
        "finibus lorem at elit varius, volutpat semper arcu"
        "interdum. Quisque egestas tristique velit vel varius. In nisi"
        "nulla, mollis quis mauris sit amet, dictum molestie"
        "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
        "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
        "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
        "vulputate orci facilisis at."))
    (replace2)
    )
  ;; Do a shu-creplace with a string in the kill ring that has a quote
  ;; at the end but not the beginning
  (setq replace2 (concat  replace1 "\""))
  (setq shu-cpp-line-end 80)
  (with-temp-buffer
      (insert shu-test-cpp-general-base-string)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (shu-internal-csplit t)             ;; Try to split
      (should (= 1 (point)))   ;; Nothing should have happened
      (forward-char 1)         ;; Move inside the quote
      (shu-internal-csplit t)             ;; Try to split again
      (should (= 215 (point))) ;; Point should be here
      (setq actual-split (buffer-substring-no-properties 1 215))
      (should (string= shu-test-cpp-general-expected-split1 actual-split))
      (with-temp-buffer        ;; Put a quote in the kill ring that
          (insert replace2)    ;; has a quote at beginning but not end
          (copy-region-as-kill (point-min) (point-max)))
      (goto-char 10)           ;; Go to first line of string in buffer
      (shu-internal-creplace t)           ;; Try to do a replace
      (should (= 10 (point)))  ;; Nothing should have happened
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      ;; Buffer should remain unchanged
      (should (string= shu-test-cpp-general-expected-split1 actual-replace)))

))



;;
;;  shu-test-shu-creplace-5
;;
(ert-deftest shu-test-shu-creplace-5 ()
  (let ((actual-split)
        (actual-replace)
        (original1
         (concat
          "\"Ut porta, quam eget tempor aliquet, lectus elit pulvinar dolor, sit amet d\"\n"
          "\"ignissim est massa ut arcu. Donec est dolor, ultricies eu cursus id, imper\"\n"
          "\"diet aliquam dui. Pellentesque ut blandit quam. Nunc dictum tempus enim no\"\n"
          "\"n elementum. Phasellus scelerisque purus sapien, quis congue ipsum ultrice\"\n"
          "\"s ut. Sed vel nibh ornare, sodales mi sed, pretium ex. Integer convallis, \"\n"
          "\"quam vulputate tempus volutpat, dui odio tincidunt nisi, et tincidunt nunc\"\n"
          "\" lectus id velit. Donec volutpat mi non laoreet scelerisque. Sed id leo si\"\n"
          "\"t amet mauris hendrerit ullamcorper. Curabitur fermentum libero vel ullamc\"\n"
          "\"orper feugiat. Nunc et hendrerit nulla, nec condimentum urna. Nullam et co\"\n"
          "\"ndimentum nisl, id semper ante. Vivamus eu tempor erat, sed tincidunt mi. \"\n"
          "\"Phasellus et massa viverra sapien bibendum tempor eget a enim. Duis varius\"\n"
          "\", dolor in ultrices posuere, lorem enim tincidunt enim, at iaculis libero \"\n"
          "\"eros id felis. Sed et justo mattis dolor porttitor fermentum id ut lorem.\""))
        (replace1
         (concat
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu"
          "justo lacinia lectus imperdiet dignissim. Suspendisse neque purus,"
          "tincidunt gravida interdum et, egestas quis dolor. Quisque"
          "fermentum lorem nec dictum tempor. Etiam eget enim pharetra,"
          "tristique ex at, porta dui. Fusce varius non orci ut semper. Nunc"
          "finibus lorem at elit varius, volutpat semper arcu"
          "interdum. Quisque egestas tristique velit vel varius. In nisi"
          "nulla, mollis quis mauris sit amet, dictum molestie"
          "justo. Curabitur feugiat eu mi at consectetur. Sed ultrices massa"
          "vel turpis pulvinar tristique. Etiam aliquam vulputate magna,"
          "vitae commodo leo dictum at. Donec aliquam purus tortor, sit amet"
          "vulputate orci facilisis at."))
        (expected-replace1
         (concat
          "\"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eujusto lacinia\"\n"
          "\" lectus imperdiet dignissim. Suspendisse neque purus,tincidunt gravida interdu\"\n"
          "\"m et, egestas quis dolor. Quisquefermentum lorem nec dictum tempor. Etiam eget\"\n"
          "\" enim pharetra,tristique ex at, porta dui. Fusce varius non orci ut semper. Nu\"\n"
          "\"ncfinibus lorem at elit varius, volutpat semper arcuinterdum. Quisque egestas \"\n"
          "\"tristique velit vel varius. In nisinulla, mollis quis mauris sit amet, dictum \"\n"
          "\"molestiejusto. Curabitur feugiat eu mi at consectetur. Sed ultrices massavel t\"\n"
          "\"urpis pulvinar tristique. Etiam aliquam vulputate magna,vitae commodo leo dict\"\n"
          "\"um at. Donec aliquam purus tortor, sit ametvulputate orci facilisis at.\"")))
    ;; Do a shu-creplace of a split string with a long, unquoted string
  (setq shu-cpp-line-end 80)
    (with-temp-buffer
      (insert original1)
      (goto-char (point-min))  ;; Sitting on top of open quote
      (with-temp-buffer        ;; Put a different string in the kill ring
        (insert replace1)
        (copy-region-as-kill (point-min) (point-max)))
      (goto-char 626)          ;; Go to five lines from the bottom
      (shu-internal-creplace t)           ;; Replace with contents of kill ring
      ;; Buffer must hold the expected result
      (setq actual-replace (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected-replace1 actual-replace)))
    ))


;;; shu-new2.el ends here
