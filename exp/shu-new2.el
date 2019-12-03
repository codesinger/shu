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


;;; shu-new2.el ends here
