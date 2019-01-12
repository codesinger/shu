;;; shu-base.el --- Shu project global variables and functions
;;
;; Copyright (C) 2013 Stewart L. Palmer
;;
;; Package: shu-base
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;; Version: 1.4
;; Homepage: https://github.com/codesinger/shu.git
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

;;; Commentary:

;; Collection of miscellaneous functions  and constants used by other
;; packages in this repository.  Most of the elisp files in this repository
;; depend on this file.

;;; Code:

(provide 'shu-base)

(defconst shu-version "1.4"
  "The version number of the Shu elisp package.")

(defconst shu-date "2019 Jan 12"
  "Date of the most recent merge with the master branch.")

(defconst shu-all-commits
  (list
   (cons "1.2"   "8a0413fbc02d77f625ab0e54fc449ec159ea2f98")
   (cons "1.3"   "6b2f890394d200303b114f49aa3d1e9880b1feab")
   (cons "1.4"   "UNKNOWN"))
  "A list of all commits by version starting with version 1.2")

(defconst shu-last-commit "UNKNOWN"
  "The git SHA-1 of the most recent commit.  This cannot be the SHA-1 hash of
the last commit because that is not known until after the commit happens.  Just
before the merge with master, a commit is done.  Its SHA-1 hash is copied into
this constant.  Then one more commit is done to push this change out.  If you
want to find this version in git, look for the commit after the one defined
here.")

(defconst shu-cpp-name-list
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
        "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
        "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
        "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
        "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
        "_" "$")
  "List of all characters that can be present in a C++ file name.")

(defconst shu-cpp-name (regexp-opt
                        shu-cpp-name-list  nil)
  "A regular expression to match a variable name in a C or C++ program.")

(defconst shu-non-cpp-name
  (concat "[^" (substring shu-cpp-name 1))
  "A regular expression to match a character not valid in a variable name
in a C or C++ program.")

(defconst shu-cpp-file-name (regexp-opt
                             (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                                   "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                                   "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                                   "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                                   "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                                   "_" "$" "." "-") nil)
  "A regular expression to match the name of a C or C++ file in the file system.")


(defgroup shu-base nil
  "A set of useful functions for dealing with C++ and other issues."
  :group 'applications)

(defcustom shu-cpp-indent-length 4
  "Size of the standard indent for names within class declarations, etc."
  :type '(number)
  :group 'shu-base)

(defcustom shu-cpp-comment-start 36
  "Column in which a standard comment starts.  Any comment that starts to the left of
this point is assumed to be a block comment."
  :type '(number)
  :group 'shu-base)

(defcustom shu-cpp-comment-end 78
  "Standard end point (right hand margin) for C style comments."
  :type '(number)
  :group 'shu-base)

(defcustom shu-cpp-author "Your name goes here"
  "The string to place in the doxygen author tag."
  :type '(string)
  :group 'shu-base)

(defcustom shu-cpp-use-bde-library nil
  "Set non-nil if the BDE library is to be used for generated C++ code."
  :type '(number)
  :group 'shu-base)

(defcustom shu-cpp-include-user-brackets nil
  "Set non-nil if user written include files are to be delimited by
angle brackets instead of quotes.
In many C and C++ environments, system include files such as stdio.h are delimited
by angle brackets, for example:

      #include <stdio.h>

while user written include files are delimited by quotes, for example:

      #include \"myclass.h\"

If this variable is non-nil, then user written include files are delimited
by angle brackets and an include of \"myclass.h\" would be written as

      #include <myclass.h>"
  :type '(number)
  :group 'shu-base)

(defcustom shu-cpp-default-global-namespace nil
  "The string that defines the default global C++ namepace, if any.
If this has a value, then C++ classes are declared with a two level
namespace with the global namespace encompassing the local one"
  :type '(string)
  :group 'shu-base)

(defcustom shu-cpp-default-namespace nil
  "The string that defines the default C++ namepace, if any."
  :type '(string)
  :group 'shu-base)

(defconst shu-all-whitespace-chars
  (list " " "\b" "\t" "\n" "\v" "\f" "\r")
  "List of all whitespace characters.
Since the syntax table considers newline to be (among other things) a
comment terminator, the usual \\s- won't work for whitespace that includes
newlines.")

(defconst shu-all-whitespace-regexp
  (regexp-opt shu-all-whitespace-chars nil)
  "Regular expression to search for whitespace.  Since the syntax table considers
newline to be (among other things) a comment terminator, the usual \\s- won't work
for whitespace that includes newlines.  Note that this regular expression is a
character alternative enclosed in left and right brackets.  skip-chars-forward does
not expect character alternatives to be enclosed in square brackets and will include
the left and right brackets in the class of characters to be skipped.")

(defconst shu-all-whitespace-regexp-scf
  (substring shu-all-whitespace-regexp 1 (1- (length shu-all-whitespace-regexp)))
  "This is the regular expression contained in shu-all-whitespace-regexp but with
the enclosing left and right square brackets removed.  skip-chars-forward does
not expect character alternatives to be enclosed in square brackets and thus
will include the brackets as characters to be skipped.")

(defconst shu-not-all-whitespace-regexp
  (concat "[^" (substring shu-all-whitespace-regexp 1))
  "Regular expression to search for non-whitespace.  Since the syntax table considers
newline to be (among other things) a comment terminator, the usual \\s- won't work
for whitespace that includes newlines.  Note that this regular expression is a
character alternative enclosed in left and right brackets.  skip-chars-forward does
not expect character alternatives to be enclosed in square brackets and will include
the left and right brackets in the class of characters to be skipped.")

(defconst shu-comment-start-pattern "/[/\\*]"
  "The regular expression that defines the delimiter used to start
a comment.")

(defconst shu-unit-test-buffer "**shu-unit-tests**"
  "The name of the buffer into which unit tests place their output and debug trace.")

(defconst shu-global-buffer-name "**shu-global-operations**"
  "The name of the buffer into which shu-global-operation places its output.")

;;
;;  shu-the-line-at
;;
(defun shu-the-line-at (arg)
  "Return the line number of the point passed in as an argument.  The line number is
relative to the start of the buffer, whether or not narrowing is in effect."
  (let ((current-line (save-excursion (save-restriction (widen) (goto-char arg) (line-number-at-pos)))))
    current-line))

;;
;;  shu-current-line
;;
(defun shu-current-line ()
  "Return the line number of the current line relative to the start of the buffer."
  (let ((current-line (save-excursion (save-restriction (widen) (line-number-at-pos)))))
    current-line))


;;
;;  shu-goto-line
;;
(defun shu-goto-line (line-number)
  "Move point to line LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))


;;
;;  shu-kill-new
;;
(defun shu-kill-new (string)
  "Effectively do a kill-new with STRING but use kill-ring-save from
a temporary buffer.  This seems to to a better job of putting STRING
in a place from which other programs running on Linux and Windows can
do a paste."
  (with-temp-buffer
    (insert string)
    (kill-ring-save (point-min) (point-max))))



;;
;;  shu-point-in-string
;;
(defun shu-point-in-string (&optional pos)
  "Return the start position of the string text if point is sitting between a pair
of non-escaped quotes (double quotes).  The left-hand quote (opening quote) must be
on the same line as point.  The string must be on a single line.  If point is sitting
on a quote, then it is not inside a string.  In order to be inside a string, point
must lie between two non-escaped quotes.  The optional argument POS, if specified,
is used in place of the position of point."
  (let ((xquote "^\\\"\\|[^\\]\\\"") ;; Match either a quote at the beginning
        ;; of a line or a quote not preceded by \
        (start-pos)
        (bol (line-beginning-position))
        (eol (line-end-position)))
    (save-excursion
      (when pos
        (goto-char pos))
      ;; Search backwards for either a quote at beginning of line or a quote
      ;; not preceded by \.  If we find the quote not at the beginning of the
      ;; line, we are positioned one character before it.  If we find the quote
      ;; at the beginning of the line, we are sitting on top of it.
      (if (looking-at "\"")
          (setq start-pos nil)
        (when (re-search-backward xquote bol t)
          (when (not (and
                      (= (point) bol)
                      (looking-at "\"")))
            (forward-char 1))
          (forward-char 1)
          (setq start-pos (point)) ;; This is where string text starts
          (if (looking-at "\"")
              (setq start-pos nil)
            (when (not (re-search-forward xquote eol t))
                (setq start-pos nil))))))
    start-pos
    ))


;;
;;  shu-format-num
;;
(defun shu-format-num (num width)
  "Return a printable representation of NUM in a string right justified
and pad filled to length WIDTH."
  (let ((num
         (if (stringp num)
             num
           (number-to-string num))))
    (when (<= width (length num))
      (setq width (length num)))
    (concat (make-string (- width (length num)) ?\ ) num)
    ))



;;
;;  shu-fixed-format-num
;;
(defun shu-fixed-format-num (num width)
  "Return a printable representation of NUM in a string right justified
and pad filled to length WIDTH.  The number is formatted as comma separated
as defined by shu-group-number."
  (let ((num  (shu-group-number num 3)))
    (when (<= width (length num))
      (setq width (length num)))
    (concat (make-string (- width (length num)) ?\ ) num)
    ))


;;
;;  shu-group-number
;;
(defun shu-group-number (num &optional size char)
  "Format NUM as string grouped to SIZE with CHAR.  Default SIZE if 3.  Default CHAR
is ','.  e.g., 1234567 is formatted as 1,234,567.  Argument to be formatted may be
either a string or a number."
  (let* ((size (or size 3))
         (char (or char ","))
         (str (if (stringp num)
                  num
                (number-to-string num)))
         ;; omitting any trailing non-digit chars
         (pt (or (string-match "[^0-9]" str) (length str))))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size))
                        char
                        (substring str (- pt size))))
      (setq pt (- pt size)))
    str
    ))


;;
;;  shu-end-of-string
;;
(defun shu-end-of-string (string-term)
  "Return the point that terminates the current quoted string in the buffer.
The single argument STRING-TERM is a string containing the character that
started the string (single or double quote).  Return nil if the current
string is not terminated in the buffer.

This function actually returns the position after the terminating quote and
also moves point to that position.  This cannot be changed because other
functions depend on this frankly strange behavior."
  (let ((limit     t)
        (epoint    nil)
        (string-end    (regexp-quote string-term))
        (back-slash-or-quote (regexp-opt (list "\\" string-term) nil)))
    (forward-char 1)
    (while limit
      (setq epoint (re-search-forward back-slash-or-quote nil t))
      (if (not epoint)
          (setq limit nil)
        (backward-char 1)
        (if (looking-at string-end)
            (progn
              (forward-char 1)
              (setq limit nil))
          (forward-char 2))))
    epoint
    ))


;;
;;  shu-line-and-column-at
;;
(defun shu-line-and-column-at (arg)
  "Return the line number and column number of the point passed in as an argument."
  (let ((line-no    -1)
        (col-no     -1))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char arg)
        (beginning-of-line)
        (setq line-no (1+ (count-lines 1 (point))))
        (setq col-no (1+ (- arg (point))))))
    (cons line-no col-no)
    ))


;;
;;  shu-the-column-at
;;
(defun shu-the-column-at (arg)
  "Return the column number of the point passed in as an argument."
  (let ((col-no     -1))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char arg)
        (beginning-of-line)
        (setq col-no (1+ (- arg (point))))))
    col-no
    ))


;;
;;  shu-remove-trailing-whitespace
;;
(defun shu-remove-trailing-all-whitespace (input-string)
  "Return a copy of INPUT-STRING with all trailing whitespace removed.  All control
characters are considered whitespace."
  (shu-trim-trailing input-string)
  )



;;
;;  shu-minimum-leading-space
;;
(defun shu-minimum-leading-space (arg &optional white-space)
  "Find the amount of white space in front of point and return either that
count or ARG, whichever is smaller.  Used by functions that wish to
safely delete ARG characters of white space from the current position
without deleting any characters that are not white space.
An optional second argument is a string that defines what is meant
by white space.  The default definition is SHU-ALL-WHITESPACE-REGEXP."
  (let ((bos (point))
        (bspace (or shu-all-whitespace-regexp-scf white-space))
        (wspace 0))
    (save-excursion
      (skip-chars-forward bspace)
      (setq wspace (- (point) bos))
      (min wspace arg))
    ))



;;
;;  shu-trim-leading
;;
(defun shu-trim-leading (string)
  "Trim leading whitespace from STRING.  Return the modified string.  String
remains unmodified if it had no leading whitespace."
  (let ((ss (concat "\\`\\(?:" shu-all-whitespace-regexp "+\\)")))
    (setq string (replace-regexp-in-string ss "" string t t))
    string
    ))



;;
;;  shu-trim-trailing
;;
(defun shu-trim-trailing (string)
  "Trim trailing whitespace from STRING.  Return the modified string.  String
remains unmodified if it had no trailing whitespace."
  (let ((ss (concat "\\(?:" shu-all-whitespace-regexp "+\\)\\'")))
    (setq string (replace-regexp-in-string ss "" string t t))
    string
    ))



;;
;;  shu-trim
;;
(defun shu-trim (string)
  "Trim leading and trailing whitespace from STRING.  Return the modified
string.  String remains unmodified if it had no leading or trailing whitespace."
  (let ((trimmed (shu-trim-trailing (shu-trim-leading string))))
    trimmed
    ))

;;; shu-base.el ends here
