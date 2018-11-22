;;; shu-base.el --- Shu project global variables and functions
;;
;; Copyright (C) 2013 Stewart L. Palmer
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
;;  shu-base.el
;;
;;  Collection of miscellaneous functions used by other packages
;;

(provide 'shu-base)

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
  "A regular expression to match a varaiable name in a C or C++ program.")

(defconst shu-non-cpp-name
  (concat "[^" (substring shu-cpp-name 1))
  "A regular expression to match a character not valid in a varaiable name
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
angle brackets instead of quotes."
  :type '(number)
  :group 'shu-base)

(defcustom shu-cpp-default-global-namespace nil
  "The string that defines the default global C++ namepace, if any.
If this has a value, then C++ classes are declared with a two level
namespace with the global namespace encompassig the local one"
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
  "*The regular expression that defines the delimeter used to start
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
(defun shu-point-in-string ()
  "Return the start position of the string text if point is sitting between a pair
of non-escaped quotes (double quotes).  The left-hand quote (opening quote) must be
on the same line as point.  Does not take into account comments, #if 0, etc.  It is
assumed that someone who wants to operate on a string will generally position point
within a legitimate string."
  (let (
        (xquote "^\\\"\\|[^\\]\\\"") ;; Match either a quote at the beginning
        ;; of a line or a quote not preceded by \
        (start-pos )
        (bol (save-excursion (beginning-of-line) (point)))
        )
    (save-excursion
      ;; Search backwards for either a quote at beginning of line or a quote
      ;; not preceded by \.  If we find the quote not at the beginning of the
      ;; line, we are positioned one character before it.  If we find the quote
      ;; at the beginning of the line, we are sitting on top of it.
      (when (re-search-backward xquote bol t)
        ;; If in front of the quote, move to sitting on it
        (when (not (looking-at "\\\"")) (forward-char 1))
        (setq start-pos (1+ (point))) ;; This is where string text starts
        (when (not (re-search-forward xquote nil t))
          (setq start-pos nil))))
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
and pad filled to length WIDTH.  The number is formatted as comma saparated
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
string is not terminated in the buffer."
  (let (
        (limit     t)
        (epoint    nil)
        (string-end    (regexp-quote string-term))
        (back-slash-or-quote (regexp-opt (list "\\" string-term) nil))
        )
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
          (forward-char 2)))
      )
    epoint
    ))


;;
;;  shu-line-and-column-at
;;
(defun shu-line-and-column-at (arg)
"Return the line number and column number of the point passed in as an argument."
  (let (
    (line-no    -1)
    (col-no     -1)
       )
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
  (let (
    (col-no     -1)
       )
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
  (replace-regexp-in-string (rx (* (any " \b\t\n\v\f\r")) eos) "" input-string))



;;
;;  shu-minimum-leading-space
;;
(defun shu-minimum-leading-space (arg &optional white-space)
  "Find the amount of white space in front of point and return either that
count or ARG, whichever is smaller.  Used by functions that wish to
safely delete ARG characters of white space from the current position
without deleting any characters that are not white space.
An optional second argument is a string that defines what is meant
by white space.  The default definition is blanks and tabs."
  (let ((bos (point))                   ; Remember beginning point of space
        (ccount 0)                      ; The count that can be deleted
        (bspace " \t")                  ; The blank space string
        (wspace 0))                     ; Amount of white space that exists
    (save-excursion
      (when white-space                 ; A white space definition was given
        (setq bspace white-space))      ; Use the given definition
      (skip-chars-forward bspace)       ; Skip to first non white space
      (setq wspace (- (point) bos))     ; White space count at front of line
      (setq ccount (min wspace arg)))    ; Shift count is min of avail & wanted
    ))
