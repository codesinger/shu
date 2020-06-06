;;; shu-base.el --- Shu project global variables and functions
;;
;; Copyright (C) 2013 Stewart L. Palmer
;;
;; Package: shu-base
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;; Version: 1.6.37
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

(defconst shu-version "1.6.37"
  "The version number of the Shu elisp package.")

(defconst shu-date "2019 Nov 18"
  "Date of the most recent merge with the master branch.")

(defconst shu-all-commits
  (list
   (cons "1.2"   "8a0413fbc02d77f625ab0e54fc449ec159ea2f98")
   (cons "1.3"   "6b2f890394d200303b114f49aa3d1e9880b1feab")
   (cons "1.4"   "479a129fedba8a7f95d60b38de3383ab22575389")
   (cons "1.5"   "821beb4ace51edbae436f7ac1da67873cc5925c2")
   (cons "1.6"   "dcfe32ef84a3d4ca54b0ac43e754249f1e21f35e")
   (cons "1.7"   "UNKNOWN"))
  "A list of all commits by version starting with version 1.2")

(defconst shu-last-commit "dcfe32ef84a3d4ca54b0ac43e754249f1e21f35e"
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
  "List of all characters that can be present in a C++ variable name.")

(defconst shu-cpp-name (regexp-opt
                        shu-cpp-name-list  nil)
  "A regular expression to match a variable name in a C or C++ program.")

(defconst shu-non-cpp-name
  (concat "[^" (substring shu-cpp-name 1))
  "A regular expression to match a character not valid in a variable name
in a C or C++ program.")

(defconst shu-cpp-file-name-list
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
        "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
        "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
        "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
        "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
        "_" "$" "." "-")
  "List of all characters that can be present in a C++ file name.")

(defconst shu-cpp-file-name (regexp-opt
                             shu-cpp-file-name-list nil)
  "A regular expression to match the name of a C or C++ file in the file system.")

(defconst shu-cpp-file-directory-name (regexp-opt
                                       (append shu-cpp-file-name-list (list "/")) nil)
  "A regular expression to match the name of a C or C++ file in the file system
including directory names.  This is the same regular expression as
SHU-CPP-FILE-NAME-LIST with a \"/\" included.")


(defgroup shu-base nil
  "A set of useful functions for dealing with C++ and other issues."
  :group 'applications)

(defcustom shu-trim-file t
  "If true, whitespace is trimmed from the end of lines and blank lines at the
end of a file are deleted when a file is saved."
  :type 'boolean
  :group 'shu-base)

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

(defcustom shu-cpp-line-end 80
  "Standard end point (right hand margin) for a line of code."
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


(defcustom shu-cpp-default-member_prefix "m_"
  "The prefix used to indicate that a variable in a class is a member variable of
that class."
  :type '(string)
  :group 'shu-base)


(defcustom shu-cpp-default-allocator-name "m_allocator"
  "The name of the class member variable that holds the pointer to the allocator
used by the class."
  :type '(string)
  :group 'shu-base)


(defcustom shu-cpp-default-allocator-type "std::allocator"
  "The class name of the standard abstract allocator."
  :type '(string)
  :group 'shu-base)


(defcustom shu-cpp-std-namespace "std"
  "The name of the namespace for the C++ standard library.  Some users of the
BDE open source Bloomberg libraries may prefer \"bsl\" instead."
  :type '(string)
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

(defconst shu-library-files
  (list
   "shu-misc.elc"
   "shu-cpp-token.elc"
   "shu-cpp-general.elc"
   "shu-cpp-misc.elc"
   "shu-cpp-match.elc"
   "shu-match.elc"
   "shu-bde.elc"
   "shu-bde-cpp.elc"
   "shu-cpp-project.elc"
   "shu-nvplist.elc"
   "slp-comment-hooks.elc"
   "slp-bb-comment-hooks.elc")
  "A list of all of the library files that comprise the Shu elisp package in the
order in which they should be loaded.")



;;
;;  shu-load-library-files
;;
(defun shu-load-library-files (path-to-libs)
  "Load all of the library files listed in SHU-LIBRARY-FILES using the path
PATH-TO-LIBS.  Return true if all of the files were successfully loaded."
  (let ((libs shu-library-files)
        (lib-path (shu-delete-last-char-if path-to-libs "/"))
        (lib)
        (ln)
        (no-error nil)
        (no-message t)
        (no-suffix t)
        (loaded)
        (did-load t))
    (while libs
      (setq lib (car libs))
      (setq ln (concat lib-path "/" lib))
      (setq loaded (load ln no-error no-message no-suffix))
      (when (not loaded)
        (setq did-load nil))
      (setq libs (cdr libs)))
    did-load
    ))



;;
;;  shu-conditional-load-library-files
;;
(defun shu-conditional-load-library-files (path-to-libs libs)
  "Conditionally Load all of the library files listed in in the list LIBS using
the path PATH-TO-LIBS.  A file in the list is loaded only if FILE-READABLE-P
returns true for that file.  Return true if all readable files were loaded.
Return false if any readable file failed to load."
  (let ((lib-path (shu-delete-last-char-if path-to-libs "/"))
        (lib)
        (ln)
        (no-error nil)
        (no-message t)
        (no-suffix t)
        (loaded)
        (did-load t))
    (while libs
      (setq lib (car libs))
      (setq ln (concat lib-path "/" lib))
      (when (file-readable-p ln)
        (setq loaded (load ln no-error no-message no-suffix))
        (when (not loaded)
          (setq did-load nil)))
      (setq libs (cdr libs)))
    did-load
    ))



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
;;  shu-delete-last-char-if
;;
(defun shu-delete-last-char-if (input test-char)
  "Return the string INPUT with the last character removed if the last character
is equal to the string TEST-CHAR.  If the last character is not equal to the
string TEST-CHAR, return the input string unmodified."
  (let ((last-char)
        (new-string input))
    (when (> (length input) 0)
      (setq last-char (substring input (1- (length input))))
      (when (string= last-char test-char)
        (setq new-string (substring input 0 (1- (length input))))))
    new-string
    ))



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
        (spoint)
        (bol (line-beginning-position))
        (eol (line-end-position)))
    (save-excursion
      (when pos
        (goto-char pos))
      (setq spoint (1+ (point)))
      ;; Search backwards for either a quote at beginning of line or a quote
      ;; not preceded by \.  If we find the quote not at the beginning of the
      ;; line, we are positioned one character before it.  If we find the quote
      ;; at the beginning of the line, we are sitting on top of it.
      (when (re-search-backward xquote bol t)
        (when (not (and
                    (= (point) bol)
                    (looking-at "\"")))
          (forward-char 1))
        (forward-char 1)
        (setq start-pos (point)) ;; This is where string text starts
        (if (not (re-search-forward xquote eol t))
            (setq start-pos nil)
          (when (or
                 (= start-pos (point))
                 (= spoint (point)))
            (setq start-pos nil)))))
    start-pos
    ))



;;
;;  shu-format-num
;;
(defun shu-format-num (num width &optional pad-char)
  "Return a printable representation of NUM in a string right justified and pad
filled to length WIDTH.  The number is padded with blanks unless an optional
PAD-CHAR is supplied, which is then used instead of blanks.

PAD-CHAR must be a character, not a string.  If you want the string
representation of the number to be right justified and zero filled, specify a
pad character of ?0.  Do not use a pad character of \"0\""
  (let ((num
         (if (stringp num)
             num
           (number-to-string num)))
        (pc (if pad-char
                pad-char
              ?\ )))
    (when (<= width (length num))
      (setq width (length num)))
    (concat (make-string (- width (length num)) pc) num)
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
;;  shu-end-of-dq-string
;;
(defun shu-end-of-dq-string ()
  "Return the point that represents the end of the current quoted string in the
buffer.  The quote character is the double quote character (\").  Escaped quotes
are skipped.  If the current string is not terminated by a ;doubloe quote
character, nil is returned.  If the current string is terminated by a double
quote character, the position following the quote is returned and point is set
to that position."
  (let ((nequote "[^\\]\\\"")  ;; Non-escaped quote
        (epoint))
    (save-excursion
      (setq epoint (re-search-forward nequote nil t)))
    (when epoint
      (goto-char epoint))
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
  "Find the amount of white space in front of point on the same line and return
either that count or ARG, whichever is smaller.  Used by functions that wish to
safely delete ARG characters of white space from the current position without
deleting any characters that are not white space.  An optional second argument,
WHITE-SPACE, is a string that defines what is meant by white space.  The default
definition is SHU-ALL-WHITESPACE-REGEXP."
  (let ((bos (point))
        (bspace (or shu-all-whitespace-regexp-scf white-space))
        (bol (line-beginning-position))
        (eol (line-end-position))
        (llen)
        (wspace 0))
    (setq llen (- eol bol))
    (save-excursion
      (skip-chars-forward bspace)
      (setq wspace (- (point) bos))
      (min wspace llen arg))
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



;;
;;  shu-add-to-alist
;;
(if (version< emacs-version "26.1")
    (progn
      (defmacro shu-add-to-alist (added-item new-item alist)
        "Add an item to an alist.  The car of NEW-ITEM is a key to be added to
the alist ALIST.  If the key does not already exist in ALIST, NEW-ITEM is added
to ALIST.  ADDED-ITEM is either the item that was added or the item that was
previously there.  If (eq ADDED-ITEM NEW-ITEM), then NEW-ITEM was added to the
list.  If (not (eq ADDED-ITEM NEW-ITEM)), then the key already existed in the
list and ADDED-ITEM is the item that was already on the list with a matching
key.  equal is the function used to determine equality.

This version of this macro is for versions of emacs older than 26.1, which was
released on May 28, 2018"
        `(if (not ,alist)
             (progn
               (push ,new-item ,alist)
               (setq ,added-item ,new-item))
           (setq ,added-item (assoc (car ,new-item) ,alist))
           (when (not ,added-item)
             (push ,new-item ,alist)
             (setq ,added-item ,new-item)))
        ))
  (defmacro shu-add-to-alist (added-item new-item alist &optional testfn)
    "Add an item to an alist.  The car of NEW-ITEM is a key to be added to the
alist ALIST.  If the key does not already exist in ALIST, NEW-ITEM is added to
ALIST.  ADDED-ITEM is either the item that was added or the item that was
previously there.  If (eq ADDED-ITEM NEW-ITEM), then NEW-ITEM was added to the
list.  If (not (eq ADDED-ITEM NEW-ITEM)), then the key already existed in the
list and ADDED-ITEM is the item that was already on the list with a matching
key.  equal is the function used to determine equality unless TESTFN is
supplied, in which case TESTFN is used.

This version of this macro is for emacs 26.1 and later.  emacs 26.1 was released
on May 28, 2018."
    `(if (not ,alist)
         (progn
           (push ,new-item ,alist)
           (setq ,added-item ,new-item))
       (setq ,added-item (assoc (car ,new-item) ,alist ,testfn))
       (when (not ,added-item)
         (push ,new-item ,alist)
         (setq ,added-item ,new-item)))
    ))



;;
;;  shu-invert-alist-list
;;
(defun shu-invert-alist-list (alist &optional compare-fn)
  "ALIST is an alist in which the car of each item is the key and the cdr of
each item is a list of things associated with the key.  This function inverts
the alist.  The car of each item in the new list is a member of one of the value
lists in ALIST.  The cdr of each item in the new list is a list of all of those
keys that map to the value that is now the key in the new list.

If COMPARE-FN is non-nil, then the lists in the car of each item in the new list
are sorted using COMPARE-FN.

As an example, the following input:

     A -> (X Y Z)
     B -> (Q X Z)
     C -> (P X)

results in the following output being returned:

     P -> (C)
     Q -> (B)
     X -> (A B C)
     Y -> (A)
     Z -> (A B)"
  (let ((al alist)
        (item)
        (keys)
        (key)
        (values)
        (value)
        (new-item)
        (added-item)
        (new-list)
        (nl))
    (while al
      (setq item (car al))
      (setq key (car item))
      (setq values (cdr item))
      (while values
        (setq value (car values))
        (setq new-item (cons value (list key)))
        (shu-add-to-alist added-item new-item new-list)
        (when (not (eq added-item new-item))
          (setq keys (cdr added-item))
          (push key keys)
          (setcdr added-item keys))
        (setq values (cdr values)))
      (setq al (cdr al)))
    (when compare-fn
      (setq nl new-list)
      (while nl
        (setq item (car nl))
        (setq values (cdr item))
        (setq values (sort values compare-fn))
        (setcdr item values)
        (setq nl (cdr nl))))
    new-list
    ))




;;
;;  shu-split-new-lines
;;
(defun shu-split-new-lines (data &optional separator)
  "Split a string into a list of strings.  If the optional SEPARATOR is present,
it is used as a regular expression that represents the line separator and it is
not retained in each split line.  If SEPARATOR is not present, the separator is
the newline character.  The separator expressions are removed from the
input string and a list of separated strings is returned.

If the input line is an empty string, a list containing one empty string is
returned.  If the line contains a trailing new line character, that trailing new
line character is discarded without generating a new, empty line in the output.

In the following examples SEPARATOR has been set to a comma.

For example, the input string \"Hello,There,\" will return exactly the same
output list as the input string \"Hello,There\".

As another example, the input string \"Hi,How are you?,\" returns a list of
two strings, which are \"Hi\" and \"How are you?\"."
  (let* ((count 0)
         (sep-exp (if separator separator (regexp-opt (list "\n"))))
         (lines (split-string data sep-exp))
         (line)
         (nlines (length lines))
         (new-lines)
         (empty ""))
    (if (= 0 (length data))
        (push empty new-lines)
      (while lines
        (setq count (1+ count))
        (setq line (car lines))
        (when (not (and (= count nlines) (= 0 (length line))))
          (push (concat line) new-lines))
        (setq lines (cdr lines)))
      (setq new-lines (nreverse new-lines)))
    new-lines
    ))



;;
;;  shu-point-at-sexp
;;
(defsubst shu-point-at-sexp (sos)
  "Return the point of the sexp that matches the point at SOS."
  (let ((eos))
    (save-excursion
      (goto-char sos)
      (forward-sexp)
      (setq eos (point)))
    eos
    ))



;;
;;  shu-starts-with
;;
(defun shu-starts-with (regexp)
  "If the first non-whitespace on the current line matches REGEXP, return the position
of the beginning of the mqtched REGEXP.  If the first non-whitespace does not match
REGEXP, return nil."
  (let ((isit))
    (save-excursion
      (goto-char (line-beginning-position))
      (when (re-search-forward shu-not-all-whitespace-regexp (line-end-position) t)
        (backward-char 1)
        (when (looking-at regexp)
          (setq isit (point)))))
    isit
    ))


;;
;;  shu-trim-file-hook
;;
(defun shu-trim-file-hook ()
  "Run as a before-save-hook to trim trailing whitespace from the end of lines and
to trim blank lines from the end of a file if SHU-TRIM-FILE is true."
  (when shu-trim-file
    (delete-trailing-whitespace))
  )



;;
;;  shu-line-number-at-pos
;;
(defun shu-line-number-at-pos (&optional pos absolute)
  "line-number-at-pos in simple.el takes two arguments as of emacs 26.  This
allows the two argument version to run on older versions of emacs.  If
ABSOLUTE is specified, widen the buffer, then call the one argument version
of line-number-at-pos, which is supported in amacs 24 and 25, and perhaps
others."
  (let ((p))
    (save-restriction
      (when absolute
        (widen))
      (setq p (line-number-at-pos pos)))
    p
    ))


;;
;;  shu-replace-string
;;
(defun shu-replace-string (original replacement &optional fixedcase literal)
  "Go to the top of the current buffer and use SEARCH-FORWARD to find all
instances of ORIGINAL, replacing each instance with REPLACEMENT.  The optional
arguments FIXEDCASE and LITERAL are passed through to the REPLACE-MATCH
function.  Return the count of the number of instances that were replaced,
if any."
  (let ((count 0))
    (goto-char (point-min))
    (while (search-forward original nil t)
      (replace-match replacement fixedcase literal)
      (setq count (1+ count)))
    count
    ))



;;
;;  shu-bool-to-string
;;
(defun shu-bool-to-string (arg)
  "Convert a boolean value to its string representation and return the string."
  (let ((sval "nil"))
    (when arg
      (setq sval "t"))
    sval
    ))



;;
;;  shu-upcase-first-letter
;;
(defun shu-upcase-first-letter (string)
  "Return the given string with the first character of the string converted
to upper case"
  (let ((first-letter (substring string 0 1))
        (remainder (substring string 1)))
    (concat (upcase first-letter) remainder)
    ))



;;
;;  shu-downcase-first-letter
;;
(defun shu-downcase-first-letter (string)
  "Return the given string with the first character of the string converted
to lower case"
  (let ((first-letter (substring string 0 1))
        (remainder (substring string 1)))
    (concat (downcase first-letter) remainder)
    ))



;;
;;  shu-random-range
;;
(defun shu-random-range (x y)
  "Return a random number that lies within the closed interval [X, Y].  If Y < X, then the
closed interval is [Y, X].  If Y is equal to X, then the returned value is X."
  (let ((lower x)
        (upper y)
        (range)
        (value)
        (r1))
    (when (< upper lower)
      (setq lower (prog1 upper (setq upper lower))))
    (setq value lower)
    (when (< lower upper)
      (setq range (1+ (- upper lower)))
      (setq r1 (random range))
      (setq value (+ lower r1)))
    value
    ))



;;
;;  shu-random-letter
;;
(defun shu-random-letter ()
  "Return a randomly selected lower case letter as a single character (not as
a string)."
  (let* ((letters "abcdefghijklmnopqrstuvwxyz")
         (nletters (length letters)))
    (elt letters (random nletters))
    ))



;;; shu-base.el ends here
