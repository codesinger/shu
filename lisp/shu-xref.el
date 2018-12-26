;;; shu-xref.el --- Shu project ...
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-xref
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
;;

;;; Commentary:

;;
;; A set of functions that scan a set of elisp files and create a cross reference
;; of all of the definitions (functions, macros, constants, variables, etc.).
;; See the doc string for SHU-MAKE-XREF for further details.

;;; Code:


;; NOTES: Look at variable "obarray"

;;
;;
;; From Section 8.2 of emacs lisp manual:
;;
;; As previously noted, Emacs Lisp allows the same symbol to be defined both as
;; a variable (e.g., with defvar) and as a function or macro (e.g., with
;; defun). Such definitions do not conflict.
;;
;; emacs lisp does this by having a value cell and a function cell associated
;; with each name.  See Section 8.1 (Symbol Components).  We might consider two
;; different lists of names?

;;
;; For each file, we accumulate a list of all of the function, macro, etc.
;; definitions with the line number at which the definition starts.  When we
;; find a reference to a function or variable name in a file, we derive the
;; enclosing function name by looking at the def-list for the file.  We scan the
;; def-list until we find a definition with the smallest, positive difference
;; between the line number of the definition and the line number of the
;; reference.

;;
;;  file-funs-list is a list of file-funs
;;
;;
;;  file-funs
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> def-list for this file
;;        |
;;        +-------------> File name
;;
;;
;;
;;  def-list is a list of file-item.
;;
;;
;;  file-item:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Function name
;;        |
;;        +-------------> Line number
;;
;;
;;
;;
;;
;;  Name-lengths:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Length of longest variable name
;;        |
;;        +-------------> Length of longest type name
;;
;;
;;
;;
;;  Item:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Info
;;        |
;;        +-------------> Name
;;
;;
;;  Info:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Ref
;;        |
;;        +-------------> Def
;;
;;
;;  Def:
;;
;;   -------------------
;;   |        |        |
;;   | Type   | Where  |
;;   |        |        |
;;   -------------------
;;
;;
;;  Where:
;;
;;   -------------------
;;   |        |        |
;;   | File   | Line # |
;;   |        |        |
;;   -------------------
;;
;;
;;  Ref: Is a list of all of the functions that call this one

(require 'shu-base)
(provide 'shu-xref)


(defconst shu-xref-buffer "**shu-xref**"
"The name of the buffer into which the cross reference is placed.")

(defconst shu-xref-lisp-name (regexp-opt
        (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
              "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
              "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
              "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
              "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
              "_" "-" "$") nil)

"A regular expression to match a variable name in emacs lisp.")
(defconst shu-xref-var-types
  '(("un"     . 1)
    ("macro"  . 2)
    ("subst"  . 3)
    ("alias"  . 4)
    ("var"    . 5)
    ("const"  . 6)
    ("custom" . 7)
    ("group"  . 8))
  "Associate a number with each type of variable")


;;
;;  shu-make-xref
;;
(defun shu-make-xref (start end)
  "Mark a region in a file that contains one name per line of an emacs lisp file
Then invoke shu-make-xref.  It will do a cross reference of all of those files."
  (interactive "r")                 ; bounds of marked region (start end)
  (let ((file-list)
        (fun-defs)
        (max-type-name-length)
        (max-var-name-length)
        (retval))
    (setq file-list (shu-xref-get-file-list start end file-list))

    (setq fun-defs (shu-xref-get-defs file-list fun-defs))

    (setq retval (shu-xref-get-longest-name fun-defs))
    (setq max-type-name-length (car retval))
    (setq max-var-name-length (cdr retval))

    ;; Sort by variable name and dump the list
    (setq fun-defs (sort fun-defs (lambda(t1 t2) (string< (car t1) (car t2)))))
    (shu-xref-dump fun-defs max-var-name-length max-type-name-length)

    ;; Sort by variable type and dump the list
    (setq fun-defs (sort fun-defs 'shu-xref-type-compare))
    (shu-xref-dump fun-defs max-var-name-length max-type-name-length)

    ;; Sort by file name and dump the list
    (setq fun-defs (sort fun-defs 'shu-xref-file-compare))
    (shu-xref-dump fun-defs max-var-name-length max-type-name-length)
    (switch-to-buffer shu-xref-buffer)
    (goto-char (point-min))
    ))


;;
;;  shu-xref-get-defs
;;
(defun shu-xref-get-defs (file-list fun-defs)
  "Extract the variable definitions from each file."
  (let (
    (tlist file-list)
    (file-name)
    (fun-defs)
       )
    (while tlist
        (setq file-name (car tlist))
        (find-file file-name)
        (setq fun-defs (shu-get-all-definitions fun-defs))
        (kill-buffer (current-buffer))
        (setq tlist (cdr tlist)))
  fun-defs
))

;;
;;  shu-xref-get-file-list
;;
(defun shu-xref-get-file-list (start end file-list)
  "Return a list of file names from a region of a buffer.  START and END
define the region.  Each line in the region is assumed to be a file name.
FILE-LIST is the list that is also the return value of this function."
  (interactive "r")                 ; bounds of marked region (start end)
  (let (
    (eline (shu-the-line-at end))
    (eol)
    (file-name)
    (line-diff 0)
       )
  (save-excursion
    (goto-char start)               ; Move to region start
                                    ; While we have not reached last line and
    (while (and (<= (shu-current-line) eline) (= line-diff 0)) ; there are more lines
      (setq eol (save-excursion (end-of-line) (point)))
      (when (> eol (point))
          (setq file-name (buffer-substring (point) eol)))
          (setq file-list (cons file-name file-list))
      (setq line-diff (forward-line 1)))
    (setq file-list (nreverse file-list)))
    file-list
))


;;
;;  shu-xref-type-compare
;;
(defun shu-xref-type-compare (t1 t2)
  "Compare the type names from two variable names.  Return t if the type
name in T1 comes before the type name in T2.  If the type names are the same,
then compare the variable names so that variables are in alphabetical order
within type."
  (let
   ((fun-name1)
    (info1)
    (def1)
    (type1)
    (type-name1)
    (fun-name2)
    (info2)
    (def2)
    (type2)
    (type-name2)
    (is-less))
  (setq fun-name1 (car t1))
  (setq info1 (cdr t1))
  (setq def1 (car info1))
  (setq type1 (car def1))
  (setq type-name1 (concat "def" (car (rassoc type1 shu-xref-var-types))))
  (setq fun-name2 (car t2))
  (setq info2 (cdr t2))
  (setq def2 (car info2))
  (setq type2 (car def2))
  (setq type-name2 (concat "def" (car (rassoc type2 shu-xref-var-types))))
  (if (string< type-name1 type-name2)
      (setq is-less t)
      (when (string= type-name1 type-name2)
          (when (string< fun-name1 fun-name2)
              (setq is-less t))))
  is-less
))

;;
;;  shu-xref-file-compare
;;
(defun shu-xref-file-compare (t1 t2)
  "Compare the file names from two variable names.  Return t if the file
name in T1 comes before the type name in T2.  If the file names are the same,
then compare the variable names so that variables are in alphabetical order
within file."
  (let
     ((def1)
      (file-name1)
      (fun-name1)
      (info1)
      (is-less)
      (line-no1)
      (type-name1)
      (type1)
      (where1)
      (def2)
      (file-name2)
      (fun-name2)
      (info2)
      (is-less)
      (line-no2)
      (type-name2)
      (type2)
      (where2))
  (setq fun-name1 (car t1))
  (setq info1 (cdr t1))
  (setq def1 (car info1))
  (setq type1 (car def1))
  (setq type-name1 (concat "def" (car (rassoc type1 shu-xref-var-types))))
  (setq where1 (cdr def1))
  (setq file-name1 (car where1))
  (setq line-no1 (cdr where1))
  (setq fun-name2 (car t2))
  (setq info2 (cdr t2))
  (setq def2 (car info2))
  (setq type2 (car def2))
  (setq type-name2 (concat "def" (car (rassoc type2 shu-xref-var-types))))
  (setq where2 (cdr def2))
  (setq file-name2 (car where2))
  (setq line-no2 (cdr where2))
  (if (string< file-name1 file-name2)
      (setq is-less t)
      (when (string= file-name1 file-name2)
          (when (string< fun-name1 fun-name2)
              (setq is-less t))))
  is-less
))

;;
;;  shu-xref-get-longest-name
;;
(defun shu-xref-get-longest-name (fun-defs)
  " Return the length of the longest variable name in the list and the longest type
name in the list.  These are returned as a cons cell with the length of the longest
type name in the CAR and the longest variable name in the CDR."
  (let
   ((tlist fun-defs)
    (max-var-name-length 0)
    (max-type-name-length 0)
    (item)
    (fun-name)
    (info)
    (def)
    (type)
    (type-name)
    (retval))
  (while tlist
      (setq item (car tlist))
      (setq fun-name (car item))
      (setq info (cdr item))
      (setq def (car info))
      (setq type (car def))
      (setq type-name (concat "def" (car (rassoc type shu-xref-var-types))))
      (when (> (length fun-name) max-var-name-length)
          (setq max-var-name-length (length fun-name)))
      (when (> (length type-name) max-type-name-length)
          (setq max-type-name-length (length type-name)))
      (setq tlist (cdr tlist)))
  (setq retval (cons max-type-name-length max-var-name-length))
  retval
))

;;
;;  shu-xref-dump
;;
(defun shu-xref-dump (fun-defs max-var-name-length max-type-name-length)
  (let
   ((tlist fun-defs)
    (item)
    (fun-name)
    (type)
    (type-name)
    (info)
    (def)
    (where)
    (file-name)
    (line-no)
    (pad-count)
    (pad)
    (gbuf (get-buffer-create shu-xref-buffer)))
  (princ "\n================================================================\n" gbuf)
  (while tlist
      (setq item (car tlist))
      (setq fun-name (car item))
      (setq info (cdr item))
      (setq def (car info))
      (setq type (car def))
      (setq type-name (concat "def" (car (rassoc type shu-xref-var-types))))
      (setq where (cdr def))
      (setq file-name (car where))
      (setq line-no (cdr where))
      (setq pad-count (1+ (- max-var-name-length (length fun-name))))
      (setq pad (make-string pad-count ? ))
      (setq fun-name (concat fun-name pad))
      (setq pad-count (1+ (- max-type-name-length (length type-name))))
      (setq pad (make-string pad-count ? ))
      (setq type-name (concat type-name pad))
      (princ (format "%s %s: %s: %d\n" fun-name type-name file-name line-no) gbuf)
      (setq tlist (cdr tlist)))
))
; (setq pad (make-string pad-count ? ))
;  (setq type (concat "def" (car (rassoc 2 shu-xref-var-types))))

;;
;;  shu-get-all-definitions
;;
;; This returns a list with the following information in it for each function
;;
;;  Item:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Info
;;        |
;;        +-------------> Name
;;
;;
;;  Info:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |  nil   |
;;   |    |   |        |
;;   -----|-------------
;;        |
;;        |
;;        |
;;        +-------------> Def
;;
;;
;;  Def:
;;
;;   -------------------
;;   |        |        |
;;   | Type   | Where  |
;;   |        |        |
;;   -------------------
;;
;;
;;  Where:
;;
;;   -------------------
;;   |        |        |
;;   | File   | Line # |
;;   |        |        |
;;   -------------------
;;
(defun shu-get-all-definitions (fun-defs)
  "Find all of the emacs lisp function definitions in the current buffer."
  (let
   ((fun-extract)
     (name "def\\(un\\|macro\\|subst\\|alias\\|var\\|const\\|custom\\|group\\)")
;    (name "defun")
    (fun-name)
    (info)
    (line-no)
    (var-type-number)
    (where)
    (def)
    (gbuf (get-buffer-create shu-xref-buffer))
    (info)
    (item))
  (save-excursion (save-restriction
      (widen)
      (goto-char (point-min))
      (setq fun-extract (shu-xref-get-next-definition fun-extract))
      (while fun-extract
          (setq fun-name (car fun-extract))
          (setq info (cdr fun-extract))
          (setq line-no (cdr info))
          (setq var-type-number (car info))
          (setq where (cons (buffer-file-name) line-no))
          (setq def (cons var-type-number where))
          (setq info (cons def nil))
          (setq item (cons fun-name info))
          (setq fun-defs (cons item fun-defs))
          (setq fun-extract (shu-xref-get-next-definition fun-extract)))))
  (setq fun-defs (nreverse fun-defs))
  fun-defs
))


;;
;;  shu-xref-get-next-definition
;;
;;
;;  Retval:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Info
;;        |
;;        +-------------> Name
;;
;;
;;  Info:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Line number
;;        |
;;        +-------------> Type number
;;
;;
(defun shu-xref-get-next-definition (retval)
  "Find and return the next definition of an emacs lisp function of variable.
   RETVAL is returned
as nil if there are no more function definitions after point.  If a definition
is found, RETVAL is returned as a cons cell with the name of the function
in the CAR and the information about the function in the CDR.  The information in the
CDR is a cons cell with the numeric variable type in the CAR and the line number in
which the definition started in the CDR."
  (let*
   ((name "def\\(un\\|macro\\|subst\\|alias\\|var\\|const\\|custom\\|group\\)")
   (sdefun (concat "\(" shu-all-whitespace-regexp "*" name shu-all-whitespace-regexp "*" "\\(" shu-xref-lisp-name "+\\)"))
    (retval)
    (x)
    (something t)
    (fun-name)
    (fun-start-point)
    (fun-start-line)
    (fun-end-point)
    (info)
    (var-type-number)
    (var-type-name))
  (while something
      (setq fun-end-point (re-search-forward sdefun nil t))
      (if (not (numberp fun-end-point))
          (setq something nil)
      ;;
          (setq var-type-name (match-string 1))
          (setq fun-name (match-string 2))
          (save-excursion
              (re-search-backward sdefun nil t)
              (setq fun-start-point (point))
              (setq fun-start-line (shu-the-line-at (point)))
              (when (not (shu-point-in-string))
                  (beginning-of-line)
                  (when (not (re-search-forward ";" fun-start-point t))
                      (setq something nil)
                      (setq var-type-number (cdr (assoc var-type-name shu-xref-var-types)))
                      (setq info (cons var-type-number fun-start-line))
                      (setq retval (cons fun-name info)))))))
  retval
))



;;
;;  shu-xref-get-next-funcall
;;
(defun shu-xref-get-next-funcall (name retval)
  "Find and return the next call to the emacs lisp function NAME.  RETVAL is returned
as nil if there are no more function invocations after point.  If a function
invocation is found, RETVAL is returned as a cons cell with the name of the function
in the CAR and the line number in which the function definition starts in the CDR."
  (let
   ((sdefun (concat "\(" shu-all-whitespace-regexp "*" name shu-all-whitespace-regexp "*" "\\(" shu-xref-lisp-name "+\\)"))
    (retval)
    (x)
    (something t)
    (fun-name)
    (fun-start-point)
 (gbuf (get-buffer-create "**zzlp**"))
    (fun-start-line)
    (fun-end-point))
  (while something
      (setq fun-end-point (re-search-forward sdefun nil t))
      (if (not (numberp fun-end-point))
          (setq something nil)
      ;;
          (setq fun-name (match-string 1))
          (save-excursion
              (re-search-backward sdefun nil t)
              (setq fun-start-point (point))
              (setq fun-start-line (shu-the-line-at (point)))
              (when (not (shu-point-in-string))
                  (beginning-of-line)
                  (when (not (re-search-forward ";" fun-start-point t))
                      (setq something nil)
                      (setq retval (cons fun-name fun-start-line)))))))
  retval
))

;;; shu-xref.el ends here
