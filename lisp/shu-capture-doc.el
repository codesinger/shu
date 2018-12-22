;;; shu-capture-doc.el --- Shu project code to capture function doc strings
;;
;; Copyright (C) 2018 Stewart L. Palmer
;;
;; Package: shu-capture-doc
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

;;; Commentary:

;;  Collection of functions used to capture doc strings in elisp functions

;;; Code

;; NB: (intern-soft string) will return non-nil if string holds a symbol name


(provide 'shu-capture-doc)
(require 'subr-x)


;; Information about a function and its doc string is contained in the folllwing
;; three cons cells.  The macros immmediately below can create the cons cells from
;; the information and extract the information from the cons cells.
;;
;;
;;
;;
;;      func-def:
;;
;;       -------------------
;;       |        |        |
;;       |    o   |   o    |
;;       |    |   |   |    |
;;       -----|-------|-----
;;            |       |
;;            |       +-----> func-alias
;;            |
;;            +-------------> signature
;;
;;
;;      func-alias
;;
;;       -------------------
;;       |        |        |
;;       |    o   |   o    |
;;       |    |   |   |    |
;;       -----|-------|-----
;;            |       |
;;            |       +-----> func-info
;;            |
;;            +-------------> alias
;;
;;
;;      func-info:
;;
;;       -------------------
;;       |        |        |
;;       |    o   |   o    |
;;       |    |   |   |    |
;;       -----|-------|-----
;;            |       |
;;            |       +-----> description
;;            |
;;            +-------------> attributes
;;

;;
;;  shu-capture-set-func-def
;;
(defmacro shu-capture-set-func-def (func-def signature attributes description)
  "Create a func-def to describe the function"
  (let ((tfunc-alias (make-symbol "func-alias"))
        (tfunc-info (make-symbol "func-info")))
  `(let ((,tfunc-alias)
         (,tfunc-info))
     (setq ,tfunc-info (cons ,attributes ,description))
     (setq ,tfunc-alias (cons nil ,tfunc-info))
     (setq ,func-def (cons ,signature ,tfunc-alias)))
     ))


;;
;;  shu-capture-set-func-def-alias
;;
(defmacro shu-capture-set-func-def-alias (func-def signature attributes description alias)
  "Create a func-def to describe the function"
  `(let ((func-alias)
         (func-info))
     (setq func-info (cons ,attributes ,description))
     (setq func-alias (cons ,alias func-info))
     (setq ,func-def (cons ,signature func-alias))
     ))


;;
;;  shu-capture-get-func-def
;;
(defmacro shu-capture-get-func-def (func-def signature attributes description alias)
  "Extract the information from the func-def"
  (let ((tfunc-alias (make-symbol "func-alias"))
        (tfunc-info (make-symbol "func-info")))
    `(let ((,tfunc-alias)
           (,tfunc-info))
       (setq ,signature (car ,func-def))
       (setq ,tfunc-alias (cdr ,func-def))
       (setq ,alias (car ,tfunc-alias))
       (setq ,tfunc-info (cdr ,tfunc-alias))
       (setq ,attributes (car ,tfunc-info))
       (setq ,description (cdr ,tfunc-info)))
    ))


;;
;;  shu-capture-get-func-def-sig
;;
(defmacro shu-capture-get-func-def-sig (func-def signature)
  "Extract the function signature from the func-def"
  `(let ()
     (setq ,signature (car ,func-def))
     ))


;;
;;  shu-capture-get-func-def-alias
;;
(defmacro shu-capture-get-func-def-alias (func-def alias)
  "Extract the function alias from the func-def"
  (let ((tfunc-alias (make-symbol "func-alias")))
  `(let ((,tfunc-alias))
     (setq ,tfunc-alias (car ,func-def))
     (setq ,alias (car ,tfunc-alias)))
     ))



;;
;;  shu-capture-get-name-and-args
;;
(defmacro shu-capture-get-name-and-args (signature func-name args)
  "Extract the function and the string of arguments from a whole signature that
includes both the function name and the arguments.  If SIGNATURE contains:

     \"do-something (to something)\"

The on return FUNC-NAME will hold \"do-something\" and ARGS will contain the
string \"to something)\".  If there are no arguments, ARGS will contain a string
of length zero.  If there is no function name, FUNC-NAME will contain a string
of length zero"
  (let ((tfs (make-symbol "fs")))
  `(let ((,tfs   "\\s-*\\([0-9a-zA-Z-]+\\)\\s-*(\\s-*\\([ 0-9a-zA-Z-,&\n]*\\))"))
     (if (string-match ,tfs ,signature)
         (progn
           (setq ,func-name (match-string 1 ,signature))
           (setq ,args (string-trim (match-string 2 ,signature))))
       (setq ,func-name "")
       (setq ,args "")))
     ))


;;
;;  shu-capture-alias-list
;;
(defvar shu-capture-alias-list
  "The alist that holds all of the alias names.")


;;
;;  shu-capture-a-type-hdr
;;
(defconst shu-capture-a-type-hdr 1
  "The a-list key value that identifies the function that emits section headers")


;;
;;  shu-capture-a-type-func
;;
(defconst shu-capture-a-type-func 2
  "The a-list key value that identifies the function that formats a fundtion signature'")


;;
;;  shu-capture-a-type-buf
;;
(defconst shu-capture-a-type-buf 3
  "The a-list key value that identifies the function that converts a buffer name or
other name that begins and ends with asterisks to markup.")


;;
;;  shu-capture-a-type-arg
;;
(defconst shu-capture-a-type-arg 4
  "The a-list key value that identifies the function that converts an argument name
to markup.")


;;
;;  shu-capture-a-type-keywd
;;
(defconst shu-capture-a-type-keywd 5
  "The a-list key value that identifies the function that converts a key word, such
as \"&optional\" or \"&rest\" to markup.")


;;
;;  shu-capture-pre-code-in-doc
;;
(defconst shu-capture-pre-code-in-doc 6
  "The a-list key value that identifies the function that converts characters in a
doc string right before the code snippets are captured.")

;;
;;  shu-capture-a-type-doc-string
;;
(defconst shu-capture-a-type-doc-string 7
  "The a-list key value that identifies the function that converts a key word, such
as \"&optional\" or \"&rest\" to markup.")


;;
;;  shu-capture-a-type-before
;;
(defconst shu-capture-a-type-before 8
  "The a-list key value that identifies the string that is placed before a verbatim
code snippet.")


;;
;;  shu-capture-a-type-after
;;
(defconst shu-capture-a-type-after 9
  "The a-list key value that identifies the string that is placed after a verbatim
code snippet.")


;;
;;  shu-capture-a-type-open-quote
;;
(defconst shu-capture-a-type-open-quote 10
  "The a-list key value that identifies the string that is an open quote.")


;;
;;  shu-capture-a-type-close-quote
;;
(defconst shu-capture-a-type-close-quote 11
  "The a-list key value that identifies the string that is a close quote.")


;;
;;  shu-capture-keywd-optional
;;
(defconst shu-capture-keywd-optional "&optional"
  "The argument list keyword for an optional argument.")


;;
;;  shu-capture-keywd-rest
;;
(defconst shu-capture-keywd-rest "&rest"
  "The argument list keyword for a multiple optional arguments.")


;;
;;  shu-capture-md-section-delimiter
;;
(defconst shu-capture-md-section-delimiter "##"
  "Define the markdown delimiter that is used to identfy a section.  This is se[arated
from the section name by a space.")


;;
;;  shu-capture-latex-section-start
;;
(defconst shu-capture-latex-section-start "\\subsection{"
  "Define the LaTex tag that is used to identfy the start of a section headig.")


;;
;;  shu-capture-latex-section-end
;;
(defconst shu-capture-latex-section-end "}"
  "Define the LaTex tag that is used to identfy the start of a section headig.")


;;
;;  shu-capture-md-arg-delimiter
;;
(defconst shu-capture-md-arg-delimiter "*"
  "Define the markdown delimiter that is used to surround an argument name.")


;;
;;  shu-capture-md-keywd-delimiter
;;
(defconst shu-capture-md-keywd-delimiter "**"
  "Define the markdown delimiter that is used to surround an key word such as
\"&optional\" or \"&rest\".")


;;
;;  shu-capture-latex-arg-start
;;
(defconst shu-capture-latex-arg-start "\\emph{"
  "Define the latex string that is used to prepended to an arguument name.")


;;
;;  shu-capture-latex-arg-end
;;
(defconst shu-capture-latex-arg-end "}"
  "Define the latex string that is used to terminate an arguument name.")


;;
;;  shu-capture-latex-keywd-start
;;
(defconst shu-capture-latex-keywd-start "\\textbf{"
  "Define the latex string that is used to prepended to an arguument name.")


;;
;;  shu-capture-latex-keywd-end
;;
(defconst shu-capture-latex-keywd-end "}"
  "Define the latex string that is used to terminate an arguument name.")


;;
;;  shu-capture-md-buf-delimiter
;;
(defconst shu-capture-md-buf-delimiter "`"
  "Define the markdown delimiter that is used to surround a buffer name or
any other name that has leading and trailing asterisks")


;;
;;  shu-capture-latex-buf-start
;;
(defconst shu-capture-latex-buf-start "\\emph{"
  "Define the LaTex string that is used in front of a buffer name or
any other name that has leading and trailing asterisks")


;;
;;  shu-capture-latex-buf-end
;;
(defconst shu-capture-latex-buf-end "}"
  "Define the LaTex string that is used at the end of a buffer name or
any other name that has leading and trailing asterisks")


;;
;;  shu-capture-md-code-delimiter
;;
(defconst shu-capture-md-code-delimiter "```"
  "Define the markdown delimiter that is used to surround a code snippet.")


;;
;;  shu-capture-latex-code-start
;;
(defconst shu-capture-latex-code-start "\\begin{verbatim}"
  "Define the LaTex string that is at the beginning of a verbatim
code snippet.")


;;
;;  shu-capture-latex-code-end
;;
(defconst shu-capture-latex-code-end "\\end{verbatim}"
  "Define the LaTex string that is at the end of a verbatim
code snippet.")


;;
;;  shu-capture-md-quote-delimiter
;;
(defconst shu-capture-md-quote-delimiter "\""
  "Define the markdown delimiter that is used for open and close quote.")


;;
;;  shu-capture-latex-open-quote
;;
(defconst shu-capture-latex-open-quote "``"
  "Define the LaTex string that is an open quote.")


;;
;;  shu-capture-latex-close-quote
;;
(defconst shu-capture-latex-close-quote "''"
  "Define the LaTex string that is a close quote.")


;;
;;  shu-capture-latex-doc-start
;;
(defconst shu-capture-latex-doc-start "\\begin{doc-string}"
  "Define the LaTex string that starts a doc string.")


;;
;;  shu-capture-latex-doc-end
;;
(defconst shu-capture-latex-doc-end "\\end{doc-string}"
  "Define the LaTex string that ends a doc string.")


;;
;;  shu-capture-attr-inter
;;
(defconst shu-capture-attr-inter (lsh 1 0)
  "Bit that indicates that a function is interactive")


;;
;;  shu-capture-attr-alias
;;
(defconst shu-capture-attr-alias (lsh 1 1)
  "Bit that indicates that a function is identified by its alias name")


;;
;;  shu-capture-attr-macro
;;
(defconst shu-capture-attr-macro (lsh 1 2)
  "Bit that indicates that a function is a macro")


;;
;;  shu-capture-attr-const
;;
(defconst shu-capture-attr-const (lsh 1 3)
  "Bit that indicates that a definition is a defconst")


;;
;;  shu-capture-attr-var
;;
(defvar shu-capture-attr-var (lsh 1 4)
  "Bit that indicates that a definition is a defvar")



;;
;;  shu-capture-doc-code-indent
;;
(defconst shu-capture-doc-code-indent 4
  "Any line indented by this much in a doc stringis assumed to be a sample
code snippet.")



;;
;;  shu-capture-buffer-name
;;
(defconst shu-capture-buffer-name "**shu-capture-doc**"
  "Name of the buffer into which the converted documentation is written")



;;
;;  shu-capture-index-buffer
;;
(defconst shu-capture-index-buffer "**shu-capture-index**"
  "Name of the buffer into which the markdown index is written")



;;
;;  shu-capture-md-converters
;;
(defconst shu-capture-md-converters
  (list
   (cons shu-capture-a-type-hdr          'shu-capture-make-md-section)
   (cons shu-capture-a-type-func         'shu-capture-convert-func-md)
   (cons shu-capture-a-type-buf          'shu-capture-buf-to-md)
   (cons shu-capture-a-type-arg          'shu-capture-arg-to-md)
   (cons shu-capture-a-type-keywd        'shu-capture-keywd-to-md)
   (cons shu-capture-pre-code-in-doc     'shu-capture-pre-code-md)
   (cons shu-capture-a-type-doc-string   'shu-capture-finish-doc-string-md)
   (cons shu-capture-a-type-before       shu-capture-md-code-delimiter)
   (cons shu-capture-a-type-after        shu-capture-md-code-delimiter)
   (cons shu-capture-a-type-open-quote   shu-capture-md-quote-delimiter)
   (cons shu-capture-a-type-close-quote  shu-capture-md-quote-delimiter))
  "This is the association list of functions and strings that is used to take an elisp
function and its associated doc string and convert it to markdown.")



;;
;;  shu-capture-latex-converters
;;
(defconst shu-capture-latex-converters
  (list
    (cons shu-capture-a-type-hdr          'shu-capture-make-latex-section)
    (cons shu-capture-a-type-func         'shu-capture-convert-func-latex)
    (cons shu-capture-a-type-buf          'shu-capture-buf-to-latex)
    (cons shu-capture-a-type-arg          'shu-capture-arg-to-latex)
    (cons shu-capture-a-type-keywd        'shu-capture-keywd-to-latex)
    (cons shu-capture-pre-code-in-doc     'shu-capture-pre-code-latex)
    (cons shu-capture-a-type-doc-string   'shu-capture-finish-doc-string-latex)
    (cons shu-capture-a-type-before       shu-capture-latex-code-start)
    (cons shu-capture-a-type-after        shu-capture-latex-code-end)
    (cons shu-capture-a-type-open-quote   shu-capture-latex-open-quote)
    (cons shu-capture-a-type-close-quote  shu-capture-latex-close-quote))
  "This is the association list of functions and strings that is used to take an elisp
function and its associated doc string and convert it to LaTex.")



;;
;;  shu-capture-make-md-section
;;
(defun shu-capture-make-md-section (level hdr)
  "Turn HDR into a markdown section header of level LEVEL, where 1 is a section,
2 a subsection, etc.  Return the markdown string."
  (let* ((delim (make-string level ?#))
         (header
          (concat delim
                  " "
                  hdr
                  " "
                  delim)))
    header
    ))



;;
;;  shu-capture-make-latex-section
;;
(defun shu-capture-make-latex-section (level hdr)
  "Turn HDR into a LaTex section header of level LEVEL, where 1 is a section,
2 a subsection, etc.  Return the LaTex string."
  (let (
        (x (1- level))
        (base "section{")
        (prefix "")
        (header)
        (start)
    )
    (while (> x 0)
      (setq prefix (concat "sub" prefix))
      (setq x (1- x))
      )
    (setq start (concat "\\" prefix base))
    (setq header (concat start hdr shu-capture-latex-section-end))
    header
    ))



;;
;;  shu-capture-arg-to-md
;;
(defun shu-capture-arg-to-md (arg-name)
  "Convert a function argument in a doc-string or argument list to markdown."
  (concat shu-capture-md-arg-delimiter arg-name shu-capture-md-arg-delimiter)
  )



;;
;;  shu-capture-arg-to-latex
;;
(defun shu-capture-arg-to-latex (arg-name)
  "Convert a function argument in a doc-string or argument list to LaTex."
  (let ((result (concat shu-capture-latex-arg-start arg-name shu-capture-latex-arg-end)))
  result
    ))



;;
;;  shu-capture-keywd-to-md
;;
(defun shu-capture-keywd-to-md (arg-name)
  "Convert a function argument key word in a doc-string or argument list
to markdown."
  (concat shu-capture-md-keywd-delimiter arg-name shu-capture-md-keywd-delimiter)
  )



;;
;;  shu-capture-keywd-to-latex
;;
(defun shu-capture-keywd-to-latex (keywd-name)
  "Convert a function argument key word in a doc-string or argument list
to LaTex."
  (let ((prefix "")
        (name))
    (when (string= "&" (substring keywd-name 0 1))
      (setq prefix "\\"))
    (setq name (concat prefix keywd-name))
    (concat shu-capture-latex-keywd-start name shu-capture-latex-keywd-end)
    ))



;;
;;  shu-capture-buf-to-md
;;
(defun shu-capture-buf-to-md (buf-name)
  "Convert a buffer name or other name that starts and ends with asterisks
 in a doc-string to markdown."
  (concat shu-capture-md-buf-delimiter buf-name shu-capture-md-buf-delimiter)
  )



;;
;;  shu-capture-buf-to-latex
;;
(defun shu-capture-buf-to-latex (buf-name)
  "Convert a buffer name or other name that starts and ends with asterisks
 in a doc-string to markdown."
  (concat shu-capture-latex-buf-start buf-name shu-capture-latex-buf-end)
  )



;;
;;  shu-capture-pre-code-md
;;
(defun shu-capture-pre-code-md ()
  "Function that prepares a doc string to capture code snippets in markdown."
    )



;;
;;  shu-capture-pre-code-latex
;;
(defun shu-capture-pre-code-latex ()
  "Function that prepares a doc string to capture code snippets in LaTex."
    (goto-char (point-min))
    (while (search-forward "{" nil t)
      (replace-match "\\{" t t))
    (goto-char (point-min))
    (while (search-forward "}" nil t)
      (replace-match "\\}" t t))
    (goto-char (point-min))
    (while (search-forward "_" nil t)
      (replace-match "\\_" t t))
    )



;;
;;  shu-capture-finish-doc-string-md
;;
(defun shu-capture-finish-doc-string-md ()
  "Function that exeacutes last step in the conversion of a doc-string to
markdown."
    )



;;
;;  shu-capture-finish-doc-string-latex
;;
(defun shu-capture-finish-doc-string-latex ()
  "Function that exeacutes last step in the conversion of a doc-string to
markdown."
  (interactive)
  (let ((start (concat shu-capture-latex-doc-start "\n"))
        (end (concat "\n" shu-capture-latex-doc-end)))
    (goto-char (point-min))
    (while (search-forward "&" nil t)
      (replace-match "\\&" t t))
    (goto-char (point-min))
    (insert start)
    (goto-char (point-max))
    (insert end)
    ))




;;
;;  shu-capture-md
;;
(defun shu-capture-md ()
  "Capture all of the function and macro definitions in a .el source file and tuem
them into markdown text that documents the functions and their doc strings."
  (interactive)
    (shu-capture-doc shu-capture-md-converters)
    )



;;
;;  shu-capture-latex
;;
(defun shu-capture-latex ()
  "Capture all of the function and macro definitions in a .el source file and tuem
them into a LaTex text that documents the functions and their doc strings."
  (interactive)
    (shu-capture-doc shu-capture-latex-converters)
    )



;;
;;  shu-capture-doc
;;
(defun shu-capture-doc (converters)
  "Top level function that captures all definitions and doc strings in a language
neutral manner and then uses the supplied CONVERTERS to convert the documentation to
either markdown or LaTex."
  (let (
        (gb (get-buffer-create shu-capture-buffer-name))
        (section-converter (cdr (assoc shu-capture-a-type-hdr converters)))
        (af-lists)
        (alias-list)
        (func-list)
        (sec-hdr)
        )
    (setq af-lists (shu-capture-internal-doc))
    (setq alias-list (car af-lists))
    (setq func-list (cdr af-lists))
    (setq func-list (shu-capture-vars func-list))
    (setq alias-list (sort alias-list 'shu-doc-sort-compare))
    (when (/= 0 (length alias-list))
      (setq sec-hdr (funcall section-converter 2 "List of functions by alias name"))
      (princ (concat "\n\n" sec-hdr "\n\n") gb)
      (princ "A list of aliases and associted function names.\n\n" gb)
      (shu-capture-show-list alias-list converters gb))
    (setq func-list (sort func-list 'shu-doc-sort-compare))
    (when (/= 0 (length func-list))
      (setq sec-hdr (funcall section-converter 2 "List of functions"))
      (princ (concat "\n\n" sec-hdr "\n\n") gb)
      (princ "A list of functions in this package.\n\n" gb)
      (shu-capture-show-list func-list converters gb))
    ))



;;
;;  shu-capture-internal-doc
;;
(defun shu-capture-internal-doc ()
  "Function that captures documentation for all instances of \"defun,\" \"defsubst,\"
and \"defmacro.\""
  (let (
        (ggb (get-buffer-create "**slp**"))
        (gb (get-buffer-create shu-capture-buffer-name))
        (ss
         (concat
          "("
          "\\s-*"
          "\\(defun\\|defsubst\\|defmacro\\)\\"
          "s-+"))
        (fs
         (concat
          "("
          "\\s-*"
          "\\(defun\\|defsubst\\|defmacro\\)"
          "\\s-*"
          "\\([0-9a-zA-Z-]+\\)"
          "\\s-*"
          "("
          "\\s-*"
          "\\([ 0-9a-zA-Z-,&\n]*\\)"
          ")"))
        (inter  "(interactive")
        (attributes 0)
        (sof 0)
        (eof 0)
        (fn)
        (ftype)
        (args)
        (desc)
        (func-sig)
        (sig)
        (func-info)
        (func-def)
        (alias-def)
        (func-list)
        (alias-list)
        (alias)
        (al)
        (xquote "^\\\"\\|[^\\]\\\"") ;; Match either a quote at the beginning
        (al)
        (sec-hdr)
        )
    (shu-capture-aliases)
    (goto-char (point-min))
    (while (re-search-forward ss nil t)
      (princ (format "Found defun: %s\n" (match-string 0)) ggb)
      (setq attributes 0)
      (beginning-of-line)
      (setq sof (point))
      (princ (format "sof: %d\n" sof ) ggb)
      (setq eof (save-excursion
                  (forward-sexp)
                  (point)))
      (when (search-forward inter eof t)
        (setq attributes (logior attributes shu-capture-attr-inter)))
      (goto-char sof)
      (if (re-search-forward fs eof t)
          (progn
            (setq ftype (match-string 1))
            (setq fn (match-string 2))
            (setq args (match-string 3))
            (setq func-sig (concat fn " (" args ")"))
            (princ (format "FUNC: %s\n" func-sig) ggb)
            (setq desc (shu-capture-get-doc-string eof))
            (when (string= ftype "defmacro")
              (setq attributes (logior attributes shu-capture-attr-macro)))
            (setq alias nil)
            (setq al nil)
            (setq alias (assoc fn shu-capture-alias-list))
            (when alias
              (setq al (cdr alias))
              (setq sig (concat al " (" args ")"))
              (setq attributes (logior attributes shu-capture-attr-alias))
              (shu-capture-set-func-def-alias alias-def sig attributes desc fn)
              (setq alias-list (cons alias-def alias-list))
              )
            (shu-capture-set-func-def-alias func-def func-sig attributes desc al)
            (setq func-list (cons func-def func-list))
            )
        (princ (format "\nERROR: At point %d, found \"defun\" but no function name\n\n" sof) gb)
        (goto-char eof)
        )
      )
    (cons alias-list func-list)
    ))




;;
;;  shu-capture-vars
;;
(defun shu-capture-vars (func-list)
  "Find the name and doc-string for instances of \"defvar\" or \"defconst.\""
  (let (
        (ss
         (concat
          "("
          "\\s-*"
          "\\(defvar\\|defconst\\)"
          "\\s-*"
          "\\([0-9a-zA-Z-]+\\)"
          ))
        (eov)
        (name)
        (type)
        (bod)
        (eod)
        (pc)
        (zzz)
        (desc)
        (attributes)
        (signature)
        (func-def)
        )
    (goto-char (point-min))
    (while (re-search-forward ss nil t)
      (setq zzz t)
      (setq desc nil)
      (setq type (match-string 1))
      (setq attributes shu-capture-attr-const)
      (if (string= type "defvar")
          (setq attributes shu-capture-attr-var)
        (when (string= type "defconst")
          (setq attributes shu-capture-attr-const)
          ))
      (setq name (match-string 2))
      (setq eov (point))
      (goto-char (match-beginning 0))
      (forward-sexp)
      (when (search-backward "\"" eov t)
        (forward-char -1)
        (setq eod (1+ (point)))
        (while zzz
          (when (search-backward "\"" eov t)
            (setq bod (1+ (point)))
            (setq pc (buffer-substring-no-properties (1- (point)) (point)))
            (when (not (string= pc "\\"))
              (setq zzz nil)
              (setq bod (1+ (point)))
              )
            )
          (setq desc (buffer-substring-no-properties bod eod))
          )
        )
      (setq signature (concat name " ()"))
      (shu-capture-set-func-def func-def signature attributes desc)
      (setq func-list (cons func-def func-list))
      )
    func-list
    ))



;;
;;  shu-capture-get-doc-string
;;
(defun shu-capture-get-doc-string (eof)
  "Enter with point positioned immediately after a function declaration.  Try to
fetch the associatd doc string as follows:  1. Look for the first open or close
[arenthesis.  2. Look for the first quote.  If the first parenthesis comes before
the first quote, then there is no doc string.  In the following function, there is
no doc string:

     (defun foo (name)
       (interactive \"sName?: \"))

but if we do not notice that the firt parenthesis comes before the first quote, then
we might think that there is a doc string that contains \"sName?: \".

Return the doc string if there is one, nil otherwie."
  (let ((xquote "^\\\"\\|[^\\]\\\"") ;; Match either a quote at the beginning
        (fp "[()]+")
        (first-quote 0)
        (first-paren 0)
        (sdesc 0)
        (edesc 0)
        (desc))
    (save-excursion
      (when (re-search-forward fp eof t)
        (setq first-paren (point)))
      )
    (save-excursion
      (when (re-search-forward xquote eof t)
        (setq first-quote (point)))
      )
    (when (< first-quote first-paren)
      (when (re-search-forward xquote eof t)
        (setq sdesc (point))
        (when (re-search-forward xquote eof t)
          (setq edesc (1- (point)))
          (setq desc (buffer-substring-no-properties sdesc edesc)))))
    desc
    ))


;;
;;  shu-capture-aliases
;;
(defun shu-capture-aliases ()
  (let (
        (gb (get-buffer-create shu-capture-buffer-name))
        (alias-rx "(\\s-*defalias\\s-*'\\([a-zA-Z0-9-_]+\\)\\s-*'\\([a-zA-Z0-9-_]+\\)\\s-*)")
        (alias)
        (name)
        (cell)
        )
    (setq shu-capture-alias-list nil)
    (goto-char (point-min))
    (while (re-search-forward alias-rx nil t)
      (setq alias (match-string 1))
      (setq name (match-string 2))
      (setq cell (cons name alias))
      (setq shu-capture-alias-list (cons cell shu-capture-alias-list))
      )
    ))


;;
;;  shu-doc-sort-compare
;;
(defun shu-doc-sort-compare (lhs rhs)
  "Compare two function names in a sort."
  (string< (car lhs) (car rhs))
  )


;;
;;  shu-doc-internal-to-md
;;
(defun shu-doc-internal-to-md (description)
  "DESCRIPTION contains a doc string from a function definition (with leading
and trailing quotes removed).  This function turns escaped quotes into regular
(non-escaped) quotes and turns names with leading and trailing asterisks (e.g.,
**project-count-buffer**) into short code blocks surrounded by back ticks.  It also
turns upper case names into lower case names surrounded by mardown ticks."
  (let ((esc-quote    "\\\\\"")
        (plain-quote  "\"")
        (star-name "*[a-zA-Z0-9*-_]+")
        (arg-name "\\(?:^\\|\\s-\\)*\\([A-Z0-9-]+\\)\\(?:\\s-\\|$\\|,\\|\\.\\)+")
        (nm)
        (ln)
        (result)
        (case-fold-search nil))
    (with-temp-buffer
      (insert description)
      (goto-char (point-min))
      (while (re-search-forward esc-quote nil t)
        (replace-match plain-quote))
      (goto-char (point-min))
      (while (re-search-forward star-name nil t)
        (replace-match (concat
                        shu-capture-md-buf-delimiter
                        (match-string 0)
                        shu-capture-md-buf-delimiter)))
      (goto-char (point-min))
      (while (re-search-forward arg-name nil t)
        (setq nm (match-string 1))
        (setq ln (downcase nm))
        (replace-match (concat
                        shu-capture-md-arg-delimiter
                        ln
                        shu-capture-md-arg-delimiter) t nil nil 1))
      (shu-capture-code-in-md)
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    result
    ))



;;
;;  shu-capture-code-in-md
;;
(defun shu-capture-code-in-md ()
  "The current buffer is assumed to hold a doc string that is being converted to
markdown.  Any line that is indented to column SHU-CAPTURE-DOC-CODE-INDENT or
greater is assumed to be a code snippet and will be surrounded by \"```\" to make
it a code snippet in markdown.  Return the number of code snippets marked."
  (let ((line-diff 0)
        (in-code)
        (count 0))
  (goto-char (point-min))
  (while (and (= line-diff 0)
              (not (= (point) (point-max))))
      (beginning-of-line)
      (when (re-search-forward shu-not-all-whitespace-regexp (line-end-position) t)
        (if (not in-code)
            (progn
              (when (> (current-column) shu-capture-doc-code-indent)
                (setq in-code t)
                (setq count (1+ count))
                (if (= 1 (line-number-at-pos))
                    (progn
                      (beginning-of-line)
                      (insert (concat shu-capture-md-code-delimiter "\n")))
                  (forward-line -1)
                  (end-of-line)
                  (insert (concat "\n" shu-capture-md-code-delimiter)))
                (forward-line 1)))
          (when (< (current-column) shu-capture-doc-code-indent)
            (setq in-code nil)
            (forward-line -1)
            (end-of-line)
            (insert (concat "\n" shu-capture-md-code-delimiter))
            (beginning-of-line))))
      (setq line-diff (forward-line 1)))
    (when in-code
      (insert (concat shu-capture-md-code-delimiter "\n")))
    count
    ))




;;
;;  shu-capture-show-list
;;
(defun shu-capture-show-list (func-list converters buffer)
  "FUNC-LIST is a list of function and macro defintions.  CONVERTERS
is an a-list of functions and strings as
follows:

      Key                              Value
      ---                              -----
      shu-capture-a-type-hdr           Function to format a section header
      shu-capture-a-type-func          Function to format a function signature
      shu-capture-a-type-buf           Function to format a buffer name
      shu-capture-a-type-arg           Function to format an argument name
      shu-capture-a-type-keywd         Function to format a key word
      shu-capture-a-type-doc-string    Function to finish formatting the doc string
      shu-capture-a-type-before        String that starts a block of verbatim code
      shu-capture-a-type-after         String that ends a block of verbstim code
      shu-capture-a-type-open-quote    String that is an open quote
      shu-capture-a-type-close-quote   String that is a close quote

This function goes through the list and uses the CONVERTERS to turn the set of
function definitions into either markdown or LaTex."
  (let (
        (func-converter (cdr (assoc shu-capture-a-type-func converters)))
        (xx func-list)
        (func-def)
        (func-string)
        (signature)
        (attributes)
        (description)
        (alias)
        )
    (while xx
      (setq func-def (car xx))
      (shu-capture-get-func-def func-def signature attributes description alias)
      (setq func-string (funcall func-converter func-def converters))
      (if (not description)
          (setq description "Undocumented")
        (setq description (shu-capture-convert-doc-string signature description converters))
        )
      (princ (concat "\n\n" func-string) buffer)
      (princ (concat "\n\n" description) buffer)
      (setq xx (cdr xx))
      )
    ))


;;
;;  shu-capture-show-list-md
;;
(defun shu-capture-show-list-md (func-list buffer)
  "Show a list"
  (let (
        (xx func-list)
        (func-def)
        (func-string)
        )
    (while xx
      (setq func-def (car xx))
      (setq func-string (shu-doc-internal-func-to-md func-def))
      (princ func-string buffer)
      (setq xx (cdr xx))
      )
    ))

;;
;;  shu-doc-internal-func-to-md
;;
(defun shu-doc-internal-func-to-md (func-def)
  "Take a function definition and turn it into a string of markdown text."
  (let (
        (gb (get-buffer-create shu-capture-buffer-name))
        (signature)
        (attributes)
        (description)
        (alias)
        (title "")
        (alias-line "")
        (interact-line "")
        (result ""))
    (shu-capture-get-func-def func-def signature attributes description alias)
    (setq title (shu-capture-func-type-name attributes))
    (if description
            (setq description (shu-doc-internal-to-md description))
      ;;      (princ (format "\nERROR: %s has no doc string.\n\n" signature) gb)
      (setq description "Undocumented"))
    (when alias
      (setq alias-line (concat " (" title ": " alias ")")))
    (when (and (not (= 0 (logand attributes shu-capture-attr-inter)))
               (= 0 (logand attributes shu-capture-attr-alias)))
      (setq interact-line "<br/>\nInteractive"))
    (when (not (= 0 (logand attributes shu-capture-attr-macro)))
      (setq interact-line "<br/>\nMacro"))
    (setq result
          (concat
           "\n**" signature "**"
           alias-line
           interact-line
           "\n\n" description "\n"))
    result
    ))



;;
;;  shu-capture-convert-func-md
;;
(defun shu-capture-convert-func-md (func-def converters)
  "Take a function definition and turn it into a string of markdown.  Return said string."
  (let (
        (gb (get-buffer-create shu-capture-buffer-name))
        (arg-converter (cdr (assoc shu-capture-a-type-arg shu-capture-md-converters)))
        (keywd-converter (cdr (assoc shu-capture-a-type-keywd shu-capture-md-converters)))
        (section-converter (cdr (assoc shu-capture-a-type-hdr converters)))
        (signature)
        (attributes)
        (description)
        (alias)
        (title "")
        (alias-line "")
        (interact-line "")
        (func-name)
        (func-type)
        (markups)
        (args)
        (result ""))
    (shu-capture-get-func-def func-def signature attributes description alias)
    (setq title (shu-capture-func-type-name attributes))
    (when alias
      (setq alias-line (concat " (" title ": " alias ")")))
    (setq func-type (shu-capture-func-type-name attributes))
    (shu-capture-get-name-and-args signature func-name args)
    (setq markups (shu-capture-convert-args-to-markup signature arg-converter keywd-converter))
    (setq result (shu-capture-make-args-md func-name markups func-type section-converter))
    result
    ))




;;
;;  shu-capture-make-args-md
;;
(defun shu-capture-make-args-md (func-name markups func-type section-converter)
  "FUNC-NAME is the name of the function, macro, alias, ect.  FUNC-TYPE is a
string that represents the function type.  This will be part of the argument
display.  MARKUPS is either nil or is a cons cell that points to two lists.  If
MARKUPS is nil, the function has no arguments.  If MARKUPS is non-nil, it is a
cons cell that points to two lists.  The car of MARKUPS is a list of the lengths
of each argument before any markup was added to the argument.  If an argument
name is \"arg1,\" its length is 4 even though the length of the argument name
after markup is applied mDaye be longer.  The cdr of MARKUPS is a list of the
arguments with markup applied to them.  SECTION-CONVERTER is the function that
will turn a sring into a section heading."
  (let (
        (arg)
        (args)
        (pad)
        (result)
        (sec-hdr (funcall section-converter 4 func-name))
        )
    (with-temp-buffer
      (insert
       (concat
        "\n\n" sec-hdr "\n"))
      (when markups
        (insert func-name)
        (setq args (cdr markups))
        (setq pad " ")
        (while args
          (setq arg (car args))
          (insert (concat pad arg))
          (setq args (cdr args))
          )
        (insert "\n")
        )
      (insert (concat "[" func-type "]"))
      (setq result (buffer-substring-no-properties (point-min) (point-max)))
      )
    result
    ))



;;
;;  shu-capture-convert-func-latex
;;
(defun shu-capture-convert-func-latex (func-def converters)
  "Take a function definition and turn it into a string of LaTex.  Return said string."
  (let (
        (gb (get-buffer-create shu-capture-buffer-name))
        (arg-converter (cdr (assoc shu-capture-a-type-arg shu-capture-latex-converters)))
        (keywd-converter (cdr (assoc shu-capture-a-type-keywd shu-capture-latex-converters)))
        (signature)
        (attributes)
        (description)
        (alias)
        (title "")
        (alias-line "")
        (interact-line "")
        (func-name)
        (func-type)
        (markups)
        (args)
        (result ""))
    (shu-capture-get-func-def func-def signature attributes description alias)
    (setq title (shu-capture-func-type-name attributes))
    (when alias
      (setq alias-line (concat " (" title ": " alias ")")))
    (setq func-type (shu-capture-func-type-name attributes))
    (shu-capture-get-name-and-args signature func-name args)
    (setq markups (shu-capture-convert-args-to-markup signature arg-converter keywd-converter))
    (setq result (shu-capture-make-args-latex func-name markups func-type))
    result
    ))




;;
;;  shu-capture-make-args-latex
;;
(defun shu-capture-make-args-latex (func-name markups func-type)
  "FUNC-NAME is the name of the function, macro, alias, ect.  FUNC-TYPE is a
string that represents the function type.  This will be part of the argument
display.  MARKUPS is either nil or is a cons cell that points to two lists.  If
MARKUPS is nil, the function has no arguments.  If MARKUPS is non-nil, it is a
cons cell that points to two lists.  The car of MARKUPS is a list of the lengths
of each argument before any markup was added to the argument.  If an argument
name is \"arg1,\" its length is 4 even though the length of the argument name
after markup is applied mDaye be longer.  The cdr of MARKUPS is a list of the
arguments with markup applied to them."
  (let ((fill-string (concat
                      "\\index{" func-name "} "
                      "\\hfill [" func-type "]"))
        (filled)
        (size)
        (sizes)
        (arg)
        (args)
        (pad)
        (acount)
        (ccount)
        (ncount)
        (result))
    (with-temp-buffer
      (insert
       (concat
        "\\vspace{1em}\n"
        "\\noindent\n"`
        "\\savebox{\\funcname}{\\noindent\\texttt{"
        func-name " }}\n"
        "\\usebox{\\funcname}"))
      (if (not markups)
          (progn
            (insert (concat "\n" fill-string))
            (setq filled t))
        (setq sizes (car markups))
        (setq args (cdr markups))
        (setq pad "")
        (setq acount 0)
        (setq ccount (1+ (length func-name)))
        (while (and sizes args)
          (setq arg (car args))
          (setq size (car sizes))
          (setq ncount (+ ccount size (length pad)))
          (if (and (> acount 0)
                   (> ncount 50))
              (progn
                (when (not filled)
                  (insert (concat "\n" fill-string))
                  (setq filled t))
                (insert "\n\\hspace*{\\wd\\funcname}")
                (setq pad "")
                (setq acount 0)
                (setq ccount (1+ (length func-name))))
            (insert (concat pad arg))
            (setq acount (1+ acount))
            (setq pad " ")
            (setq ccount ncount))
          (setq sizes (cdr sizes))
          (setq args (cdr args)))
        (when (not filled)
          (insert (concat "\n" fill-string))
          (setq filled t)))
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    result
    ))



;;
;;  shu-capture-convert-doc-string
;;
(defun shu-capture-convert-doc-string (signature description converters)
  "DESCRIPTION contains a doc string from a function definition (with leading
and trailing quotes removed).  CONVERTERS is an a-list of functions and strings as
follows:

      Key                              Value
      ---                              -----
      shu-capture-a-type-hdr           Function to format a section header
      shu-capture-a-type-func          Function to format a function signature
      shu-capture-a-type-buf           Function to format a buffer name
      shu-capture-a-type-arg           Function to format an argument name
      shu-capture-a-type-keywd         Function to format a key word
      shu-capture-a-type-doc-string    Function to finish formatting the doc string
      shu-capture-a-type-before        String that starts a block of verbatim code
      shu-capture-a-type-after         String that ends a block of verbstim code
      shu-capture-a-type-open-quote    String that is an open quote
      shu-capture-a-type-close-quote   String that is a close quote

This function turns escaped quotes into open and close quote strings, turns names
with leading and trailing asterisks (e.g., **project-buffer**) into formatted buffer
names, turns upper case names that match any argument names into lower case,
formatted argument names.  This is an internal function of shu-capture-doc and
will likely crash if called with an invalid a-list."
  (let (
        (gb (get-buffer-create "**slp**"))
        (star-name "*[a-zA-Z0-9*-_]+")
        (arg-name "\\(?:^\\|\\s-\\)*\\([A-Z0-9-]+\\)\\(?:\\s-\\|$\\|,\\|\\.\\)+")
        (buf-converter (cdr (assoc shu-capture-a-type-buf converters)))
        (arg-converter (cdr (assoc shu-capture-a-type-arg converters)))
        (all-converter (cdr (assoc shu-capture-a-type-doc-string converters)))
        (pre-code (cdr (assoc shu-capture-pre-code-in-doc converters)))
        (before-code (cdr (assoc shu-capture-a-type-before converters)))
        (after-code (cdr (assoc shu-capture-a-type-after converters)))
        (open-quote  (cdr (assoc shu-capture-a-type-open-quote converters)))
        (close-quote  (cdr (assoc shu-capture-a-type-close-quote converters)))
        (nm)
        (ln)
        (result)
        (debug-on-error t)
        (case-fold-search nil))
    (with-temp-buffer
      (princ (concat "\n\nBEFORE:\n" description) gb)
      (insert description)
      (funcall pre-code)
      (goto-char (point-min))
      (shu-capture-convert-quotes open-quote close-quote)
      (princ (concat "\nAFTER shu-capture-convert-quotes:\n"
                     (buffer-substring-no-properties (point-min) (point-max))) gb)
      (goto-char (point-min))
      (while (re-search-forward star-name nil t)
        (replace-match (funcall buf-converter (match-string 0)) t t))
      (princ (concat "\nAFTER converting buffer names:\n"
                     (buffer-substring-no-properties (point-min) (point-max))) gb)
      (goto-char (point-min))
      (shu-capture-doc-convert-args signature converters)
      (princ (concat "\nAFTER converting arg names:\n"
                     (buffer-substring-no-properties (point-min) (point-max))) gb)
      (shu-capture-code-in-doc before-code after-code)
      (funcall all-converter)
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
      (princ (concat "\n\nAFTER:\n" result) gb)
    result
    ))



;;
;;  shu-capture-code-in-doc
;;
(defun shu-capture-code-in-doc (before-code after-code)
  "The current buffer is assumed to hold a doc string that is being converted to
markdown.  Any line that is indented to column SHU-CAPTURE-DOC-CODE-INDENT or
greater is assumed to be a code snippet.  To format this as a code snippet,
BEFORE-CODE is placed one line above the code snippet and AFTER-CODE is placed
one line below the code snippet.  Return the number of code snippets marked."
  (let ((line-diff 0)
        (in-code)
        (count 0))
  (goto-char (point-min))
  (while (and (= line-diff 0)
              (not (= (point) (point-max))))
      (beginning-of-line)
      (when (re-search-forward shu-not-all-whitespace-regexp (line-end-position) t)
        (if (not in-code)
            (progn
              (when (> (current-column) shu-capture-doc-code-indent)
                (setq in-code t)
                (setq count (1+ count))
                (if (= 1 (line-number-at-pos))
                    (progn
                      (beginning-of-line)
                      (insert (concat before-code "\n")))
                  (forward-line -1)
                  (end-of-line)
                  (insert (concat "\n" before-code)))
                (forward-line 1)))
          (when (< (current-column) shu-capture-doc-code-indent)
            (setq in-code nil)
            (forward-line -1)
            (end-of-line)
            (insert (concat "\n" after-code))
            (beginning-of-line))))
      (setq line-diff (forward-line 1)))
    (when in-code
      (insert (concat after-code "\n")))
    count
    ))



;;
;;  shu-capture-convert-quotes
;;
(defun shu-capture-convert-quotes (open-quote close-quote)
  "Go through the current buffer converting any escaped quote to either an open or
close quote.  If an escaped quote is preceded by whitespace, \"(\", \"{\", \"<\", or \">\",
or by a close quote, then we replace it with an open quote.  Otherwise we replace it
with a close quote."
  (let* ((diff-quotes (not (string= open-quote close-quote)))
         (esc-quote    "\\\\\"")
         (is-open-chars (append shu-all-whitespace-chars (list "(" "[" "<" ">")))a
         (is-open-rx (regexp-opt is-open-chars))
         (is-open)
         (move-len 0)
         (count 0))
    (goto-char (point-min))
    (while (re-search-forward esc-quote nil t)
      (setq is-open nil)
      (when diff-quotes
        (when (> (current-column) (1+ (length open-quote)))
          (setq move-len (- (+ 2 (length open-quote))))
          (save-excursion
            (save-match-data
              (forward-char move-len)
              (when (looking-at close-quote)
                (setq is-open t))))))
      (when (not is-open)
        (if (not (> (current-column) 2))
            (setq is-open t)
          (save-excursion
            (save-match-data
              (forward-char -3)
              (when (looking-at is-open-rx)
                (setq is-open t))))))
      (if is-open
          (replace-match open-quote)
        (replace-match close-quote))
      (setq count (1+ count)))
    count
    ))



;;
;;  shu-capture-doc-convert-args-to-md
;;
(defun shu-capture-doc-convert-args-to-md (signature)
  (shu-capture-doc-convert-args signature shu-capture-md-converters))



;;
;;  shu-capture-doc-convert-args-to-latex
;;
(defun shu-capture-doc-convert-args-to-latex (signature)
  (shu-capture-doc-convert-args signature shu-capture-latex-converters))



;;
;;  shu-capture-doc-convert-args
;;
(defun shu-capture-doc-convert-args (signature converters)
  "The current buffer contains a doc string from a function.  The argument to this
function is the SIGNATURE of the function for which the doc string was written.
This function goes through the doc string buffer looking for any word that is all
upper case.  If the upper case word matches the name of an argument to the function,
it is passed to the CONVERTER function for conversion into a markup language, which
is probably markdown or LaTex, and it is then replaced in the doc string buffer.

For example, if the function has the following signature:

     do-something (hat cat)

with the following doc string:

  \"The Linux HAT is converted to an IBM CAT.\"

would be converted to:

  \"The Linux *hat* is converted to an IBM *cat*.\""
  (let (
        (gb (get-buffer-create "**slp**"))
        (arg-name "\\(?:^\\|\\s-\\)*\\([A-Z0-9-]+\\)\\(?:\\s-\\|$\\|,\\|\\.\\)+")
        (args (shu-capture-get-args-as-alist signature))
        (arg-converter (cdr (assoc shu-capture-a-type-arg converters)))
        (pname)
        (new-name)
        (name-prefix)
        (count 0))
    (goto-char (point-min))
    (princ "\narg-converter " gb)
    (print arg-converter gb)
    (princ "\n" gb)
    (while (re-search-forward arg-name nil t)
      (setq pname (downcase (match-string 1)))
      (setq new-name (funcall arg-converter pname))
      (setq name-prefix "")
      (when (> (length pname) 11)
        (setq name-prefix (substring pname 0 11)))
      (if (assoc pname args)
          (progn
            (replace-match new-name t t nil 1)
            (setq count (1+ count)))
        (when (and (intern-soft pname)
                   (string= "shu-capture" name-prefix))
          (replace-match new-name t t nil 1)
          (setq count (1+ count)))))
    count
    ))



;;
;;  shu-capture-get-args-as-alist
;;
(defun shu-capture-get-args-as-alist (signature)
  "SIGNATURE contains the function signature (both function name and arguments).
This function returns the arguments as an a-list in which all of the argument
names are the keys.  The special argument names \"&optional\" and \"&rest\", if
present, are not copied into the a-list.

For example, if SIGNATURE holds the following:

     do-somerhing (with these things &optional and &rest others)

an a-list is returned with the keys \"others,\" \"and,\" \"things,\" \"these,\" and
\"with.\""
  (let ((func-name)
        (arg-string)
        (arg-list)
        (arg-assoc)
        (arg)
        (x))
    (shu-capture-get-name-and-args signature func-name arg-string)
    (when (not (= 0 (length arg-string)))
      (setq arg-list (split-string arg-string))
      (while arg-list
        (setq arg (car arg-list))
        (when (and (not (string= arg shu-capture-keywd-optional))
                   (not (string= arg shu-capture-keywd-rest)))
          (setq x (cons arg 1))
          (if (not arg-assoc)
              (setq arg-assoc (list x))
            (when (not (assoc arg arg-assoc))
              (setq arg-assoc (cons x arg-assoc)))))
        (setq arg-list (cdr arg-list))))
    arg-assoc
    ))



;;
;;  shu-capture-convert-args-to-markup
;;
(defun shu-capture-convert-args-to-markup (signature arg-converter keywd-converter)
  "SIGNATURE contains the function signature (both function name and arguments).
ARG-CONVERTER is the function used to convert an argument to markup.  KEYWD-CONVERTER
is the function used to convert an argument list keyword, such as \"&optional\" or \"&rest\"
to markup.

This function returns a cons cell pointing to two lists.  The first list contains the length
of each argument name prior to conversion to markup.  This is because the amount of space
on a line is largely determied by the length of the unconverted argument.  \"arg\" will
take much less space on a line than will the same word with markup added.  The second list
contains each of the argument names converted to the appropriate markup.

Given the following function signature:

     do-somerhing (with these things &optional and &rest others)

the length list will contain (4, 5, 6, 9, 3, 5, 6).  The converted arguments list for
markdown will contain (\"*with*\", \"*these*\", \"*things*\", \"**&optional**\", \"*and*\",
\"**&rest**\", \"*others*\").

If the function signature contains no arguments, then nil is returned instead of the
above described cons cell."
  (let ((func-name)
        (arg-string)
        (arg-list)
        (arg)
        (lengths)
        (markup-args)
        (result)
        (x))
    (shu-capture-get-name-and-args signature func-name arg-string)
    (unless (= 0 (length arg-string))
      (setq arg-list (split-string arg-string))
      (setq arg-list (nreverse arg-list))
      (setq x arg-list)
      (while x
        (setq arg (car x))
        (setq lengths (cons (length arg) lengths))
        (if (or (string= arg shu-capture-keywd-optional)
                (string= arg shu-capture-keywd-rest))
            (setq arg (funcall keywd-converter arg))
          (setq arg (funcall arg-converter arg)))
        (setq markup-args (cons arg markup-args))
        (setq x (cdr x)))
      (setq result (cons lengths markup-args)))
    result
    ))



;;
;;  shu-capture-func-type-name
;;
(defun shu-capture-func-type-name (attributes)
  "Return the name of the type \"Alias,\" \"Macro,\" \"Constant,\" \"Variable,\" or
\"Function\" based on the ATTRIBUTES passed in."
  (let ((macro-name "Macro")
        (function-name "Function")
        (interactive-name "Command")
        (constant-name "Constant")
        (variable-name "Variable")
        (name))
    (cond
     ((and (/= 0 (logand attributes shu-capture-attr-alias))
           (/= 0 (logand attributes shu-capture-attr-macro)))
      (setq name macro-name))
     ((and (/= 0 (logand attributes shu-capture-attr-alias))
           (/= 0 (logand attributes shu-capture-attr-inter)))
      (setq name interactive-name))
     ((/= 0 (logand attributes shu-capture-attr-alias))
      (setq name function-name))
     ((/= 0 (logand attributes shu-capture-attr-macro))
      (setq name macro-name))
     ((/= 0 (logand attributes shu-capture-attr-const))
      (setq name constant-name))
     ((/= 0 (logand attributes shu-capture-attr-var))
      (setq name variable-name))
     (t
      (if (/= 0 (logand attributes shu-capture-attr-inter))
          (setq name interactive-name)
        (setq name function-name))))
    name
    ))

;;; shu-capture-doc.el ends here
