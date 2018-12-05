;;; shu-capture-doc.el --- Shu project code to capture function doc strings
;;
;; Copyright (C) 2018 Stewart L. Palmer
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

;; NB: (intern-soft string) will return non-nil if string holds a symbol name

;;
;;  shu-capture-doc.el
;;
;;  Collection of functions used to capture doc strings in elisp functions
;;

(provide 'shu-capture-doc)

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
  `(let ((func-alias)
         (func-info))
     (setq func-info (cons ,attributes ,description))
     (setq func-alias (cons nil func-info))
     (setq ,func-def (cons ,signature func-alias))
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
  `(let ((func-alias)
         (func-info))
     (setq ,signature (car ,func-def))
     (setq func-alias (cdr ,func-def))
     (setq ,alias (car func-alias))
     (setq func-info (cdr func-alias))
     (setq ,attributes (car func-info))
     (setq ,description (cdr func-info))
     ))


;;
;;  shu-capture-get-func-def-sig
;;
(defmacro shu-capture-get-func-def-sig (func-def signature)
  "Extract the function signature from the func-def"
  `(let ((func-info))
     (setq ,signature (car ,func-def))
     ))


;;
;;  shu-capture-get-func-def-alias
;;
(defmacro shu-capture-get-func-def-alias (func-def alias)
  "Extract the function alias from the func-def"
  `(let (
         (func-alias)
         )
     (setq func-alias (car ,func-def))
     (setq ,alias (car func-alias))
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
;;  shu-capture-a-type-before
;;
(defconst shu-capture-a-type-before 5
  "The a-list key value that identifies the string that is placed before a verbatim
code snippet.")


;;
;;  shu-capture-a-type-after
;;
(defconst shu-capture-a-type-after 6
  "The a-list key value that identifies the string that is placed after a verbatim
code snippet.")


;;
;;  shu-capture-a-type-open-quote
;;
(defconst shu-capture-a-type-open-quote 7
  "The a-list key value that identifies the string that is an open quote.")


;;
;;  shu-capture-a-type-close-quote
;;
(defconst shu-capture-a-type-close-quote 8
  "The a-list key value that identifies the string that is a close quote.")



;;
;;  shu-capture-md-section-delimiter
;;
(defconst shu-capture-md-section-delimiter "##"
  "Define the markdown delimiter that is used to identfy a section.  This is se[arated
from the section name by a space.")


;;
;;  shu-capture-latex-section-start
;;
(defconst shu-capture-latex-section-start "\subsection{"
  "Define the LaTex tag that is used to identfy the start of a section headig.")


;;
;;  shu-capture-latex-section-end
;;
(defconst shu-capture-latex-section-end "}"
  "Define the LaTex tag that is used to identfy the start of a section headig.")


;;
;;  shu-capture-md-arg-delimiter
;;
(defconst shu-capture-md-arg-delimiter "**"
  "Define the markdown delimiter that is used to surround an argument name.")


;;
;;  shu-capture-latex-arg-start
;;
(defconst shu-capture-latex-arg-start "\\textbf{"
  "Define the latex string that is used to prepended to an arguument name.")


;;
;;  shu-capture-latex-arg-end
;;
(defconst shu-capture-latex-arg-end "}"
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
(defconst shu-capture-latex-buf-start "\emph{"
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
(defconst shu-capture-latex-code-start "\begin{verbatim}"
  "Define the LaTex string that is at the beginning of a verbatim
code snippet.")


;;
;;  shu-capture-latex-code-end
;;
(defconst shu-capture-latex-code-end "\end{verbatim}"
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
;;  shu-capture-doc-code-indent
;;
(defconst shu-capture-doc-code-indent 4
  "Any line indented by this much in a doc stringis assumed to be a sample
code snippet.")



;;
;;  shu-capture-md-converters
;;
(defconst shu-capture-md-converters
  (list
   (cons shu-capture-a-type-hdr          'shu-capture-make-md-section)
   (cons shu-capture-a-type-func         'shu-capture-convert-func-md)
   (cons shu-capture-a-type-buf          'shu-capture-buf-to-md)
   (cons shu-capture-a-type-arg          'shu-capture-arg-to-md)
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
  "Convert a function argument in a doc-string to markdown."
  (concat shu-capture-md-arg-delimiter arg-name shu-capture-md-arg-delimiter)
  )



;;
;;  shu-capture-arg-to-latex
;;
(defun shu-capture-arg-to-latex (arg-name)
  "Convert a function argument in a doc-string to LaTex."
  (concat shu-capture-latex-arg-start arg-name shu-capture-latex-arg-end)
    )



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
  (let (
        (ggb (get-buffer-create "**slp**"))
        (gb (get-buffer-create "**shu-capture-doc**"))
        (ss
         (concat
          "("
          "\\s-*"
          "\\(defun\\|defmacro\\)\\"
          "s-+"))
        (fs
         (concat
          "("
          "\\s-*"
          "\\(defun\\|defmacro\\)"
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
    (setq alias-list (sort alias-list 'shu-doc-sort-compare))
    (princ "\n\n## List of functions by alias name ##\n\n" gb)
    (shu-capture-show-list alias-list converters gb)
    (setq func-list (sort func-list 'shu-doc-sort-compare))
    (princ "\n\n## Full list of functions ##\n" gb)
    (shu-capture-show-list func-list converters gb)
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

(defun shu-capture-aliases ()
  (let (
        (gb (get-buffer-create "**shu-capture-doc**"))
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
gteater is assumed to be a code snippet and will be surrounded by \"```\" to make
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
        (description)
        (alias)
        )
    (while xx
      (setq func-def (car xx))
      (shu-capture-get-func-def func-def signature attributes description alias)
      (setq func-string (funcall func-converter func-def))
      (if (not description)
          (setq description "Undocumented")
        (setq description (shu-capture-convert-doc-string-new func-def converters))
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
        (gb (get-buffer-create "**shu-capture-doc**"))
        (signature)
        (attributes)
        (description)
        (alias)
        (title "")
        (alias-line "")
        (interact-line "")
        (result ""))
    (shu-capture-get-func-def func-def signature attributes description alias)
    (setq title
          (if (not (= 0 (logand attributes shu-capture-attr-alias)))
              "Function"
            "Alias"))
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
(defun shu-capture-convert-func-md (func-def)
  "Take a function definition and turn it into a string of markdown text."
  (let (
        (gb (get-buffer-create "**shu-capture-doc**"))
        (signature)
        (attributes)
        (description)
        (alias)
        (title "")
        (alias-line "")
        (interact-line "")
        (result ""))
    (shu-capture-get-func-def func-def signature attributes description alias)
    (setq title
          (if (not (= 0 (logand attributes shu-capture-attr-alias)))
              "Function"
            "Alias"))
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
           "<br/>\n"))
    result
    ))



;;
;;  shu-capture-convert-func-latex
;;
(defun shu-capture-convert-func-latex (func-def)
  "Take a function definition and turn it into a string of LaTex."
  (let (
        (gb (get-buffer-create "**shu-capture-doc**"))
        (signature)
        (attributes)
        (description)
        (alias)
        (title "")
        (alias-line "")
        (interact-line "")
        (result ""))
    (shu-capture-get-func-def func-def signature attributes description alias)
    (setq title
          (if (not (= 0 (logand attributes shu-capture-attr-alias)))
              "Function"
            "Alias"))
    (when alias
      (setq alias-line (concat " (" title ": " alias ")")))
    (when (and (not (= 0 (logand attributes shu-capture-attr-inter)))
               (= 0 (logand attributes shu-capture-attr-alias)))
      (setq interact-line "\%\nInteractive"))
    (when (not (= 0 (logand attributes shu-capture-attr-macro)))
      (setq interact-line "\%\nMacro"))
    (setq result
          (concat
           "\n\\textbf{" signature "}"
           alias-line
           interact-line
           "\n"))
    result
    ))




;;
;;  shu-capture-convert-doc-string
;;
(defun shu-capture-convert-doc-string (description converters)
"DESCRIPTION contains a doc string from a function definition (with leading
and trailing quotes removed).  CONVERTERS is an a-list of functions and strings as
follows:

      Key                              Value
      ---                              -----
      shu-capture-a-type-hdr           Function to format a section header
      shu-capture-a-type-func          Function to format a function signature
      shu-capture-a-type-buf           Function to format a buffer name
      shu-capture-a-type-arg           Function to format an argument name
      shu-capture-a-type-before        String that starts a block of verbatim code
      shu-capture-a-type-after         String that ends a block of verbstim code
      shu-capture-a-type-open-quote    String that is an open quote
      shu-capture-a-type-close-quote   String that is a close quote

This function turns escaped quotes into open and close quote strings, turns names
with leading and trailing asterisks (e.g., **project-buffer**) into formatted buffer
names, turns upper case names that match any argument names into lower case,
formatted argument names.  This is an internal function of shu-capture-doc and
will likely crash if called with an invalid a-list."
  (let ((star-name "*[a-zA-Z0-9*-_]+")
        (arg-name "\\(?:^\\|\\s-\\)*\\([A-Z0-9-]+\\)\\(?:\\s-\\|$\\|,\\|\\.\\)+")
        (buf-converter (cdr (assoc shu-capture-a-type-buf converters)))
        (arg-converter (cdr (assoc shu-capture-a-type-arg converters)))
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
      (insert description)
      (goto-char (point-min))
      (shu-capture-convert-quotes open-quote close-quote)
      (goto-char (point-min))
      (while (re-search-forward star-name nil t)
        (replace-match (funcall buf-converter (match-string 0))))
      (goto-char (point-min))
      (while (re-search-forward arg-name nil t)
        (setq nm (match-string 1))
        (setq ln (downcase nm))
        (replace-match (funcall arg-converter ln) t nil nil 1))
      (shu-capture-code-in-doc before-code after-code)
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    result
    ))



;;
;;  shu-capture-convert-doc-string-new
;;
(defun shu-capture-convert-doc-string-new (func-def converters)
"DESCRIPTION contains a doc string from a function definition (with leading
and trailing quotes removed).  CONVERTERS is an a-list of functions and strings as
follows:

      Key                              Value
      ---                              -----
      shu-capture-a-type-hdr           Function to format a section header
      shu-capture-a-type-func          Function to format a function signature
      shu-capture-a-type-buf           Function to format a buffer name
      shu-capture-a-type-arg           Function to format an argument name
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
        (before-code (cdr (assoc shu-capture-a-type-before converters)))
        (after-code (cdr (assoc shu-capture-a-type-after converters)))
        (open-quote  (cdr (assoc shu-capture-a-type-open-quote converters)))
        (close-quote  (cdr (assoc shu-capture-a-type-close-quote converters)))
        (nm)
        (ln)
        (result)
        (debug-on-error t)
        (signature)
        (attributes)
        (description)
        (alias)
        (case-fold-search nil))
  (princ "shu-capture-convert-doc-string-new\n" gb)
  (princ func-def gb)
  (princ "\n" gb)
    (shu-capture-get-func-def func-def signature attributes description alias)
    (with-temp-buffer
      (insert description)
      (goto-char (point-min))
      (shu-capture-convert-quotes open-quote close-quote)
      (goto-char (point-min))
      (while (re-search-forward star-name nil t)
        (replace-match (funcall buf-converter (match-string 0))))
      (goto-char (point-min))
      (shu-capture-doc-convert-args signature converters)
      (while (re-search-forward arg-name nil t)
        (setq nm (match-string 1))
        (setq ln (downcase nm))
        (replace-match (funcall arg-converter ln) t nil nil 1))
      (shu-capture-code-in-doc before-code after-code)
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    result
    ))



;;
;;  shu-capture-code-in-doc
;;
(defun shu-capture-code-in-doc (before-code after-code)
  "The current buffer is assumed to hold a doc string that is being converted to
markdown.  Any line that is indented to column SHU-CAPTURE-DOC-CODE-INDENT or
gteater is assumed to be a code snippet.  To format this as a code snippet,
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

  \"The Linux \emph{hat} is converted to an IBM \emph{cat}.\""
  (let ((arg-name "\\(?:^\\|\\s-\\)*\\([A-Z0-9-]+\\)\\(?:\\s-\\|$\\|,\\|\\.\\)+")
        (args (shu-capture-get-args-as-alist signature))
        (arg-converter (cdr (assoc shu-capture-a-type-arg converters)))
        (pname)
        (new-name)
        (count 0))
    (goto-char (point-min))
    (while (re-search-forward arg-name nil t)
      (setq pname (downcase (match-string 1)))
      (setq new-name (funcall arg-converter pname))
      (when (assoc pname args)
        (replace-match (concat
                        shu-capture-md-arg-delimiter
                        pname
                        shu-capture-md-arg-delimiter) t nil nil 1)
        (setq count (1+ count))))
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
  (let ((fs   "\\s-*\\([0-9a-zA-Z-]+\\)\\s-*(\\s-*\\([ 0-9a-zA-Z-,&\n]*\\))")
        (arg-string)
        (arg-list)
        (arg-assoc)
        (arg)
        (x))
    (when (string-match fs signature)
      (setq arg-string (match-string 2 signature))
      (setq arg-list (split-string arg-string))
      (when (not (= 0 (length arg-string)))
        (while arg-list
          (setq arg (car arg-list))
          (when (and (not (string= arg "&optional"))
                     (not (string= arg "&rest")))
            (setq x (cons arg 1))
            (if (not arg-assoc)
                (setq arg-assoc (list x))
              (when (not (assoc arg arg-assoc))
                (setq arg-assoc (cons x arg-assoc)))))
          (setq arg-list (cdr arg-list)))))
    arg-assoc
    ))
