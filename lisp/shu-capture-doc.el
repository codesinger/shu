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
;; TODO: Use the same structure to capture an alias.  But in the case of the alias,
;;       the signature in func-def is the signature of the alias and the alias name
;;       in func-alias is the name of the actual function.
;;
;;
;;
;;
;;  func-def:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> func-alias
;;        |
;;        +-------------> signature
;;
;;
;;  func-alias
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> func-info
;;        |
;;        +-------------> alias
;;
;;
;;  func-info:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> description
;;        |
;;        +-------------> attributes
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
;;  shu-capture-md-arg-delimiter
;;
(defconst shu-capture-md-arg-delimiter "`"
  "Define the delimiter that is used to surround an argument name.")


;;
;;  shu-capture-attr-inter
;;
(defconst shu-capture-attr-inter (lsh 1 0)
  "Bit that indicates that a function is interactive")


;;
;;  shu-capture-attr-alias
;;
(defconst shu-capture-attr-alias (lsh 1 1)
  "Bit that indicates that a function is interactive")


;;
;;  shu-capture-md-buf-delimiter
;;
(defconst shu-capture-md-buf-delimiter "`"
  "Define the delimiter that is used to surround a buffer name or
any other name that has leading and trailing asterisks")



;;
;;  shu-capture-doc-code-indent
;;
(defconst shu-capture-doc-code-indent 4
  "Any line indented by this much in a doc stringis assumed to be a sample
code snippet.")


;;
;;  shu-capture-doc
;;
(defun shu-capture-doc ()
  (interactive)
  (let (
        (ggb (get-buffer-create "**slp**"))
        (gb (get-buffer-create "**shu-capture-doc**"))
        (ss   "(defun\\s-+")
        (fs   "(defun\\s-*\\([0-9a-zA-Z-]+\\)\\s-*(\\s-*\\([ 0-9a-zA-Z-,&\n]*\\))")
        (inter  "(interactive")
        (attributes 0)
        (attribs 0)
        (sof 0)
        (eof 0)
        (fn)
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
        (debug-on-error t)
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
            (setq fn (match-string 1))
            (setq args (match-string 2))
            (setq func-sig (concat fn " (" args ")"))
            (princ (format "FUNC: %s\n" func-sig) ggb)
            (setq desc (shu-capture-get-doc-string eof))
            (setq alias nil)
            (setq al nil)
            (setq alias (assoc fn shu-capture-alias-list))
            (when alias
              (setq al (cdr alias))
              (setq sig (concat al " (" args ")"))
              (setq attribs (logior attributes shu-capture-attr-alias))
              (shu-capture-set-func-def-alias alias-def sig attribs desc fn)
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
    (shu-capture-show-list-md alias-list gb)
    (setq func-list (sort func-list 'shu-doc-sort-compare))
    (princ "\n\n## Full list of functions ##\n" gb)
    (shu-capture-show-list-md func-list gb)
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
;;  func-def:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> func-alias
;;        |
;;        +-------------> signature
;;
;;
;;  func-alias
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> func-info
;;        |
;;        +-------------> alias
;;
;;
;;  func-info:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> description
;;        |
;;        +-------------> attributes
;;
(defun shu-internal-capture-doc ()
  (interactive)
  (let (
        (gb (get-buffer-create "**shu-capture-doc**"))
        (ss   "(defun\\s-+")
        (fs   "(defun\\s-*\\([a-zA-Z-]+\\)\\s-*(\\s-*\\([ a-zA-Z-,&\n]*\\))")
        (inter  "(interactive")
        (interact  "I")
        (sof 0)
        (eof 0)
        (fn)
        (args)
        (sdesc)
        (edesc)
        (desc)
        (func-sig)
        (func-def)
        (func-list)
        (xx)
        (xquote "^\\\"\\|[^\\]\\\"") ;; Match either a quote at the beginning
        )
    (goto-char (point-min))
    (while (re-search-forward ss nil t)
      (beginning-of-line)
      (setq sof (point))
      (setq eof (save-excursion
                  (forward-sexp)
                  (point)))
      (if (search-forward inter eof t)
          (setq interact "I")
        (setq interact "."))
      (goto-char sof)
      (when (re-search-forward fs eof t)
        (setq fn (match-string 1))
        (setq args (match-string 2))
        (setq func-sig (concat fn " (" args ")"))
        (princ (concat func-sig "\n") gb)
        (when (re-search-forward xquote eof t)
          (setq sdesc (point))
          (when (re-search-forward xquote eof t)
            (setq edesc (1- (point)))
            (setq desc (buffer-substring-no-properties sdesc edesc))
            (with-temp-buffer
              (insert desc)
              (shu-doc-internal-to-md)
              (fill-region (point-min) (point-max))
              (setq desc (buffer-substring-no-properties (point-min) (point-max)))
              )
            )
          (shu-capture-set-func-def func-def func-sig interact desc)
          (setq func-list (cons func-def func-list))
          (princ (concat desc "\n\n") gb)
          )
        )
      )
    (princ "End while\n" gb)
    (princ "\n\n" gb)
    (princ func-list gb )
    (princ "\n\nSorted:\n\n" gb)
    (sort func-list 'shu-doc-sort-compare)
    (princ func-list gb )
    func-list
    ))

(defun pcd ()
  (interactive)
  (let (
        (gb (get-buffer-create "**foo**"))
        (func-list (shu-internal-capture-doc))
        )

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
(defun shu-doc-internal-to-md ()
  "The current buffer contains a doc string from a function definition (with leading
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
        (case-fold-search nil))
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
                      (insert "```\n"))
                  (forward-line -1)
                  (end-of-line)
                  (insert "\n```"))
                (forward-line 1)))
          (when (< (current-column) shu-capture-doc-code-indent)
            (setq in-code nil)
            (forward-line -1)
            (end-of-line)
            (insert "\n```")
            (beginning-of-line))))
      (setq line-diff (forward-line 1)))
    (when in-code
      (insert "```\n"))
    count
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
        (progn
          (with-temp-buffer
            (insert description)
            (shu-doc-internal-to-md)
            (setq description (buffer-substring-no-properties (point-min) (point-max))))
          )
      ;;      (princ (format "\nERROR: %s has no doc string.\n\n" signature) gb)
      (setq description "Undocumented")
      )
    (when alias
      (setq alias-line (concat " (" title ": " alias ")")))
    (when (and (not (= 0 (logand attributes shu-capture-attr-inter)))
               (= 0 (logand attributes shu-capture-attr-alias)))
      (setq interact-line "<br/>\nInteractive"))
    (setq result
          (concat
           "\n**" signature "**"
           alias-line
           interact-line
           "\n\n" description "\n"))
    result
    ))



;;
;;  shu-capture-get-args-as-alist
;;
(defun shu-capture-get-args-as-alist (signature)
  "SIGNATURE contains the function signature (both function name and arguments).
This function returns the arguments as an a-list in which all of the argument names
are the keys.  The special argument names \"&optional\" and \"&rest\" are not
copied into the a-list if present."
  (interactive)
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
