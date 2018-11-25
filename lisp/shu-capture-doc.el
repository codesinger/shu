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

;; Information about a function and its doc string is contained in the folllwing two
;; cons cells.  The macros immmediately below can create the cons cells from the information
;; and extract the information from the cons cells.
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
  `(let (
         (func-alias)
         (func-info)
         )
     (setq func-info (cons ,attributes ,description))
     (setq func-alias (cons nil func-info))
     (setq ,func-def (cons ,signature func-alias))
     ))


;;
;;  shu-capture-set-func-def-alias
;;
(defmacro shu-capture-set-func-def-alias (func-def signature attributes description alias)
  "Create a func-def to describe the function"
  `(let (
         (func-alias)
         (func-info)
         )
     (setq func-info (cons ,attributes ,description))
     (setq func-alias (cons ,alias func-info))
     (setq ,func-def (cons ,signature func-alias))
     ))


;;
;;  shu-capture-get-func-def
;;
(defmacro shu-capture-get-func-def (func-def signature attributes description alias)
  "Extract the information from the func-def"
  `(let (
         (func-alias)
         (func-info)
         )
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
  `(let (
         (func-info)
         )
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
;;  shu-capture-md-buf-delimiter
;;
(defconst shu-capture-md-buf-delimiter "`"
  "Define the delimiter that is used to surround a buffer name or
any other name that has leading and trailing asterisks")


;;
;;  shu-capture-doc
;;
(defun shu-capture-doc ()
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
        (func-info)
        (func-def)
        (func-list)
        (xx)
        (alias)
        (al)
        (xquote "^\\\"\\|[^\\]\\\"") ;; Match either a quote at the beginning
        (al)
        (debug-on-error t)
        )
;;;    (setq al (shu-capture-aliases))
    (shu-capture-aliases)
    (princ "Captured start:\n" gb)
    (princ shu-capture-alias-list gb)
    (princ "\nCaptured end:\n" gb)
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
        (princ (concat "Looking: " func-sig "\n") gb)
        (when (re-search-forward xquote eof t)
          (setq sdesc (point))
          (when (re-search-forward xquote eof t)
            (setq edesc (1- (point)))
            (setq desc (buffer-substring-no-properties sdesc edesc))
            (with-temp-buffer
              (insert desc)
              (shu-internal-doc-to-md)
              (fill-region (point-min) (point-max))
              (setq desc (buffer-substring-no-properties (point-min) (point-max)))
              )
            )
          (princ "\n\nshu-capture-alias-list\n\n" gb)
          (princ shu-capture-alias-list gb)
          (princ "\n\n" gb)
          (princ "FN: " gb)
          (princ fn gb)
          (princ "\n" gb)
          (setq alias nil)
          (setq al nil)
          (setq alias (assoc fn shu-capture-alias-list))
          (when alias
            (setq al (cdr alias)))
          (shu-capture-set-func-def-alias func-def func-sig interact desc al)
          (setq func-list (cons func-def func-list))
          (princ (concat desc "\n\n") gb)
          )
        )
      )
    (princ "\n\n" gb)
    (princ "End while\n" gb)
    (princ func-list gb )
    (princ "\n\nBefore sort:\n\n" gb)
    (setq xx func-list)
    (while xx
      (setq func-def (car xx))
      (shu-capture-get-func-def func-def func-sig interact desc alias)
      (princ (concat "\n**" func-sig "**") gb)
      (when alias
        (princ (format " (alias: %s)" alias) gb)
        )
      (princ "\n" gb)
      (setq xx (cdr xx))
      )
    (princ "\n\nSorted:\n\n" gb)
    (setq func-list (sort func-list 'shu-doc-sort-compare))
    (setq xx func-list)
    (while xx
      (setq func-def (car xx))
      (shu-capture-get-func-def func-def func-sig interact desc alias)
      (princ (concat "\n**" func-sig "**") gb)
      (when alias
        (princ (format " (alias: %s)" alias) gb)
        )
      (princ "\n" gb)
      (setq xx (cdr xx))
      )
    (princ func-list gb )
    (princ "\n\nFINAL LIST\n" gb)

    (setq xx func-list)
    (while xx
      (setq func-def (car xx))
      (shu-capture-get-func-def func-def func-sig interact desc alias)
      (princ (concat "\n**" func-sig "**") gb)
      (when alias
        (princ (format " (alias: %s)" alias) gb)
        )
      (when (string= interact "I")
        (princ "<br/>\nInteractive" gb))
;;;      (princ (concat "\n" interact) gb)
      (princ (concat "\n\n" desc "\n") gb)
      (setq xx (cdr xx))
      )

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
              (shu-internal-doc-to-md)
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
      (princ (format "name: \"%s\", alias: \"%s\"\n" name alias) gb)
      (setq cell (cons name alias))
      (setq shu-capture-alias-list (cons cell shu-capture-alias-list))
      )
    (princ shu-capture-alias-list gb)
    (princ "\n\n" gb)
    ))



(defun shu-doc-sort-compare (lhs rhs)
  (string< (car lhs) (car rhs))
  )


;;
;;  shu-internal-doc-to-md
;;
(defun shu-internal-doc-to-md ()
  "The current buffer contains a doc string from a function definition (with leading
and trailing quotes removed).  This function turns escaped quotes into regular
(non-escaped) quotes and turns names with leading and trailing asterisks (e.g.,
**project-count-buffer**) into short code blocks surrounded by back ticks.  It also
turns upper case names into lower case names surroiunded by mardown ticks."
  (let (
        (gb (get-buffer-create "**slp**"))
        (esc-quote    "\\\\\"")
      (plain-quote  "\"")
      (star-name "*[a-zA-Z0-9*-_]+")
      (arg-name "\\(?:^\\|\\s-\\)*\\([A-Z0-9-]+\\)\\(?:\\s-\\|$\\|,\\)+")
      (nm)
      (ln)
      (case-fold-search nil))
  (goto-char (point-min))
  (while (re-search-forward esc-quote nil t)
    (princ (format "Matched quote: \"%s\"\n" (match-string 0)) gb)
    (replace-match plain-quote))
  (goto-char (point-min))
  (while (re-search-forward star-name nil t)
    (princ (format "Matched star: \"%s\"\n" (match-string 0)) gb )
    (replace-match (concat
                    shu-capture-md-buf-delimiter
                    (match-string 0)
                    shu-capture-md-buf-delimiter)))
  (goto-char (point-min))
  (while (re-search-forward arg-name nil t)
    (setq nm (match-string 1))
    (setq ln (downcase nm))
    (princ (format "Matched arg: \"%s\"\n" nm) gb )
    (replace-match (concat
                    shu-capture-md-arg-delimiter
                    ln
                    shu-capture-md-arg-delimiter) t nil nil 1))
  ))
