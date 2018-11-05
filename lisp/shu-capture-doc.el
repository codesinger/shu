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
;;        |       +-----> func-info
;;        |
;;        +-------------> signature
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
         (func-info)
         )
     (setq func-info (cons ,attributes ,description))
     (setq ,func-def (cons ,signature func-info))
     ))


;;
;;  shu-capture-get-func-def
;;
(defmacro shu-capture-get-func-def (func-def signature attributes description)
  "Extract the information from the func-def"
  `(let (
         (func-info)
         )
     (setq ,signature (car ,func-def))
     (setq func-info (cdr ,func-def))
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
    (princ "\n\n" gb)
    (princ func-list gb )
    (princ "\n\nSorted:\n\n" gb)
    (sort func-list 'shu-doc-sort-compare)
    (princ func-list gb )
    (princ "\nFINAL LIST\n" gb)

    (setq xx func-list)
    (while xx
      (setq func-def (car xx))
      (setq func-sig (car func-def))
      (setq func-info (cdr func-def))
      (setq interact (car func-info))
      (setq desc (cdr func-info))
      (princ (concat "\n**" func-sig "**") gb)
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
;;        |       +-----> func-info
;;        |
;;        +-------------> Name
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
;;        |       +-----> func-desc
;;        |
;;        +-------------> func-attrs
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
        (func-info)
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
          (setq func-info (cons interact desc))
          (setq func-def (cons func-sig func-info))
          (setq func-list (cons func-def func-list))
          (princ (concat desc "\n\n") gb)
          )
        )
      )
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
(let ((esc-quote    "\\\\\"")
      (plain-quote  "\"")
      (star-name "*[a-zA-Z0-9*-_]+")
      (arg-name "\\(?:^\\|\\s-\\)*\\([A-Z0-9-]+\\)\\(?:\\s-\\|$\\)+")
      (nm)
      (ln)
      (case-fold-search nil))
  (goto-char (point-min))
  (while (re-search-forward esc-quote nil t)
    (replace-match plain-quote))
  (goto-char (point-min))
  (while (re-search-forward star-name nil t)
    (replace-match (concat "`" (match-string 0) "`")))
  (goto-char (point-min))
  (while (re-search-forward arg-name nil t)
    (setq nm (match-string 1))
    (setq ln (downcase nm))
    (replace-match (concat "`" ln "`") t nil nil 1))
  ))
