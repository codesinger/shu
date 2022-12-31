
;;; shu-cpp-misc.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-misc
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

;;
;; A miscellaneous collection of useful functions

;;; Code:

(require 'subr-x)


(defconst shu-dired-mode-name "Dired by date"
  "The name of the mode for a dired buffer")

(defvar shu-srs-last-replace nil
  "Ths holds the last string that was passed to shu-srs.  It is remembered here and used as the
prompt for subsequent invocations of shu-srs")




;;
;;  Control the EOL terminator
;;
(defun shu-set-buffer-eol-type (eol-type)
  "Define what the end of line delimiter is in a text file."
  (setq buffer-file-coding-system (coding-system-change-eol-conversion
                                   buffer-file-coding-system eol-type))
  (set-buffer-modified-p t)
  (force-mode-line-update))


;;
;;  shu-set-unix-eol
;;
(defun shu-set-unix-eol ()
  "Set the end of line delimiter to be the Unix standard (LF)."
  (interactive)
  (shu-set-buffer-eol-type 'unix))


;;
;;  shu-set-dos-eol
;;
(defun shu-set-dos-eol ()
  "Set the end of line delimiter to be the DOS standard (CRLF)."
  (interactive)
  (shu-set-buffer-eol-type 'dos))


;;
;;  shu-set-mac-eol
;;
(defun shu-set-mac-eol ()
  "Set the end of line delimiter to be the Mac standard (CR)."
  (interactive)
  (shu-set-buffer-eol-type 'mac))


;;
;; shu-trim-trailing-blanks
;;
(defun shu-trim-trailing-blanks ()
  "Eliminate whitespace at ends of all lines in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t][ \t]*$" nil t)
      (delete-region (match-beginning 0) (point)))))

;;
;;  shu-quit - Exit emacs
;;
(defun shu-quit ()
  "Invoke save-buffers-kill-emacs.  This is the function normally
invoked by C-x C-c"
  (interactive)
  (save-buffers-kill-emacs))


;;
;;  shu-disabled-quit - Explain that C-x C-c does not do anything
;;
(defun shu-disabled-quit ()
  "Explain that C-x C-c no longer kills emacs.  Must M-x quit instead.
Far too often, I hit C-x C-c by mistake and emacs vanishes.  So I map
C-x C-c to this function and use an explicit M-x quit to exit emacs."
  (interactive)
  (beep)
  (message "C-x C-c has been disabled.  To exit emacs use M-x quit."))


;;
;;  Save the current file and load it as a .el file
;;
(defun shu-save-and-load ()
  "Save and load the current file as a .el file."
  (interactive)
  (if (not (buffer-file-name))
      (message "Current buffer has no file.")
    (save-buffer)
    (load-file (buffer-file-name))
    ))


;;
;;  shu-winpath
;;
(defun shu-winpath (start end)
  "Take marked region, put in kill ring, changing / to \.
This makes it a valid path on windows machines."
  (interactive "r")
  (copy-region-as-kill start end)
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    (while (search-forward "/" nil t)
      (replace-match "\\" nil t))
    (kill-ring-save (point-min) (point-max))))



;;
;;  shu-gf
;;
(defun shu-gf()
  "While in dired, put the full path to the current file in the kill ring"
  (interactive)
  (let ((name   (dired-get-filename)))
    (shu-kill-new name)
    (message "%s" name)
    ))

;;
;;  shu-of
;;
(defun shu-of()
  "While in dired, open the current file (Mac OS X only)"
  (interactive)
  (let ((name     (dired-get-filename)))
    (call-process "open" nil nil nil name)))


;;
;;  shu-gd
;;
(defun shu-gd()
  "While in dired, put the full path to the current directory in the kill ring"
  (interactive)
  (let ((name   (dired-current-directory)))
    (shu-kill-new name)
    (message "%s" name)
    ))


;;
;;  shu-gfn
;;
(defun shu-gfn()
  "While in a file buffer, put the name of the current file into the kill ring."
  (interactive)
  (let ((name  (file-name-nondirectory (buffer-file-name))))
    (if (not name)
        (ding)
      (shu-kill-new name)
      (message "%s" name))
    ))


;;
;;  shu-gfl
;;
(defun shu-gfl()
  "While in a file buffer, put both the current line number and the name of the current
file into the kill ring in the form of \"line 1234 of foo.cpp\"."
  (interactive)
  (let
      ((name  (file-name-nondirectory (buffer-file-name)))
       (fline))
    (if (not name)
        (ding)
      (setq fline (format "line %d of %s" (shu-current-line) name))
      (shu-kill-new fline)
      (message "%s" fline))
    ))


;;
;;  shu-gfc
;;
(defun shu-gfc()
  "While in a file buffer, put both the current line number and
column number and the name of the current file into the kill ring
in the form of \"foo.cpp:123:2\"."
  (interactive)
  (let
      ((name  (file-name-nondirectory (buffer-file-name)))
       (curpos (1+ (current-column)))
       (fline))
    (if (not name)
        (ding)
      (setq fline (format "%s:%d:%d" name (shu-current-line) curpos))
      (shu-kill-new fline)
      (message "%s" fline))
    ))


;;
;;  shu-gquote
;;
(defun shu-gquote ()
  "Insert a LaTeX quote environment and position the cursor for typing the quote."
  (interactive)
  (let ((ip))
    (insert "\\begin{quote}\n\n")
    (setq ip (point))
    (insert "\n\n\\end{quote}\n")
    (goto-char ip)
    ))


;;
;;  shu-new-latex
;;
(defun shu-new-latex()
  "Build a skeleton, empty LaTeX file."
  (interactive)
  (let ((p))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))

        (insert
         (concat
          "\\documentclass[12pt,onecolumn]{article}\n"
          "\\usepackage{setspace}\n"
          "\\newcommand{\\shuspace}{\\begin{spacing}{2.00}}\n"
          "\\newcommand{\\shuendspace}{\\end{spacing}}\n"
          "%\\newcommand{\\shuspace}{}\n"
          "%\\newcommand{\\shuendspace}{}\n"
          "\n"
          "\\begin{document}\n"
          "\n"
          "\\shuspace{}\n"
          "\n"))
        (setq p (point))
        (insert
         (concat
          "\n"
          "\n"
          "\\shuendspace{}\n"
          "\n"
          "\\end{document}\n"))))
    (goto-char p)
    ))


;;
;;  shu-new-lisp
;;
(defun shu-new-lisp (func-name)
  "Insert at point a skeleton lisp function.  Prompt is issued for the function
name."
  (interactive "*sFunction name?: ")
  (shu-internal-new-lisp "defun" func-name t t)
  )


;;
;;  shu-new-ert
;;
(defun shu-new-ert (func-name)
  "Insert at point a skeleton lisp ert unit test.  Prompt is issued for the
function name."
  (interactive "*sFunction name?: ")
  (shu-internal-new-lisp "ert-deftest" func-name)
  )


;;
;;  shu-internal-new-lisp
;;
(defun shu-internal-new-lisp (func-type func-name &optional doc-string interactive)
  "Insert at point a skeleton lisp function of type FUNC-TYPE whose name is
FUNC-NAME.  FUNC-TYPE is not examined in any way but is only useful if its
value is \"defun\", \"defmacro\", \"ert-deftest\", etc.  If INTERACTIVE is
true, the function is interactive."
  (let (
        (p)
        )
    (insert
     (concat
      "\n"
      ";;\n"
      ";;  " func-name "\n"
      ";;\n"
      "(" func-type " " func-name " ()\n"))
    (when doc-string
      (insert
       "  \"Doc string.\"\n"))
    (when interactive
      (insert "  (interactive)\n"))
    (insert
     (concat
      "  (let (\n"
      "        )\n"
      "    "))
    (setq p (point))
    (insert  "\n    ))\n")
    (goto-char p)
    ))



;;
;;  shu-new-lisp-while
;;
(defun shu-new-lisp-while (var-name)
  "Insert at point a skeleton lisp while loop.  Prompt is issued for the
variable name.  The while loop is of the form:

     (while x

       (setq x (cdr x))
       )
point is placed where the the first line of code in the loop belongs."
  (interactive "*sVariable name?: ")
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (insert (concat
             "(while " var-name "\n"
             pad "  "))
    (setq start (point))
    (insert (concat
             "\n"
             pad "  (setq "var-name " (cdr " var-name "))\n"
             pad  "  )\n"))
    (goto-char start)
    ))



;;
;;  shu-misc-rx-functions
;;
(defconst shu-misc-rx-functions
  (concat
   "("
   "\\s-*"
   "\\(defun"
   "\\|defsubst"
   "\\|defmacro"
   "\\|ert-deftest"
   "\\|defvar"
   "\\|defconst"
   ")"
   "\\)")
  "Regular expression to find the beginning of a function, macro, etc.")



;;
;;  shu-misc-rx-conditionals
;;
(defconst shu-misc-rx-conditionals
  (concat
   "("
   "\\s-*"
   "\\(cond"
   "\\|if"
   "\\|ignore-errors"
   "\\|progn"
   "\\|save-current-buffer"
   "\\|save-excursion"
   "\\|save-mark-and-excursion"
   "\\|save-match-data"
   "\\|save-restriction"
   "\\|unless"
   "\\|when"
   "\\|while"
   "\\|with-demoted-errors"
   "\\|with-temp-buffer"
   "\\)")
  "Regular expression to find the beginning of a function or macro that encloses
a body.  Such functions usually require a future closing parenthesis that is
likely not on the current line.  This is used by the functions SHU-TIGHTEN-LISP
and SHU-LOOSEN-LISP.")



;;
;;  shu-misc-rx-lets
;;
(defconst shu-misc-rx-lets
  (concat
   "("
   "\\s-*"
   "\\(let"
   "\\|let\\*"
   "\\)"
   shu-all-whitespace-regexp "*"
   "\\((\\)"
   )
  "Regular expression to find the beginning of a let special form.
This searches for \"let\" or \"let*\" followed by \"(\".")



;;
;;  shu-get-containing-function
;;
(defun shu-get-containing-function ()
  "Search backwards from the current point to find the beginning of the enclosing
function, macro, etc.  If such a beginning is found, return a cons cell whose car
is the point that defines the point at the beginning of the function and whose cdr
defines the point at the end of the function.  If not inside a function, macro, etc.,
return nil"
  (let ((ret-val)
        (bof)
        (eof))
    (save-excursion
      (if (not (re-search-backward shu-misc-rx-functions nil t))
          (progn
            (ding)
            (message "%s" "Not inside a macro or function"))
        (setq bof (match-beginning 0))
        (setq eof (shu-point-at-sexp bof))
        (setq ret-val (cons bof eof))))
    ret-val
    ))



;;
;;  shu-tighten-lisp
;;
(defun shu-tighten-lisp ()
  "Within the bounds of a lisp function or macro, \"tighten\" some lisp code.
Look for any single right parenthesis that is on its own line and move it up to
the end of the previous line.  This function is the opposite of SHU-LOOSEN-LISP"
  (interactive)
  (let ((ret-val)
        (bof)
        (eof)
        (doing t)
        (p)
        (start-pos)
        (end-pos)
        (length)
        (pad)
        (pad-length)
        (start-col)
        (let-begin)
        (sp)
        (xx (concat shu-all-whitespace-regexp "*" ")"))
        (zz (concat shu-all-whitespace-regexp "*" "(")))
    (save-excursion
      (setq ret-val (shu-get-containing-function))
      (when ret-val
        (setq bof (car ret-val))
        (setq eof (cdr ret-val))
        ;; Handle all containing functions other than "let"
        (goto-char bof)
        (while doing
          (if (not (re-search-forward shu-misc-rx-conditionals eof t))
              (setq doing nil)
            (setq p (1- (point)))
            (when (search-backward "(" nil t)
              (setq eof (shu-tighten-hanging-paren eof))
              (goto-char p))))
        ;; Handle "let" and "let*"
        (goto-char bof)
        (while (re-search-forward shu-misc-rx-lets eof t)
          (setq start-pos (point))
          (backward-char 1)
          (setq eof (shu-tighten-hanging-paren eof))
          (goto-char start-pos)
          (when (re-search-forward zz eof t)
            (setq end-pos (1- (point)))
            (setq length (- end-pos start-pos))
            (when (> length 0)
              (delete-region start-pos end-pos)
              (setq eof (- eof length)))))))
    ))




;;
;;  shu-tighten-hanging-paren
;;
(defun shu-tighten-hanging-paren (eof)
  "Call this function while point is on a left parenthesis.  This function will
find the matching right parenthesis.  If the matching right parenthesis is on a line
by itself and a previous line ends in another right parenthesis, the line and
dangling right parenthesis will be moved up to the end of the line that also ends
in a right parenthesis.  This is an internal part of the function SHU-TIGHTEN-LISP.
EOF is the point at which the current function on which we are operating ends.
This function removes some text from the current function.  It adjusts EOF appropriately
and returns the new value to the caller."
  (interactive)
  (let ((sp)
        (end-pos)
        (start-pos)
        (xx (concat shu-all-whitespace-regexp "*" ")"))
        (p)
        (length))
    (forward-sexp)
    (backward-char 1)
    (setq sp (shu-starts-with ")"))
    (when sp
      (setq end-pos sp)
      (when (re-search-backward xx p t)
        (setq start-pos (1+ (point)))
        (setq length (- end-pos start-pos))
        (delete-region start-pos end-pos)
        (setq eof (- eof length))))
    eof
    ))



;;
;;  shu-loosen-lisp
;;
(defun shu-loosen-lisp ()
  "Within the bounds of a lisp function, unwind the parentheses that terminate
conditional and containing functions such that it is convenient to insert code
inside of them without having to worry about which line contains the closing
parenthesis.  All closing parentheses are now on separate lines.  Once the
changes to the function are complete, you can run SHU-TIGHTEN-LISP to put the
parentheses back where they belong."
  (interactive)
  (let ((ret-val)
        (bof)
        (eof)
        (doing t)
        (p)
        (pad)
        (pad-length)
        (start-col)
        (let-begin))
    (save-excursion
      (setq ret-val (shu-get-containing-function))
      (when ret-val
        (setq bof (car ret-val))
        (setq eof (cdr ret-val))
        ;; Handle all containing functions other than "let"
        (goto-char bof)
        (while doing
          (if (not (re-search-forward shu-misc-rx-conditionals eof t))
              (setq doing nil)
            (setq p (1- (point)))
            (goto-char (match-beginning 0))
            (setq start-col (current-column))
            (beginning-of-line)
            (forward-sexp)
            (backward-char 1)
            (setq pad-length (+ start-col 2))
            (setq pad (concat "\n" (make-string pad-length ? )))
            (insert pad)
            (setq eof (+ eof (length pad)))
            (goto-char p)))
        ;; Handle "let" and "let*"
        (goto-char bof)
        (while (re-search-forward shu-misc-rx-lets eof t)
          (setq let-begin (match-beginning 0))
          (setq p (point))
          (setq start-col (current-column))
          (setq pad-length start-col)
          (setq pad (concat "\n" (make-string pad-length ? )))
          (insert pad)
          (setq eof (+ eof (length pad)))
          (goto-char p)
          (when (re-search-backward "(\\s-*" let-begin t)
            (forward-sexp)
            (backward-char 1)
            (setq pad-length (+ start-col 2))
            (setq pad (concat "\n" (make-string pad-length ? )))
            (insert pad)
            (setq eof (+ eof (length pad)))))))
    ))



;;
;;  shu-dup - Insert a duplicate of the current line following it.
;;
(defun shu-dup ()
  "Insert a duplicate of the current line, following it."
  (interactive)
  (save-excursion
    (let* ((xline                        ; Get copy of current line
	    (buffer-substring
	     (progn (beginning-of-line) (point))
	     (progn (end-of-line) (point))))
           (len (length xline))          ; Get length of current line
           (fill-column (max fill-column (1+ len)))) ; Keep fill from truncating
      (end-of-line)
      (newline)
      (insert xline))))


;;
;;  shu-reverse-comma-names
;;
(defun shu-reverse-comma-names ()
  "In a list of names, change all occurrences
of Lastname, Firstname to Firstname Lastname.
Position to the start of the file and invoke once."
  (interactive)
  (let
      ((ss "\\s-*\\([a-zA-Z\\s-]*\\),\\s-*\\([a-z0-9A-Z\\.\\,\\ ]*\\)\n")
       (last-name "")
       (first-name "")
       (ostring    nil)
       (ccount 0))
    (while (re-search-forward ss (buffer-size) t)
      (progn
        (setq last-name (match-string 1))
        (setq first-name (match-string 2))
        (setq ostring (concat first-name " " last-name "\n"))
        (replace-match ostring t t nil)
        (setq ccount (1+ ccount))))
    (message (concat "Converted " ccount " occurrences."))))

;;
;;
;;  Explanation of the regexp used above.  First you should
;;  read the string as ...
;;
;; \s-*\([a-zA-Z\s-]*\),\s-*\([a-z0-9A-Z\.\,\ ]*\)\n
;;
;; The double backslashes are required by Lisp but confuse the reading
;;
;; The first part {\s-*\([a-zA-Z\s-]*\),} says
;;
;;  - Match any number of white space chars {\s-*}
;;  - Followed by any number of the chars
;;           alphabet or white space {\([a-zA-Z\s-]*\)}
;;  - Followed by a comma {,}
;;
;; The next part {\s-*\([a-z0-9A-Z\.\,\ ]*\)\n} says
;;
;;  - Match any number of white space chars {\s-*}
;;  - Followed by any number of the chars alphabet, number,
;;           period, comma, space  {\([a-z0-9A-Z\.\,\ ]*\)}
;;  - Followed by a new line {\n}
;;

;;
;;
;;  shu-comma-names-to-letter
;;
(defun shu-comma-names-to-letter ()
  "In a list of names, change all occurrences
of Lastname, Firstname to an empty Latex letter.
Position to the start of the file and invoke once."
  (interactive)
  (let
      ((ss "\\s-*\\([a-zA-Z\\s-]*\\),\\s-*\\([a-z0-9A-Z\\.\\,\\ ]*\\)\n")
       (last-name "")
       (first-name "")
       (ostring nil)
       (ccount 0))
    (while (re-search-forward ss (buffer-size) t)
      (setq last-name (match-string 1))
      (setq first-name (match-string 2))
      (setq ostring
            (concat
             "\n%---------------------------------------------------------------\n"
             "\\begin{letter}{%\n"
             first-name " " last-name "\\\\}\n\n"
             "\\opening{Dear " first-name ",}\n\n\n"
             "\\closing{All the best,}\n\n"
             "\\end{letter}\n"))
      (replace-match ostring t t nil)
      (setq ccount (1+ ccount)))
    (message (concat "Converted " ccount " occurrences."))))


;;
;;  Move point straight down
;;
(defun shu-move-down (arg)
  "Move point vertically down.  Whitespace in any direction is made if
necessary.  New lines will be added at the end of a file and lines that are
too short will be expanded as necessary."
  (interactive "p")
  (let ((scol (current-column)))        ; Remember current column
    (while (> arg 0)                    ; Loop through the line count
      (shu-forward-line)              ; Move down, adding a line if needed
      (setq arg (1- arg)))            ; Decrement the loop count
    (move-to-column scol t)))           ; Move to the original column


(defun shu-forward-line ()
  "Move forward by one line.  If there is a next line, point it moved into
it.  If there are no more lines, a new one is created."
  (let ((sline (shu-current-line)))     ; Starting line
    (forward-line)                      ; Try to move down one
    (when (= sline (shu-current-line))  ; Did not actually move
      (end-of-line)                  ; Add a new line to end
      (newline))))                   ;  of the file


;;
;;  shu-put-line-near-top
;;
(defun shu-put-line-near-top ()
  "Take the line containing point and position it approximately five lines
from the top of the current window."
  (interactive)
  (let ((targ-point (point))            ; Point to reposition
        (targ-line (shu-current-line))  ; Line to reposition
        (wh (1- (window-height)))       ; Displayable window height
        (midp 0)                        ; Mid-point of window height
        (addp 0)                        ; Lines to add
        (add-line 0)
        (scol (current-column)))        ; Remember column we are in
    (setq midp (/ wh 2))                ; Midpoint of window
    (setq addp (- midp 4))              ; Lines to add to target
    (if (< addp 0)                      ; Do not add
        (setq addp 0)                     ;  a negative count
      )
    (setq add-line (+ targ-line addp))
    (goto-char (point-min))
    (forward-line (1- add-line))
    (recenter)                          ; Recenter here
    (goto-char (point-min))
    (forward-line (1- targ-line))
    (move-to-column scol t)))           ; Move to the original column


;;
;; shu-kill-current-buffer
;;
(defun shu-kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))


;;
;; shu-eob
;;
(defun shu-eob()
  "Go to end of buffer without setting mark.  Like end-of-buffer
but does not set mark - just goes there."
  (interactive)
  (goto-char (point-max)))


;;
;;  shu-local-replace
;;
(defun shu-local-replace(from-string to-string)
  "Replaces FROM-STRING with TO-STRING anywhere found in the buffer.
This is like replace-string except that it is intended to be called
by lisp programs.  Note that this function does not alter the value of
case-fold-search.  The user should set it before calling this function."
  (goto-char (point-min))
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))


;;
;;   shu-get-current-line
;;
(defun shu-get-current-line()
  "Return the current line in the buffer as a string"
  (let*
      ((bol (save-excursion (beginning-of-line) (point)))
       (eol (save-excursion (end-of-line) (point)))
       (the-line (buffer-substring bol eol)))
    the-line))


;;
;;  shu-shift-region-of-text
;;
(defun shu-shift-region-of-text (count start end)
  "Shift a region of text left or right.  The test to be shifted is defined
by the bounds of lines containing point and mark.  The shift count is
read from the minibuffer."
  (interactive "*nShift count: \nr")    ; Read the count, region is marked
  (let (
        (eor (save-excursion (goto-char end) (end-of-line) (point)))
        )
    (save-excursion
      (let ((sline (shu-the-line-at start))
            (eline (shu-the-line-at end))
            (line-diff 0))
        (goto-char start)
        (while (and (<= (shu-current-line) eline) (= line-diff 0))
          (shu-shift-single-line count)
          (setq line-diff (forward-line 1)))))
    ))



;;
;;  shu-shift-line
;;
(defun shu-shift-line (count)
  "Shift a line of text left or right by COUNT positions.  Shift right
if COUNT is positive, left if COUNT is negative.  Shifting left only
eliminates whitespace.  If there is a non-whitespace character in column
5, then shift by -10 will only shift left 4."
  (interactive "*nShift count: ")
  (shu-shift-single-line count)
  )



;;
;;  shu-shift-single-line
;;
(defun shu-shift-single-line (count)
  "Shift a line of text left or right by COUNT positions.  Shift right
if COUNT is positive, left if COUNT is negative.  Shifting left only
eliminates whitespace.  If there is a non-whitespace character in column
5, then shift by -10 will only shift left 4."
  (beginning-of-line)
  (if (> count 0)                ; We are shifting right
      (insert-char ?\  count)    ; Shift it right
    ;;                           ; Not positive count
    (when (< count 0)            ; We are shifting left
      (delete-char (shu-minimum-leading-space (- count)))))
  )


;;
;; shu-remove-test-names
;;
(defun shu-remove-test-names ()
  "Remove from a file all lines that contain file names that end in .t.cpp"
  (interactive)
  (let ((suffix "[a-zA-Z_$0-9]+\\.t\\.cpp[^a-zA-Z_$0-9]")
        (bol)
        (eol)
        (it)
        (count 0)
        (lines (count-lines (point-min) (point-max))))
    (goto-char (point-min))
    (while (re-search-forward suffix nil t)
      (forward-char -1)
      (setq bol (save-excursion (beginning-of-line) (point)))
      (setq eol (save-excursion (beginning-of-line) (forward-line 1) (point)))
      (setq count (1+ count))
      (setq it (buffer-substring-no-properties bol eol))
      (delete-region bol eol))
    (message "%d of %d lines deleted" count lines)
    ))




;;
;;  shu-git-number-commits
;;
(defun shu-git-number-commits ()
  "In a git log buffer, number all of the commits with zero being the most
recent.

It is possible to refer to commits by their SHA-1 hash.  If you want to see the
difference between two commits you can ask git to show you the difference by
specifying the commit hash of each one.  But this is cumbersome.  It involves
copying and pasting two SHA-1 hashes.  Once the commits are numbered, then
SHU-GIT-DIFF-COMMITS may be used to diff two commits by number.  See the
documentation for SHU-GIT-DIFF-COMMITS for further information.

This function counts as a commit any instance of \"commit\" that starts at the
beginning of a line and is followed by some white space and a forty character
hexadecimal number.  Returns the count of the number of commits found."
  (interactive)
  (let ((ss "^\\(commit\\)\\s-+[0-9a-f]\\{40\\}")
        (count 0)
        (cn))
    (goto-char (point-min))
    (while (re-search-forward ss nil t)
      (setq cn (shu-format-num count 6))
      (replace-match (concat cn ". " (match-string 0)))
      (setq count (1+ count)))
    (goto-char (point-min))
    (message "Numnbered %d commits" count)
    count
    ))



;;
;;  shu-fix-times
;;
(defun shu-fix-times ()
  "Go through a buffer that contains timestamps of the form
     YYYY-MM-DDTHHMMSS.DDD
converting them to the form
     YYYY-MM-DD HH:MM:SS.DDD
The latter is a format that Microsoft Excel can import."
  (interactive)
  (let ((rdate "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)T")
        (rtime "\\([0-9]\\{6\\}.[0-9]\\{3\\}\\)")
        (date)
        (hh)
        (mm)
        (ss)
        (ddd)
        (ftime)
        (ttime)
        (count 0)
        (ncount))
    (while (re-search-forward rdate nil t)
      (setq date (concat (match-string 1) " "))
      (replace-match date t t)
      (re-search-forward rtime nil t)
      (setq ftime (match-string 1))
      (setq hh (substring ftime 0 2))
      (setq mm (substring ftime 2 4))
      (setq ss (substring ftime 4 6))
      (setq ddd (substring ftime 7))
      (setq ttime (concat hh ":" mm ":" ss "." ddd))
      (replace-match ttime t t)
      (setq count (1+ count))
      )
    (setq ncount (shu-group-number count))
    (message "Replaced %s occurrences" ncount)
    ))




;;
;;  shu-split-range-string
;;
(defun shu-split-range-string (range-string)
  "RANGE-STRING is a string that contains either one or two numbers, possibly
separated by plus, minus, or period.  If one number then it is the starting number
and there is no ending number.  If two numbers then the first number is the start.
The operator in the middle determines the end.  If plus, then the end is the
second number added to the first.  If minus, then the end is the second number
subtracted from the first.  If period, then the end is the second number.

Return the two numbers as a cons cell (start . end).  If there is no end then the
cdr of the cons cell is nil.  If range string is not numeric, then both the car
and the cdr of the cons cell are nil.

For example, \"99+2\" has start 99 and end 101.  \"99-2\" has start 99 and end 97.
\"99.103\" has start 99, end 103.  \"98\" has start 98 and end is nil."
  (let ((s-one "[0-9]+")
        (s-two "\\([0-9]+\\)\\(\\+\\|\\-\\|\\.\\)\\([0-9]+\\)")
        (start)
        (end)
        (op)
        (second)
        (range))
    (if (string-match s-two range-string)
        (progn
          (setq start (string-to-number (match-string 1 range-string)))
          (setq op (match-string 2 range-string))
          (setq second (string-to-number (match-string 3 range-string)))
          (cond
           ((string= "+" op)
            (setq end (+ start second))
            )
           ((string= "-" op)
            (setq end (- start second))
            )
           ((string= "." op)
            (setq end second)))
          (setq range (cons start end)))
      (if (string-match s-one range-string)
          (progn
            (setq start (string-to-number (match-string 0 range-string)))
            (setq range (cons start nil)))
        (setq range (cons nil nil))))
    range
    ))




;;
;;  shu-find-numbered-commit
;;
(defun shu-find-numbered-commit (commit-number)
  "Search through a numbered git commit log looking for the commit whose number is
COMMIT-NUMBER.  Return the SHA-1 hash of the commit if the commit number is found.
Return nil if no commit with the given number is found.
The commit log is assume to have been numbered by shu-git-number-commits."
  (let ((ss-1 "\\s-*")
        (ss-2 "\\.\\s-*commit\\s-+\\([0-9a-f]\\{40\\}\\)")
        (sss)
        (commit-hash))
    (save-excursion
      (goto-char (point-min))
      (setq sss (concat ss-1 (number-to-string commit-number) ss-2))
      (when (re-search-forward sss nil t)
        (setq commit-hash (match-string 1))))
    commit-hash
    ))



;;
;;  shu-git-diff-commits
;;
(defun shu-git-diff-commits (commit-range)
  "In a buffer that is a numbered git log, query for a range string, find the two
commits, and put into the kill ring a git diff command specifying the two commits.

For example, given the following two numbered commits:

    31. commit 38f25b6769385dbc3526f32a75b97218cb4a6754
    33. commit 052ee7f4297206f08d44466934f1a52678da6ec9

if the commit range specified is either \"31.33\" or \"31+2\", then the following
is put into the kill ring:

    \"git diff -b 38f25b6769385dbc3526f32a75b97218cb4a6754..052ee7f4297206f08d44466934f1a52678da6ec9 \""
  (interactive "sCommits in the form x.y or x+y or x-y?: ")
  (let ((range)
        (start)
        (end)
        (commit-1)
        (commit-2)
        (scommit-1)
        (scommit-2)
        (diff))
    (setq range (shu-split-range-string commit-range))
    (setq start (car range))
    (setq end (cdr range))
    (if (or (not start)
            (not end))
        (progn
          (message "%s" "Invalid range specified")
          (ding))
      (setq commit-1 (shu-find-numbered-commit start))
      (setq commit-2 (shu-find-numbered-commit end))
      (setq scommit-1 (shu-git-find-short-hash commit-1))
      (when scommit-1
        (setq commit-1 scommit-1))
      (setq scommit-2 (shu-git-find-short-hash commit-2))
      (when scommit-2
        (setq commit-2 scommit-2))
      (setq diff (concat "git diff -b " commit-1 ".." commit-2 " "))
      (shu-kill-new diff))
    ))



;;
;;  shu-git-find-short-hash
;;
(defun shu-git-find-short-hash (hash)
  "Return the git short hash for the HASH supplied as an argument.  Return nil
if the given HASH is not a valid git revision."
  (let ((short-hash)
        (ss "^[a-fA-F0-9]\\{2,40\\}"))
    (with-temp-buffer
      (call-process "git" nil (current-buffer) nil "rev-parse" "--short" hash)
      (goto-char (point-min))
      (when (re-search-forward ss nil t)
        (setq short-hash (match-string 0))))
    short-hash
    ))



;;
;;  shu-git-add-file
;;
(defun shu-git-add-file (filename)
  "Do a \"git add\" for FILENAME.  Return empty string if add succeeds.  Otherwise,
return git error message."
  (let ((result))
    (with-temp-buffer
      (call-process "git" nil (current-buffer) nil "add" filename)
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    (setq result (shu-trim-trailing result))
    (when (not (string= result ""))
      (setq result (concat "After git add " filename ":\n" result)))
    result
    ))




;;
;;  shu-git-find-branch
;;
(defun shu-git-find-branch ()
  "Return the name of the current branch in a git repository."
  (let ((branch))
    (with-temp-buffer
      (call-process "git" nil (current-buffer) nil "rev-parse" "--abbrev-ref" "HEAD")
      (setq branch (buffer-substring-no-properties (point-min) (1- (point-max)))))
    branch
    ))




;;
;;  shu-git-find-default-branch
;;
(defun shu-git-find-default-branch ()
  "Return the name of the default branch in a git repository.  The default
branch is the one branch that is created with a new repository."
  (let ((branch)
        (fset (length "origin/")))
    (with-temp-buffer
      (call-process "git" nil (current-buffer) nil "rev-parse" "--abbrev-ref" "origin/HEAD")
      (setq branch (buffer-substring-no-properties (+ (point-min) fset) (1- (point-max)))))
    branch
    ))



;;
;;  shu-git-get-pr-url
;;
(defun shu-git-get-pr-url ()
  "Put into the kill ring the path required to create a new pull request for
the current branch of the current repository."
  (interactive)
  (let ((url-repo (shu-get-url-repo))
        (branch (shu-git-find-branch))
        (url))
    (setq url (concat url-repo "/pull/new/" branch))
    (shu-kill-new url)
    ))




;;
;;  shu-git-show-branch
;;
(defun shu-git-show-branch ()
  "Display the name of the current branch in a git repository."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (message "%s" branch)
    ))




;;
;;  shu-git-copy-branch
;;
(defun shu-git-copy-branch ()
  "Put the name of the current branch in a git repository into the kill ring."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (shu-kill-new branch)
    (message "%s" branch)
    ))




;;
;;  shu-git-insert-branch
;;
(defun shu-git-insert-branch ()
  "Insert at point the name of the current branch in a git repository"
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (insert branch)
    ))




;;
;;  shu-git-insert-origin-branch
;;
(defun shu-git-insert-origin-branch ()
  "Insert at point the name of the current branch in a git repository preceded by the
word \"origin\"..  This can be used as part of git push or pull."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (insert (concat "origin " branch))
    ))




;;
;;  shu-git-insert-push-origin-branch
;;
(defun shu-git-insert-push-origin-branch ()
  "Insert at point the git command to push the current branch out to origin.  If
the current branch is the default branch (fka \"master\"), you are prompted to
see if you want to proceed.  This is to prevent an accidental push to the
default branch."
  (interactive)
  (let ((branch (shu-git-find-branch))
        (insert-push t)
        (default-branch (shu-git-find-default-branch)))
    (when (string= branch default-branch)
      (setq insert-push (yes-or-no-p (concat "Really push to " branch " branch? "))))
    (when insert-push
      (insert (concat "git push origin " branch)))
    ))




;;
;;  shu-git-insert-pull-origin-branch
;;
(defun shu-git-insert-pull-origin-branch ()
  "Insert at point the name the git command to pull the current branch from
origin."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (insert (concat "git pull origin " branch))
    ))




;;
;;  shu-git-insert-checkout-default
;;
(defun shu-git-insert-checkout-default ()
  "Insert at point the git command to cheeck out the current default branch."
  (interactive)
  (let ((default-branch (shu-git-find-default-branch)))
      (insert (concat "git checkout " default-branch))
    ))




;;
;;  shu-git-insert-git-commit
;;
(defun shu-git-insert-git-commit ()
  "Insert at point the name the git command to commit with the commentary held
in a file called \"why.txt\"."
  (interactive)
  (insert "git commit -F why.txt")
  )



;;
;;  shu-conditional-find-file
;;
(defun shu-conditional-find-file (file-name)
  "Make the buffer for FILE-NAME the current buffer.  If FILE-NAME is already
loaded into a buffer, then make that the current buffer.  If FILE-NAME is not
loaded into a buffer, load the file into a buffer and make that the current
buffer.  Return true if this function created the buffer, nil otherwise.

This function is intended to handle the situation in which a function wants
to visit the contents of several files but does not want to leave behind a
lot of file buffers that it created.

If this function returns true, then the calling function should kill the
buffer when it is finished with it."
  (interactive)
  (let ((fbuf (get-buffer file-name))
        (file-buf))
    (if fbuf
        (setq file-buf fbuf)
      (setq file-buf (find-file-noselect file-name)))
    (set-buffer file-buf)
    fbuf
    ))




;;
;;  shu-case-sensitive
;;
(defun shu-case-sensitive ()
  "Set the variable case-fold-search to nil to make searches and matches respect
case.  I can never remember which way to set case-fold-search, hence this
simple, little function."
  (interactive)
  (let ((csnot (if case-fold-search " not" "")))
    (setq case-fold-search nil)
    (message "Previous setting was%s case sensitive" csnot)
    ))



;;
;;  shu-case-insensitive
;;
(defun shu-case-insensitive ()
  "Set the variable case-fold-search to t to make searches and matches ignore
case.  I can never remember which way to set case-fold-search, hence this
simple, little function."
  (interactive)
  (let ((csnot (if case-fold-search " not" "")))
    (setq case-fold-search t)
    (message "Previous setting was%s case sensitive" csnot)
    ))



;;
;;  shu-number-lines
;;
(defun shu-number-lines ()
  "Insert in front of each line in the buffer its line number.  Starts
at point and continues to the end of the buffer."
  (interactive)
  (let ((pos (point))
        (eof)
        (x)
        (y)
        (wid))
    (setq eof (line-number-at-pos (point-max)))
    (setq y (number-to-string eof))
    (setq wid (length y))
    (save-excursion
      (while (/= eof (line-number-at-pos))
        (beginning-of-line)
        (setq x (line-number-at-pos))
        (insert
         (concat
          " " (shu-format-num x wid) ". "))
        (forward-line 1)))
    ))



;;
;;  shu-buffer-number-lines
;;
(defun shu-buffer-number-lines ()
  "Create a buffer whose name is derived from the file name of the current
buffer but with the string \"-numbered\" added to the name.  Thus \"foo.cpp\"
would become \"foo-numbered.cpp\" Into this new buffer, copy the contents of the
current file with each line prefixed with its line number.  This is designed for
those times when you want to copy snippets of code with the line number in front
of each line because you are commenting on code and want the person receiving
the comments to sea the line number in front of each line."
  (interactive)
  (let ((line-diff 0)
        (bf (buffer-file-name))
        (fn)
        (fx)
        (bn)
        (buf)
        (count)
        (eline)
        (line)
        (lno))
    (if (not bf)
        (progn
          (ding)
          (message "%s" "Buffer has no file name"))
      (setq fn (file-name-nondirectory (file-name-sans-extension bf)))
      (setq fx (file-name-extension bf))
      (setq bn (concat fn "-numbered.." fx))
      (setq buf (get-buffer-create bn))
      (setq count 0)
      (save-excursion
        (setq eline (shu-the-line-at (point-max)))
        (goto-char (point-min))
        (while (and (<= (shu-current-line) eline) (= line-diff 0))
          (setq count (1+ count))
          (setq lno (shu-format-num count 4))
          (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (setq line-diff (forward-line 1))
          (princ (concat lno ". " line "\n") buf))))
    ))




;;
;;  shu-add-prefix
;;
(defun shu-add-prefix (prefix)
  "Put a prefix and a space in front of each line in the region.  Prompt is issued
for the prefix."
  (interactive "sPrefix? ")
  (setq prefix (concat (shu-trim-trailing prefix) " "))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match (concat "\n" prefix)))
    ))




;;
;;  shu-de-star
;;
(defun shu-de-star ()
  "Remove leading spaces and asterisk from each line in the region.  This is
useful for editing doxygen comments of the form:

   /*!
    * This is some commentary.
    * This is more commentary, etc.
    */

You snip out the middle lines and put them into a text file for formatting and
spell-checking.  You want to get rid of all of the asterisks until you are
done.

This function gets rid of all the asterisks.  You can use SHU-ADD-PREFIX to
put them back."
  (interactive)
  (let ((ss-blank "^\\s-*\\* ")
        (ss-not "^\\s-*\\*"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ss-blank nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward ss-not nil t)
        (replace-match "")))
    ))



;;
;;  shu-modified-buffers
;;
(defun shu-modified-buffers ()
  "Show a list of all buffers associated with files whose status is modified.
It is not uncommon to have many emacs windows open and to realize that one
window has a file open and another window is also trying to edit it.  emacs
warns the second window of the conflict, but it is sometimes difficult to tell
which window holds the modified buffer.  The buffer list shows you all of the
buffers with an asterisk next to each modified buffer, but if the buffer list
is large, it can be difficult to find the one you seek.  This command lists
only modified buffers that hold the contents of a file."
  (interactive)
  (let ((bl (buffer-list))
        (buf)
        (buf-name)
        (buf-file-name)
        (mod-list)
        (gb))
    (while bl
      (setq buf (car bl))
      (setq buf-name (buffer-name buf))
      (when buf-name
        (setq buf-file-name (buffer-file-name buf))
        (when buf-file-name
          (when (buffer-modified-p buf)
            (push buf mod-list))))
      (setq bl (cdr bl)))
    (if (not mod-list)
        (message "%s" "No modified buffers")
      (setq gb (get-buffer-create "**shu-odified--buffers**"))
      (switch-to-buffer gb)
      (while mod-list
        (setq buf (car mod-list))
        (setq buf-name (buffer-name buf))
        (setq buf-file-name (buffer-file-name buf))
        (princ (concat buf-name "\n") gb)
        (princ (concat "    " buf-file-name "\n") gb)
        (setq mod-list (cdr mod-list)))
      (goto-char (point-min)))
    ))



;;
;;  shu-all-quit
;;
(defun shu-all-quit ()
  "Kill all dired buffers and all buffers that contain a file and are unmodified.
It is not uncommon to have dozens of buffers open that are unrelated to the current task
and this is a convenience function for closing many buffers that do not need to
be open.
If the function SHU-CLEAR-C-PROJECT is defined, it is called to clear the current
project."
  (interactive)
  (let ((gb (get-buffer-create "*shu-killed-buffers*"))
        (buf-list (buffer-list))
        (buf)
        (mod-list)
        (buf-fn)
        (buf-name)
        (buf-mode-name))
    (while buf-list
      (setq buf (car buf-list))
      (setq buf-mode-name (with-current-buffer buf (format-mode-line mode-name)))
      (if (string= buf-mode-name shu-dired-mode-name)
          (push buf mod-list)
        (setq buf-fn (buffer-file-name buf))
        (when buf-fn
          (when (not (buffer-modified-p buf))
            (push buf mod-list))))
      (setq buf-list (cdr buf-list)))
    (while mod-list
      (setq buf (car mod-list))
      (setq buf-mode-name (with-current-buffer buf (format-mode-line mode-name)))
      (setq buf-name (buffer-name buf))
      (princ "Killing buffer: " gb)(princ buf-name gb)
      (princ " (" gb)(princ buf-mode-name gb)(princ ")\n" gb)
      (kill-buffer buf)
      (setq mod-list (cdr mod-list)))
    (when (fboundp 'shu-clear-c-project)
      (shu-clear-c-project))
    ))




;;
;;  shu-erase-region
;;
(defun shu-erase-region (start end)
  "Replace everything in the region between START and END with blanks.  This is
exactly like delete-region except that the deleted text is replaced with spaces.
As with delete-region, the end point is not included in the delete.  It erases
everything up to but not including the end point.  The order of START and END
does not matter."
  (let ((spoint start)
        (epoint end)
        (len 0)
        (pad))
    (when (> spoint epoint)
      (setq spoint end)
      (setq epoint start))
    (setq len (- epoint spoint))
    (setq pad (make-string len ? ))
    (save-excursion
      (delete-region spoint epoint)
      (goto-char spoint)
      (insert pad))
    ))



;;
;;  shu-make-md-toc-entry
;;
(defun shu-make-md-toc-entry ()
  "The latest item in the kill ring is assumed to be the text of a markdown
section name.  This function creates from that section name, a markdown table of
contents entry and inserts that entry at point.

For example, if the kill ring contains \"## This is the Overview ##\", the table
of contents entry created and inserted at point will be

       * [This is the overview](#thisistheoverview)"
  (interactive)
  (let ((section-name (current-kill 0))
        (index-name)
        (toc-name))
    (setq toc-name (shu-make-md-section-name section-name))
    (setq index-name (shu-make-md-index-name toc-name))
    (beginning-of-line)
    (insert
     (concat
      " * [" toc-name "]"
      "(#" index-name ")"))
    ))




;;
;;  shu-make-md-name-entry
;;
(defun shu-make-md-name-entry ()
  "The latest item in the kill ring is assumed to be the text of a markdown
section name.  This function creates from that section name, a markdown table of
contents name that will identify the section in the table of contents.

For example, if the kill ring contains \"## This is the Overview\", the table
of contents name created and inserted at point will be:

        <a name=thisistheoverview></a>"
  (interactive)
  (let ((section-name (current-kill 0))
        (index-name)
        (toc-name))
    (setq toc-name (shu-make-md-section-name section-name))
    (setq index-name (shu-make-md-index-name toc-name))
    (insert
     (concat
      " <a name="
      index-name "></a>"))
    ))



;;
;;  shu-make-md-index-name
;;
(defun shu-make-md-index-name (name)
  "The input is a string that is assumed to be a markdown section heading from
a markdown table of contents.
The return value is an all lower case string with any whitespace characters
removed.
For example, if the input string is

     This is an Overview

The returned string would be

     thisisanoverview"
  (let ((section-name (shu-make-md-section-name name))
        (index-name)
        (no-index-chars "[.,:;()`]"))
    (with-temp-buffer
      (insert section-name)
      (goto-char (point-min))
      (while (re-search-forward shu-all-whitespace-regexp nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward no-index-chars nil t)
        (replace-match ""))
      (downcase-region (point-min) (point-max))
      (setq index-name (buffer-substring-no-properties (point-min) (point-max))))
    index-name
    ))



;;
;;  shu-make-md-section-name
;;
(defun shu-make-md-section-name (section-name)
  "The input is a string that is assumed to be a markdown section heading.  The
return value is a string with any leading and trailing \"#\" characters removed.
For example, if the input string is

     ## This is an Overview ##

The returned string would be

     This is an Overview"
  (with-temp-buffer
    (insert section-name)
    (goto-char (point-min))
    (while (search-forward "#" nil t)
      (replace-match ""))
    (setq section-name (shu-trim (buffer-substring-no-properties (point-min) (point-max)))))
  section-name)



;;
;;  shu-get-markdown-prefix
;;
(defun shu-get-markdown-prefix (section-heading)
  "Returns the pound sign prefix from a markdown section heading,
SECTION-HEADING.  The string of pound signs must begin at the beginning of the
string.  If a section heading is

   \"### This is a section heading\"

then the string \"###\" is returned.  If the first character in the section
heading is not a pound sign, nil is returned."
  (let ((ss "\\`\\([#]+\\)")
        (prefix))
    (when (string-match ss section-heading)
      (setq prefix (match-string 1 section-heading)))
    prefix
    ))



;;
;;  shu-get-markdown-heading
;;
(defun shu-get-markdown-heading (section-heading)
  "Returns the heading text from a markdown section heading, SECTION-HEADING.
There must be at least one pound sign at the beginning of the string.  If a
section heading is

   \"### This is a section heading\"

then the string \"This is a section heading\" is returned.  If the first
character in the section heading is not a pound sign, nil is returned."
  (let ((sh section-heading)
        (nsh)
        (ss "\\`[#]+")
        (heading))
    (when (string-match ss sh)
      (setq nsh (replace-match "" t t sh))
      (setq heading (shu-trim nsh)))
    heading
    ))



;;
;;  shu-get-markdown-level
;;
(defun shu-get-markdown-level (section-heading)
  "Return the level of a markdown section heading.  The level is defined as
the number of leading pound signs that start at the beginning of the string.
A level 1 heading begins with \"#\".  A level 2 heading begins with \"##\".
If there are no leading pound signs at the beginning of the string, a level of
zero is returned."
  (let ((prefix (shu-get-markdown-prefix section-heading))
        (level 0))
    (when prefix
      (setq level (length prefix)))
    level
    ))



;;
;;  shu-fix-markdown-section
;;
(defun shu-fix-markdown-section (max-depth)
  "On entry, point is positioned after one or more pound signs that define the
beginning of a markdown section heading.  If the number of pound signs is
greater than MAX-DEPTH, ignore the line and return nil.  If the number of
pound signs is less than or equal to MAX-DEPTH, fix the line as described
below and return it.

If the line ends with an expression that looks like

      \"<a name=currentliveupdate></a>\",

remove it.

If the line ends with trailing pound signs, remove them as well.

Then return the repaired line."
  (let* ((gb (get-buffer-create shu-trace-buffer))
         (ssa "\\s-*<a\s-*name")
         (sot (1+ (point)))
         (bol (line-beginning-position))
         (eol (line-end-position))
         (line (buffer-substring-no-properties bol eol))
         (level (shu-get-markdown-level line))
         (use-line (and (/= level 0) (<= level max-depth)))
         (son)
         (eon))
    (if (not use-line)
        (setq line nil)
      (if (re-search-forward ssa eol t)
          (progn
            (princ (concat "found [" (match-string 0) "]\n") gb)
            (setq son (match-beginning 0))
            (setq eol (line-end-position))
            (while (search-forward "</a>" eol t)
              (princ (concat "found [" (match-string 0) "]\n") gb)
              (setq eon (match-end 0))
              (delete-region son eon)
              (setq eol (line-end-position)))
            (setq line (buffer-substring-no-properties bol eol)))
        (goto-char eol)
        (while (search-backward "#" sot t)
          (replace-match ""))
        (setq eol (line-end-position))
        (setq line (shu-trim (buffer-substring-no-properties bol eol)))))
    line
    ))



;;
;;  shu-get-md-boundaries
;;
(defun shu-get-md-boundaries ()
  "Find all pairs of markdown literal text.  In markdown, the sequence ``` is
used to bound literal text.  When creating a markdown table of contents, we do
not want to look at pound signs contained in literal text.  This function finds
the location of each pair of ``` sentinels.  It returns a list of cons sells,
each of which has the start and end position of a ``` sequence.  If there is a
start ``` with no companion ``` close, it is not included in the list."
  (let ((sentinel "```")
        (looking-for-open t)
        (open)
        (close)
        (pair)
        (literals)
        (last-open)
        (msg)
        (balanced))
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (search-forward sentinel nil t)
          (if looking-for-open
              (progn
                (setq open (1- (point)))
                (setq last-open open)
                (setq looking-for-open nil))
            (setq close  (match-beginning 0))
            (setq pair (cons open close))
            (setq open nil)
            (setq close nil)
            (push pair literals)
            (setq looking-for-open t)))
        (if looking-for-open
            (setq balanced t)
          (setq msg (format "open literal (```) at line %d has no matching close"
                            (shu-the-line-at last-open)))
          (message "%s" msg))))
    (when literals
      (setq literals (sort literals (lambda(lhs rhs) (< (car lhs) (car rhs))))))
    literals
    ))



;;
;;  shu-md-in-literal
;;
(defun shu-md-in-literal (literals pt)
  "LITERALS is a list of markdown literal boundaries produced by
SHU-GET-MD-BOUNDARIES.  This function returns t if the point PT lies within a
markdown literal boundary."
  (let ((lits literals)
        (inside)
        (pair)
        (open)
        (close)
        (looking t))
    (when lits
      (while (and lits looking)
        (setq pair (car lits))
        (setq open (car pair))
        (setq close (cdr pair))
        (when  (and (> pt open) (< pt close))
          (setq inside t)
          (setq looking nil))
        (when (> close pt)
          (setq looking nil))
        (setq lits (cdr lits))))
    inside
    ))



;;
;;  shu-tocify-markdown-file
;;
(defun shu-tocify-markdown-file ()
  "Search the file starting at the current position for any markdown headings of
the form \"## This is a heading\".  Add a tag to each heading and then insert a
complete markdown table of contents at the current position.

Pound signs that lie inside of markdown literal areas designated by \"```\" are
ignored.  This prevents something such as an example of an #include directive
from being treated as a level 1 heading.

If a heading already has a tag, it is removed.  If a heading has trailing pound
signs, they are also removed.

The default maximum heading level is two, which means that heading levels
greater than two are not included in the table of contents.  But a numeric
prefix argument can change the maximum heading level.  The maximum heading level
cannot be set to a value less than one."
  (interactive)
  (let ((literals (shu-get-md-boundaries))
        (default-max-depth 2)
        (max-depth  current-prefix-arg)
        (ht (make-hash-table :test 'equal :size 500))
        (ss "^[#]+")
        (index-name-max 15)
        (suffix-length 6)
        (line)
        (heading)
        (index-name)
        (entry)
        (entries)
        (toc))
    (setq toc (point))
    (when (not max-depth)
      (setq max-depth default-max-depth))
    (when (< max-depth 1)
      (setq max-depth 1))
    (save-excursion
      (while (re-search-forward ss nil t)
        (when (not (shu-md-in-literal literals (point)))
          (setq line (shu-fix-markdown-section max-depth))
          (when line
            (setq heading (shu-get-markdown-heading line))
            (setq index-name (shu-make-md-index-name heading))
            (when (> (length index-name) index-name-max)
              (setq index-name (substring index-name 0 index-name-max)))
            (setq index-name (shu-misc-make-unique-string index-name suffix-length ht))
            (setq entry (cons line index-name))
            (push entry entries)))))
    (setq entries (nreverse entries))
    (insert "\n")
    (shu-insert-markdown-toc entries)
    (shu-tocify-markdown-headings entries)
    (goto-char toc)
    ))




;;
;;  shu-tocify-markdown-headings
;;
(defun shu-tocify-markdown-headings (entries)
  "ENTRIES is a list of cons cells.  The car of each item on the list is the
markdown heading line, which looks something like \"## This is a heading\".
The cdr of each item on the list is the link name.  This function searches for
each markdown heading in the file and appends to the heading a tag of the form

     <a name=link name></a>

so that it may be referenced from the table of contents."
  (let ((ents entries)
        (entry)
        (line)
        (index-name)
        (actual))
    (setq actual (buffer-substring-no-properties (point-min) (point-max)))
    (while ents
      (setq entry (car ents))
      (setq line (car entry))
      (setq index-name (cdr entry))
      (search-forward line nil)
      (insert (concat " <a name=" index-name "></a>"))
      (setq ents (cdr ents)))
    ))



;;
;;  shu-insert-markdown-toc
;;
(defun shu-insert-markdown-toc (entries)
  "ENTRIES is a list of cons cells.  The car of each item on the list is the
markdown heading line, which looks something like \"## This is a heading\".
The cdr of each item on the list is the link name.  This function inserts a
markdown table of contents in which each line in the table of contents
consists of the heading text in brackets followed by the line name in
parenthesis and preceded by a pound sign.  Each line that represents a heading
level greater than one is also indented to indicate its heading level."
  (let ((ents entries)
        (entry)
        (line)
        (index-name)
        (level)
        (x)
        (pad-count)
        (pad)
        (heading))
    (while ents
      (setq entry (car ents))
      (setq line (car entry))
      (setq index-name (cdr entry))
      (setq level (shu-get-markdown-level line))
      (setq x (1- level))
      (setq pad-count (1+ (* 4 x)))
      (setq pad (make-string pad-count ? ))
      (setq heading (shu-get-markdown-heading line))
      (insert (concat pad "- [" heading "](#" index-name ")\n"))
      (setq ents (cdr ents)))
    ))



;;
;;  shu-os-name
;;
(defun shu-os-name ()
  "Return a string with the name of the type of the host operating system."
  (let ((name
         (if (and
              (fboundp 'shu-system-type-is-mac-osx)
              (shu-system-type-is-mac-osx))
             "Mac OSX"
           (if (and
                (fboundp 'shu-system-type-is-unix)
                (shu-system-type-is-unix))
               "Unix"
             (if (and
                  (fboundp 'shu-system-type-is-windows)
                  (shu-system-type-is-windows))
                 "Windows"
               "Unknown")))))
    name
    ))



;;
;;  shu-show-os-name
;;
(defun shu-show-os-name ()
  "Display the name of the host operating system type.  This is a sanity check
function for the various functions defined in .emacs to determine the type of
the host operating system."
  (interactive)
  (message "%s" (shu-os-name))
  )



;;
;;  shu-system-name
;;
(defun shu-system-name ()
  "Return the machine name.  Prior to emacs 25.1, this was held in the variable
system-name.  As of emacs 25.1, system-name is now a function.  Return nil if
system-name is neither a function nor a variable."
  (let (
        (sys-name)
        )
    (if (fboundp 'system-name)
        (setq sys-name (system-name))
      (when (boundp 'system-name)
        (setq sys-name system-name)
        )
      )
    sys-name
    ))



;;
;;  shu-system-name-string
;;
(defun shu-system-name-string ()
  "Return the machine name.  Prior to emacs 25.1, this was held in the variable
system-name.  As of emacs 25.1, system-name is now a function.  Unlike
SHU-SYSTEM-NAME, this function always returns a string, even if the machine
name is not available for some reason."
  (let ((sys-name (shu-system-name)))
    (when (not sys-name)
      (setq sys-name "??????"))
    sys-name
    ))



;;
;;  shu-show-system-name
;;
(defun shu-show-system-name ()
  "Place the system name (machine name) in the message area."
  (interactive)
  (message "%s" (shu-system-name-string))
  )



;;
;;  shu-kill-system-name
;;
(defun shu-kill-system-name ()
  "Place the system name` (machine name) in the message area."
  (interactive)
  (setq debug-on-error t)
  (shu-kill-new (shu-system-name-string))
  )



;;
;;  shu-misc-split-string
;;
(defun shu-misc-split-string (input line-limit &optional fixed-width escape)
  "Split a string into multiple strings and return a list of the strings.  If
FIXED-WIDTH is true, then each returned string is LINE-LIMIT characters in
length, except for the last, which may be shorter.  If FIXED-WIDTH is absent or
nil, then each returned string is split on a word boundary and no string exceeds
LINE-LIMIT characters in length."
  (let ((lines))
    (with-temp-buffer
      (insert input)
      (setq lines (shu-misc-split-buffer line-limit fixed-width escape)))
    lines
    ))



;;
;;  shu-misc-split-buffer
;;
(defun shu-misc-split-buffer (line-limit &optional fixed-width escape)
  "Split an entire buffer into multiple strings and return a list of the
strings.  If FIXED-WIDTH is true, then each returned string is LINE-LIMIT
characters in length, except for the last, which may be shorter.  If FIXED-WIDTH
is absent or nil, then each returned string is split on a word boundary and no
string exceeds LINE-LIMIT characters in length."
  (let ((lines))
    (if fixed-width
        (setq lines (shu-misc-split-chunk-buffer line-limit escape))
      (setq lines (shu-misc-split-phrase-buffer line-limit)))
    ))



;;
;;  shu-misc-split-phrase-buffer
;;
(defun shu-misc-split-phrase-buffer (line-limit)
  "Split an entire buffer into multiple strings and return a list of the
strings.  Each returned string is split on a word boundary and no string exceeds
LINE-LIMIT characters in length."
  (shu-misc-internal-split-buffer line-limit 'shu-misc-get-phrase)
  )



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
;;  shu-misc-get-phrase
;;
(defun shu-misc-get-phrase (line-limit &optional escape)
  "Remove from the front of the current buffer and return the longest possible
string of whitespace separated things whose length does not exceed line-limit.
If there is at least one whitespace character before LINE-LIMIT, the string will
end with one or more whitespace characters.  i.e., the string will end on a word
boundary if that is possible.

Words will not be split unless there is no whitespace character before
LINE-LIMIT characters have been scanned, in which case a string of exactly
LINE-LIMIT length will be removed and returned.

This function is used to split a string of words into a set of smaller strings
such that words are not split."
  (let ((ss (concat shu-all-whitespace-regexp "+"))
        (sn (concat shu-not-all-whitespace-regexp "+"))
        (something t)
        (tpoint)
        (lpoint)
        (rpoint)
        (epoint)
        (part))
    (goto-char (point-min))
    (while something
      (setq tpoint (re-search-forward ss nil t))
      (if (not tpoint)
          (progn
            (if (and lpoint (> (point-max) line-limit))
                (setq epoint lpoint)
              (setq epoint line-limit))
            (setq part (shu-misc-get-chunk epoint))
            (setq something nil))
        (setq tpoint (1- tpoint))
        (when (> tpoint line-limit)
          (setq rpoint (re-search-backward sn nil t))
          (if (not rpoint)
              (setq epoint line-limit)
            (setq rpoint (1+ rpoint))
            (if (< rpoint line-limit)
                (setq epoint line-limit)
              (if lpoint
                  (setq epoint lpoint)
                (setq epoint line-limit))))
          (setq part (shu-misc-get-chunk epoint))
          (setq something nil)))
      (setq lpoint tpoint))
    part
    ))




;;
;;  shu-misc-get-chunk
;;
(defun shu-misc-get-chunk (line-limit &optional escape)
  "Return a string that consists of the first LINE-LIMIT characters in the
current buffer.  If LINE-LIMIT is larger than the buffer size, return a
string that is the entire contents of the buffer.  Before returning, delete
from the buffer the returned string."
  (let* ((spoint (point-min))
         (xpoint (+ line-limit spoint))
         (epoint (if (< (point-max) xpoint) (point-max) xpoint))
         (last-char)
         (part)
         (esc (if escape "t" "nil")))
    ;;    (goto-char (1- epoint))
    (when (and (> (point-max) 3) (> line-limit 3))
      (setq last-char (buffer-substring-no-properties (1- epoint) epoint))
      (when (and escape (string= last-char "\\"))
        (setq epoint (1- epoint))
        (setq last-char (buffer-substring-no-properties (1- epoint) epoint))
        (when (string= last-char "\\")
          (setq epoint (1- epoint)))))
    (setq part (buffer-substring-no-properties spoint epoint))
    (delete-region spoint epoint)
    part
    ))




;;
;;  shu-obfuscate-region
;;
(defun shu-obfuscate-region (start end)
  "Obfuscate a region of text by replacing every alphabetic character in the
region with the next letter of the alphabet, staring with 'a'. For example, of
the region contains

  Now is the time for all good men to come to the aid of the Party 10 times.

Then the obfuscated text would be:

  Abc de fgh ijkl mno pqr stuv wxy za bcde fg hij klm no pqr Stuvw 10 xyzab.

This is useful if you want to capture some text for later testing and
manipulation that might contain confidential or proprietary information.  This
is an encoding that cannot be reversed."
  (interactive "*r")
  (let ((current-char ?z)
        (new-char)
        (case-fold-search nil)
        (advance))
    (goto-char start)
    (while (/= (point) end)
      (setq advance nil)
      (if (looking-at "[a-z]")
          (progn
            (setq current-char (shu-next-char-in-seq current-char))
            (delete-char 1)
            (insert-char current-char))
        (if (looking-at "[A-Z]")
            (progn
              (setq current-char (shu-next-char-in-seq current-char))
              (delete-char 1)
              (setq new-char (upcase current-char))
              (insert-char new-char))
          (setq advance t)))
      (when advance
        (forward-char 1)))
    ))



;;
;;  shu-next-char-in-seq
;;
(defun shu-next-char-in-seq (current-char)
  "CURRENT-CHAR is a character in the range a-z (or A-Z).  This function returns
the next character, where next is the next character in the alphabet unless
CURRENT-CHAR is 'z', in which case the next character returned is 'a'.  If
CURRENT-CHAR is 'Z', then the next character returned is 'A'."
  (let ((next-char)
        (case-fold-search nil))
    (cond
     ((char-equal current-char ?z)
      (setq next-char ?a))
     ((char-equal current-char ?Z)
      (setq next-char ?A))
     (t
      (setq next-char (1+ current-char))))
    next-char
    ))



;;
;;  shu-reverse2
;;
(defun shu-reverse2 ()
  "When positioned in front of a pair of parenthesis that contains a pair of
expressions separated by a comma, reverse the positions of the two expressions.
The first becomes the second and the second becomes the first.
i.e.,
      foo(mumble, bar);
becomes
      foo(bar, mumble);"
  (interactive)
  (let ((eol (line-end-position))
        (rxnc "\\([^,]+\\),\\s-*")
        (spos)
        (epos)
        (arg1)
        (arg2))
    (save-excursion
      (if (not (search-forward "(" nil t))
          (progn
            (ding)
            (message "%s" "No opening parenthesis on this line"))
        (setq spos (point))
        (backward-char 1)
        (forward-sexp)
        (backward-char 1)
        (setq epos (point))
        (goto-char spos)
        (if (not (re-search-forward rxnc eol t))
            (progn
              (ding)
              (message "%s" "No first expression found inside parenthesis"))
          (setq arg1 (match-string 1))
          (setq arg2 (buffer-substring-no-properties (point) epos))
          (delete-region (point) epos)
          (insert arg1)
          (replace-match arg2 t t nil 1))))
    ))



;;
;;  shu-frame-width
;;
(defun shu-frame-width ()
  "Return the width of an emacs frame.  Different operating systems appear to
have slightly different windowing systems, which means that the
FRAME-INNER-WIDTH function does not quite report the exact width."
  (let ((frame-width (frame-inner-width))
        (fudge
         (if (shu-system-type-is-unix)
             8
           (if (shu-system-type-is-mac-osx)
               4
             0))))
    (+ frame-width fudge)
    ))




;;
;;  shu-adapt-frame
;;
(defun shu-adapt-frame (frame-no)
  "Adapt the current frame to the current display by stretching the frame to the
full height of the display and putting the top of the frame at the top of the
display.  This function makes some assumptions about the display geometry based
on the current operating system.  It assumes that Windows loses five lines for
top and bottom tool bars.  Mac OS X loses three lines for the top tool bar.
Unix loses two lines for something.  These numbers should, at some point, be
customizable.

With a numeric prefix argument N, the emacs window is positioned N frames from
the right hand side of the display.  For example, if you open three frames and
type into the first frame C-u 1 M-x SHU-ADAPT-FRAME, into the next frame
C-u 2 M-x SHU-ADAPT-FRAME, and into the third frame C-u 3 M-x SHU-ADAPT-FRAME,
then the three frames will be grouped together side by side at the right side of
the display.

If the prefix argument is large enough that the left side of the frame would be
moved past the left side of the display, the window is positioned such that the
left edge of the window is aligned with the left edge of the display.

Prefix arguments greater than 10 assume a two display system.  Prefix arguments
of 11 and 12 put two frames on the right diaplay.  Prefix arguments of 13 and
14 put two frames on the left display.

Implementation note:

If this function is called when the left side of the frame is positioned to the
left of the leftmost edge of the display, the function FRAME-POSITION returns a
negative value for the x coordinate of the frame.  The function
SET-FRAME-POSITION takes the x and y coordinates of the new position of the top
left corner of the frame.

But if x is negative, it specifies the coordinates of the right edge of the
frame relative to the right edge of the display.  This puts a frame that is very
close to the left edge of the display all the way over to the right edge of the
display.

The assumption is that a negative x frame position means that the user has
positioned the frame just a bit past the left edge and that the desired frame
position is actually the leftmost edge of the display."
  (interactive "P")
  (let* ((lost-lines
          (if (shu-system-type-is-windows)
              5
            (if (shu-system-type-is-mac-osx)
                3
              (if (shu-system-type-is-unix)
                  2
                2))))
         (width-fudge (if (shu-system-type-is-unix) 10 0))
         (top-y 0)
         (hpx (- (x-display-pixel-height) (* lost-lines (frame-char-height))))
         (hpl (/ hpx (frame-char-height)))
         (fp)
         (actual-x)
         (calc-x)
         (y)
         (frame-pixel-width (shu-frame-width))
         (display-pixel-width (x-display-pixel-width))
         (offset 0)
         (right-panel-x 2553)
         (left-panel-x 2547))
    (setq fp (frame-position))
    (setq actual-x (car fp))
    (setq calc-x actual-x)
    (setq y (cdr fp))
    (when frame-no
      (if (< frame-no 11)
          (progn
            (if (= frame-no 1)
                (setq offset frame-pixel-width)
              (setq offset (+ (* (1- frame-no) (- frame-pixel-width width-fudge)) frame-pixel-width)))
            (setq calc-x (- display-pixel-width offset)))
        (cond
         ((= frame-no 11)
          (setq calc-x (- (+ right-panel-x frame-pixel-width) width-fudge)))
         ((= frame-no 12)
          (setq calc-x right-panel-x))
         ((= frame-no 13)
          (setq calc-x (+ (- left-panel-x frame-pixel-width) width-fudge)))
         ((= frame-no 14)
          (setq calc-x (+ (- left-panel-x (+ frame-pixel-width frame-pixel-width)) width-fudge width-fudge))))))
    (when (< calc-x 0)
      (setq calc-x 0))
    (set-frame-height (selected-frame) hpl)
    (set-frame-position (selected-frame) calc-x top-y)
    (setq fp (frame-position))
    (setq actual-x (car fp))
    (message "frame-width: %d, display-wid: %d, calc-x: %d, actual-x: %d"
             frame-pixel-width display-pixel-width calc-x actual-x)
    ))



;;
;;  shu-misc-random-ua-string
;;
(defun shu-misc-random-ua-string (length)
  "Return a string composed of random upper case letters of length LENGTH."
  (let ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (shu-misc-random-internal-string letters length)
    ))



;;
;;  shu-misc-random-lad-string
;;
(defun shu-misc-random-lad-string (length)
  "Return a string composed of random lower case letters and digits of length
 LENGTH."
  (let ((letters "abcdefghijklmnopqrstuvwxyz0123456789"))
    (shu-misc-random-internal-string letters length)
    ))



;;
;;  shu-misc-random-internal-string
;;
(defun shu-misc-random-internal-string (letters length)
  "Return a string composed of random LETTERS of length LENGTH."
  (let ((nletters (length letters))
        (rs "")
        (count 0))
    (while (< count length)
      (setq rs (concat rs (char-to-string (elt letters (random nletters)))))
      (setq count (1+ count)))
    rs
    ))


;;
;;  shu-misc-make-unique-string
;;
(defun shu-misc-make-unique-string (string suffix-length ht)
  "Input is a hash table, HT, as well as a STRING.  If the string does not
already exist in HT, add the string to the hash table and return the string.
If the string already exists in HT, add a suffix to the string that is a
random string of length SUFFIX-LENGTH.  If the combination of the original
STRING plus the random string added as a suffix, does not exist in the hash
table, add the new string to the hash table and return it.  This provides the
generation of a set of unique string names."
  (let ((nstring string)
        (value)
        (something t))
    (while something
      (setq value (gethash nstring ht))
      (if value
          (progn
            (setq nstring (concat string (shu-misc-random-lad-string suffix-length)))
            (setq value (gethash nstring ht)))
        (puthash nstring 1 ht)
        (setq something nil)))
    nstring
    ))



;;
;;  shu-extract-name-open-grok
;;
(defun shu-extract-name-open-grok ()
  "The current buffer contains output from an OpenGrok search that has been
copied from the web page and pasted into the buffer.  This function scans the
buffer from the current point and harvests all of the file names that hold the
references for which OpenGrok searched.  It puts the file names (including their
top level directories) into the buffer \"**shu-open-grok**\".
The number of file names found is returned, mostly for the benefit of unit
tests."
  (interactive)
  (let ((gb (get-buffer-create "**shu-open-grok**"))
        (ss "H A D	")
        (son)
        (eon)
        (eol)
        (file-name)
        (file-path)
        (prev-file-path "")
        (full-name)
        (count 0))
    (while (search-forward ss nil t)
      (setq son (point))
      (setq eol (line-end-position))
      (re-search-forward shu-all-whitespace-regexp eol t)
      (setq eon (point))
      (setq file-name (shu-trim (buffer-substring-no-properties son eon)))
      (forward-line -1)
      (setq file-path (shu-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (if (shu-string-starts-ends file-path "/")
          (setq prev-file-path file-path)
        (setq file-path prev-file-path))
      (setq full-name (concat file-path file-name))
      (goto-char eon)
      (princ (concat full-name "\n") gb)
      (setq count (1+ count)))
    (if (= count 0)
        (message "%s" "No file names found")
      (message "%d file names placed in buffer **shu-open-grok**" count))
    count
    ))



;;
;;  shu-add-alexandria-in-batch-mode
;;
(defun shu-add-alexandria-in-batch-mode ()
  "Call the function SHU-ADD-ALEXANDRIA in batch mode.  The function
SHU-ADD-ALEXANDRIA normally ends in edit mode in Doxyfile so that the user can
do a final edit and save.  In batch mode, this function does a save of the
Doxyfile since there is no interactive user.

If the function succeeds, it returns true, else nil.  This allows the top level
batch invoking function to terminate emacs with a zero or non-zero return code
to indicate to an external script whether or not the add command worked."
  (let ((done))
    (setq done (shu-add-alexandria))
    (when done
      (basic-save-buffer)
      (shu-kill-current-buffer))
    done
    ))




;;
;;  shu-add-alexandria
;;
(defun shu-add-alexandria ()
  "Add Alexandria coverage to a git repository.
This function first checks to ensure that a README.md file exists that does not
contain an Alexandria badge and that a Doxyfile does not exist.  If those two
conditions are met, an Alexandria badge is added to the bottom of the README.md
file, a Doxyfile is created, and some of the tags in the Doxyfile are set to
reasonable defaults.  An ALEXANDRIA_DOC_DEPENDENCIES tag is added to the end of
the Doxyfile as a comment.
If this function succeeds, it returns true, else nil.  The return value may
be used by batch mode functions that want to call this function and report
whether or not it succeeded."
  (interactive)
  (let ((readme-name "README.md")
        (doxyfile-name "Doxyfile")
        (have-badge)
        (fbuf)
        (file-buf)
        (badge-added)
        (done))
    (if (not (file-readable-p readme-name))
        (progn
          (ding)
          (message "%s file does not exist." readme-name))
      (if (file-readable-p doxyfile-name)
          (progn
            (ding)
            (message "%s file already exists." doxyfile-name))
        (setq fbuf (get-file-buffer readme-name))
        (if (and fbuf (buffer-modified-p fbuf))
            (progn
              (ding)
              (message "%s file is already open and modified" readme-name))
          (find-file readme-name)
          (goto-char (point-min))
          (setq have-badge (search-forward "[![Alexandria doxygen]" nil t))
          (if have-badge
              (progn
                (ding)
                (message "Alexandria badge already exists in %s file." readme-name)
                (when (not fbuf)
                  (shu-kill-current-buffer)))
            (goto-char (point-max))
            (insert "\n")
            (setq badge-added (shu-add-alexandria-badge))
            (basic-save-buffer)
            (when (not fbuf)
              (shu-kill-current-buffer))
            (if (not badge-added)
                (progn
                  (ding))
              (shu-add-doxyfile)
              (shu-git-add-file doxyfile-name)
              (find-file doxyfile-name)
              (setq done (shu-fixup-doxyfile)))))))
    done
    ))



;;
;;  shu-get-git-name
;;
(defun shu-get-git-name (path)
  "PATH is the url of a git repository from the [remote \"origin\"] section of a
.git/config file.  For example, the entry for this repository is

      https://github.com/codesinger/shu.git

This function extracts two pieces of information from the URL.  One is the name
of the repository, which in this case is \"shu\".  The other is the path to the
repository, which includes the owning group, which in this case is
\"codesinger/shu\".

Those two items are returned in a cons cell with the car of the cons cell
holding the path (with owning group) and the cdr of the cons cell holding the
repository name.

The assumptions made by this function are as follows: The beginning of the
owning group and repository name are preceded by a domain name followed by
either a colon or a slash.  In the case of this repository, the owning group and
repository name are preceded by \"github.com/\".  The repository name may or may
not have a trailing \".git\", which this function removes."
  (let ((tpath (shu-trim-git-end path))
        (sep-char "/")
        (ss1 "\\.[com|net|edu]+[:/]+")
        (pstart)
        (pend)
        (fpath)
        (plist)
        (repository-name))
    (with-temp-buffer
      (insert tpath)
      (setq pstart (point-min))
      (goto-char (point-min))
      (when (re-search-forward ss1 nil t)
        (setq pstart (point)))
      (setq fpath (buffer-substring-no-properties pstart (point-max))))
    (setq plist (split-string fpath sep-char t))
    (setq repository-name (nth (1- (length plist)) plist))
    (cons fpath repository-name)
    ))




;;
;;  shu-trim-git-end
;;
(defun shu-trim-git-end (path)
  "First trim leading and trailing spaces from PATH.  If PATH ends in \".git\",
trim the last four characters from the path.  If PATH does not end in \".git\",
do not trim the last four characters.

Return the PATH, leading and trailing spaces trimmed, with perhaps \".git\"
removed from the end."
  (interactive)
  (let ((git-end ".git")
        (tpath (shu-trim path))
        (ending))
    (setq ending (substring tpath (- (length tpath) (length git-end))))
    (when (string= ending git-end)
      (setq tpath (substring tpath 0 (- (length tpath) (length git-end)))))
    tpath
    ))



;;
;;  shu-copy-repo
;;
(defun shu-copy-repo ()
  "Call SHU-GET-REPO to find the path to the repository and put the result in
the kill ring."
  (interactive)
  (let ((repo (shu-get-repo)))
    (if repo
        (progn
          (message "%s" repo)
          (shu-kill-new repo))
      (message "%s" "*** Not found ***"))
    ))



;;
;;  shu-show-repo
;;
(defun shu-show-repo ()
  "Call SHU-GET-REPO to find the path to the repository and show the result in
the minibuffer."
  (interactive)
  (let ((repo (shu-get-repo)))
    (if repo
        (message "%s" repo)
      (message "%s" "*** Not found ***"))
    ))



;;
;;  shu-get-repo
;;
(defun shu-get-repo ()
  "When positioned anywhere in a git repository, return the git path to the
repository.  This is found in .git/config as the url of [remote \"origin\"].
Return nil if the path cannot be found.

The search is made from the current directory and upwards for the first
directory called \".git\"."
  (let ((config)
        (path)
        (dom-path (locate-dominating-file "." ".git"))
        (git-path))
    (when dom-path
      (setq git-path (concat dom-path ".git/config"))
      (with-temp-buffer
        (setq config (insert-file-contents-literally git-path))
        (if (not config)
            (progn
              (ding)
              (message "%s" "Cannot open .git/config"))
          (setq path (shu-internal-get-repo)))))
    path
    ))



;;
;;  shu-get-url-repo
;;
(defun shu-get-url-repo ()
  "Return the web URL for the current git repository.  If the URL cannot be
found, nil is returned.

The url for the git repository in .git/config is of the form

       git@web-address:repository-name.git

This function removes the trailing \".git\", replaces the leading \"git@\" with
\"https://\" and replaces the \":\" between the web-address and repository-name
with \"/\"."
  (let ((repo (shu-get-repo))
        (prefix)
        (postfix)
        (url-repo)
        (ss ".[com|net|edu]\\(:\\)"))
    (setq prefix (substring repo 0 4))
    (if (not (string= prefix "git@"))
        (progn
          (ding)
          (message "Unknown repository prefix.  Expected 'git@', found '%s'" prefix))
      (setq postfix (substring repo (- (length repo) 4)))
      (if (not (string= postfix ".git"))
          (progn
            (ding)
            (message "Unknown repository postfix.  Expected '.git', found '%s'" postfix))
        (setq url-repo (concat "https://" (substring repo 4 (- (length repo) 4)))))
      (with-temp-buffer
        (insert url-repo)
        (goto-char (point-min))
        (if (re-search-forward ss nil t)
            (progn
              (replace-match "/" t t nil 1)
              (setq url-repo (buffer-substring-no-properties (point-min) (point-max))))
          (ding)
          (message "Cannot find ':' in git url (%s)" url-repo)
          (setq url-repo nil))))
    url-repo
    ))




;;
;;  shu-internal-get-repo
;;
(defun shu-internal-get-repo ()
  "The current buffer holds an instance of the \".git/config\" file for the
repository.  This function returns the git path to the repository, which is the
url given after [remote \"origin\"].  nil is returned if the path cannot be
found."
  (let ((ss1 "remote\\s-*\"origin\"")
        (ss2 "url\\s-*=\\s-*")
        (pstart)
        (pend)
        (path))
    (goto-char (point-min))
    (if (not (re-search-forward ss1 nil t))
        (progn
          (ding)
          (message "%s" "Cannot find [remote \"origin\"] in .git/config"))
      (if (not (re-search-forward ss2 nil t))
          (progn
            (ding)
            (message "%s" "Cannot find \"url = \" following [remote \"origin\"] in .git/config"))
        (setq pstart (point))
        (setq pend (line-end-position))
        (setq path (buffer-substring-no-properties pstart pend))))
    ))




;;
;;  shu-kill-repo
;;
(defun shu-kill-repo ()
  "When positioned in the top level directory of a git repository, place into
the kill ring the git path to the repository.  This is found in .git/config as
the url of [remote \"origin\"]

This should probably be extended to do a search for the .git directory anywhere
above the current position, which would remove the requirement to be in the root
of the repository."
  (interactive)
  (let ((path (shu-get-repo)))
    (when path
      (shu-kill-new path))
    ))



;;
;;  shu-add-alexandria-badge
;;
(defun shu-add-alexandria-badge ()
  "Insert an Alexandria badge for the current project."
  (interactive)
  (let (
        (library-name (shu-get-directory-prefix))
        (repo-path (shu-get-git-repo-path))
        (default-branch (shu-git-find-default-branch))
        (badge-added)
          )
    (if (not shu-internal-dev-url)
        (progn
          (ding)
          (message "%s" "SHU-INTERNAL-DEV-URL custom variable is not set.")
          )
      (when repo-path
        (insert
         (concat
          "[![Alexandria doxygen](https://badges." shu-internal-dev-url "/badge"
          "//Alexandria%20|%20Doxygen/blue?icon=fa-book-open)]"
          "(http://alexandria-doc.stacker." shu-internal-dev-url "/" repo-path
          "/" default-branch "/)"))
        (setq badge-added t)
        )
      )
    badge-added
    ))



;;
;;  shu-get-git-repo-name
;;
(defun shu-get-git-repo-name ()
  "This function tries to get the name of the current git repository
from the .git/config file.  Returns nil if it cannot open .git/config."
  (let (
        (gb (get-buffer-create shu-trace-buffer))
        (git-url)
        (ret-val)
        (repo-name)
        (git-url (shu-get-repo))
        )
    (when git-url
      (princ (concat "git-url: " git-url "\n") gb)
      (setq ret-val (shu-get-git-name git-url))
      (setq repo-name (cdr ret-val))
      )
    repo-name
    ))



;;
;;  shu-get-git-repo-path
;;
(defun shu-get-git-repo-path ()
  "This function tries to get the host local path to the current git repository
from the .git/config file if possible.  If it cannot find the .git/config file,
the it uses the shu custom variable SHU-INTERNAL-GROUP-NAME as the group owner
and uses the name of the current directory as the repository name and constructs
a host local path that is the owning group name, a slash, and the putative
repository name (the name of the current directory."
  (let ((git-url)
        (ret-val)
        (repo-name)
        (repo-path))
    (setq git-url (shu-get-repo))
    (if git-url
        (progn
          (setq ret-val (shu-get-git-name git-url))
          (setq repo-path (car ret-val)))
      (if (not shu-internal-group-name)
          (progn
            (ding)
            (message "%s" "SHU-INTERNAL-GROUP-NAME custom variable is not set."))
        (setq repo-name (shu-get-directory-prefix))
        (setq repo-path (concat shu-internal-group-name "/" repo-name))))
    repo-path
    ))




;;
;;  shu-fixup-doxyfile
;;
(defun shu-fixup-doxyfile ()
  "The current directory is assumed to have the same name as the project for
which the Doxyfile was created.  This function sets various default values in
the Doxyfile.  The current buffer is the Doxyfile."
  (interactive)
  (shu-fixup-project-doxyfile (shu-get-directory-prefix))
  )




;;
;;  shu-fixup-project-doxyfile
;;
(defun shu-fixup-project-doxyfile (project-name)
  "PROJECT-NAME is the name of the project for which the Doxyfile has been created.
This function sets standard default values.
If this function succeeds, it return true, else nil.  The return value may be used
in batch mode to determine if the fixup was successful."
  (interactive "sProject name? ")
  (let ((gb (get-buffer-create shu-trace-buffer))
        (library-name (shu-get-git-repo-name))
        (extract-private "EXTRACT_PRIVATE\\s-*=")
        (extract-static "EXTRACT_STATIC\\s-*=")
        (generate-latex "GENERATE_LATEX\\s-*=")
        (have-dot "HAVE_DOT\\s-*=")
        (project-name-tag "PROJECT_NAME\\s-*=")
        (gen-latex "GENERATE_LATEX\\s-*=")
        (input "INPUT\\s-*=")
        (project-brief "PROJECT_BRIEF\\s-*=")
        (dep-line (shu-get-debian-dependency-line))
        (now (format-time-string "%Y-%m-%dT%T"))
        (fixed))
    (princ (concat "shu-fixup-project-doxyfile: dep-line: '" dep-line "'\n") gb)
    (goto-char (point-min))
    (if (not (re-search-forward extract-private nil t))
        (progn
          (ding)
          (message "%s" "Cannot find EXTRACT_PRIVATE tag."))
      (when (search-forward "NO" (line-end-position)  t)
        (replace-match "YES" t t))
      (goto-char (point-min))
      (if (not (re-search-forward extract-static nil t))
          (progn
            (ding)
            (message "%s" "Cannot find EXTRACT_STATIC tag."))
        (when (search-forward "NO" (line-end-position)  t)
          (replace-match "YES" t t))
        (goto-char (point-min))
        (if (not (re-search-forward generate-latex nil t))
            (progn
              (ding)
              (message "%s" "Cannot find GENERATE_LATEX tag."))
          (when (search-forward "NO" (line-end-position)  t)
            (replace-match "YES" t t))
          (goto-char (point-min))
          (if (not (re-search-forward have-dot nil t))
              (progn
                (ding)
                (message "%s" "Cannot find HAVE_DOT tag."))
            (when (search-forward "NO" (line-end-position)  t)
              (replace-match "YES" t t))
            (goto-char (point-min))
            (if (not (re-search-forward project-name-tag nil t))
                (progn
                  (ding)
                  (message "%s" "Cannot find PROJECT_NAME tag."))
              (when (search-forward "\"My Project\"" (line-end-position)  t)
                (replace-match (concat "\"" project-name "\"")t t))
              (goto-char (point-min))
              (if (not (re-search-forward input nil t))
                  (progn
                    (ding)
                    (message "%s" "Cannot find INPUT tag."))
                (end-of-line)
                (insert (concat " ./" project-name))
                (goto-char (point-min))
                (if (not (re-search-forward gen-latex nil t))
                    (progn
                      (ding)
                      (message "%s" "Cannot find GENERATE_LATEX tag."))
                  (when (search-forward "YES" (line-end-position)  t)
                    (replace-match "NO" t t))
                  (goto-char (point-max))
                  (insert
                   (concat
                    "\n"
                    "# The ALEXANDRIA_DOC_DEPENDENCIES tag is used to list other repositories that\n"
                    "# this repository references.  If you have other referenced repositories, uncomment\n"
                    "# the line below and list the repository names in a line, space separated.  The\n"
                    "# repository name is the name of the owning group followed by a slash followed\n"
                    "# the repository name.\n"
                    "#\n"
                    ))
                  (when dep-line
                    (insert
                     (concat
                      "# As of " now "\n"
                      "# Dependencies of this repository: " dep-line "\n"
                      "#\n")))
                  (insert "# ALEXANDRIA_DOC_DEPENDENCIES = group1/repo1 group2/repo2\n")
                  (goto-char (point-min))
                  (if (not (re-search-forward project-brief nil t))
                      (progn
                        (ding)
                        (message "%s" "Cannot find PROJECT_BRIEF tag."))
                    (setq fixed t)))))))))
    fixed
    ))



;;
;;  shu-get-debian-dependency-line
;;
(defun shu-get-debian-dependency-line ()
  "This function tries to find a Debian library dependency file in the current
directory tree.  If such a file is found, this function returns a single line of
text that holds the space separated names of all of the dependencies."
  (let ((deps (shu-get-debian-dependencies))
        (dep-line))
    (when deps
      (setq dep-line (string-join deps " ")))
    dep-line
    ))



;;
;;  shu-get-debian-dependencies
;;
(defun shu-get-debian-dependencies ()
  "This function tries to find a Debian library dependency file in the current
directory tree.  If such a file is found, this function returns a sorted list of
the dependencies listed in the Debian dependency file.  If no such file exists,
nil is returned."
  (let (
        (gb (get-buffer-create shu-trace-buffer))
        (dep-file (shu-get-debian-dependency-file))
        (dep)
        (deps)
        (line-diff 0)
        (df "***NONE***")
        )
    (when dep-file
      (setq df dep-file)
      )
    (princ (concat "shu-get-debian-dependencies: dep-file: '" df "'\n") gb)
    (when dep-file
      (with-temp-buffer
        (insert-file-contents dep-file)
        (goto-char (point-min))
        (while (and (= line-diff 0)
                    (not (= (point) (point-max))))
          (setq dep (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (push dep deps)
          (princ (concat "    dep: " dep "\n") gb)
          (setq line-diff (forward-line 1))
          )
        )
      (setq deps (sort deps 'string<))
      )
    deps
    ))




;;
;;  shu-get-debian-dependency-file
;;
(defun shu-get-debian-dependency-file ()
  "Use the name of the current directory as the name of a debian library.
Construct a dependency file name which is the name of the current directory with
a file type of \".dep\".  Search through the directory tree for such a file.  If
the file is found return its fully qualified name, i.e., the full path to the
file so that it may be opened.  If no such file exists, return nil."
  (let (
        (gb (get-buffer-create shu-trace-buffer))
        (dep-name (concat (shu-get-directory-prefix) "\\.dep$"))
        (dep-list)
        (dep-file)
        )
    (princ (concat "shu-get-debian-dependency-file: dep-name: '" dep-name "'\n") gb)
    (setq dep-list (directory-files-recursively "." dep-name))
    (princ "dep-list: " gb)(princ dep-list gb)(princ "\n" gb)
    (princ (format "(length dep-list): %d\n" (length dep-list)) gb)
    (when (and dep-list (= (length dep-list) 1))
      (setq dep-file (car dep-list))
      (princ (concat "dep-file: '" dep-file "'\n") gb)
      )
    dep-file
    ))



;;
;;  shu-add-doxyfile
;;
(defun shu-add-doxyfile ()
  "Call \"doxygen -g\" to create a Doxyfile.  Return the output from the doxygen command."
  (let ((result))
    (with-temp-buffer
      (process-file "doxygen" nil nil nil "-g")
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    (shu-trim-trailing result)
    ))



;;
;;  shu-unbrace
;;
(defun shu-unbrace ()
  "When point is on an opening sexp, this function converts, within the scope of
the sexp, all \"{\" to \"(\" and all \"}\" to \").\".
If the number of left braces does not match the number of right braces a warning
message is emitted.

For the benefit of unit tests, the count of left braces converted iff the count
of left braces matches the count of right braces.  If the counts do not match,
nil is returned."
  (interactive)
  (let ((spoint (1+ (point)))
        (epoint)
        (left-count 0)
        (right-count 0)
        (ccount nil))
    (forward-sexp)
    (setq epoint (1- (point)))
    (goto-char spoint)
    (while (search-forward "{" epoint t)
      (setq left-count (1+ left-count))
      (replace-match "(" t t))
    (goto-char spoint)
    (while (search-forward "}" epoint t)
      (setq right-count (1+ right-count))
      (replace-match ")" t t))
    (if (= left-count right-count)
        (progn
          (message "%d braces converted" left-count)
          (setq ccount left-count))
      (ding)
      (message "Found %d { and %d }" left-count right-count))
    ccount
    ))



;;
;;  shu-sitting-on
;;
(defun shu-sitting-on (regex)
  "If the contiguous string of characters at (point) all match REGEX bounded by
either whitespace or the begin / end of the line, return the matched string.  If
any characters are found that do not match REGEX, return nil."
  (let ((here (point))
        (begin)
        (end)
        (result))
    (save-excursion
      (setq begin (shu-sitting-end regex -1))
      (when begin
        (setq end (shu-sitting-end regex 1))
        (when end
          (setq result (buffer-substring-no-properties begin (1+ end))))))
    result
    ))



;;
;;  shu-sitting-end
;;
(defun shu-sitting-end (regex dir)
  "If the text at (point) is a character that matches REGEX, scan until either
whitespace or the beginning / end of the line is reached.  If all characters
scanned match REGEX, return the point of the last matching character, otherwise
return nil.  DIR indicates the direction of the scan.  Negative does a backward
scan.  Non-negative does a forward scan."
  (let ((something t)
        (mc (if (< dir 0) -1 1))
        (end-adjust (if (< dir 0) +1 -1))
        (epos (if (< dir 0)  (line-beginning-position) (1- (line-end-position))))
        (fpos)
        (count 0)
        (case-fold-search nil)
        (c))
    (save-excursion
      (when (looking-at regex)
        (setq count 1)
        (while something
          (if (= (point) epos)
              (progn
                (setq something nil)
                (when (/= count 0)
                  (setq fpos (point))))
            (forward-char mc)
            (setq count (1+ count))
            (when (not (looking-at regex))
              (setq something nil)
              (when (looking-at shu-all-whitespace-regexp)
                (setq fpos (+ (point) end-adjust))))))))
    fpos
    ))



;;
;;  shu-longest-common-substring
;;
(defun shu-longest-common-substring (strings)
  "Return the longest common substring of the list of STRINGS.  Return nil if
there is no common substring."
  (interactive)
  (let ((strs strings)
        (ref (car strings))
        (i)
        (j)
        (s)
        (lcs ""))
    (setq i 0)
    (while (< i (length ref))
      (setq j (1+ i))
      (while (< j (length ref))
        (setq s (substring ref i j))
        (when (shu-is-common-substring s strings)
          (when (> (length s) (length lcs))
            (setq lcs s)))
        (setq j (1+ j)))
      (setq i (1+ i)))
    (when (string= lcs "")
      (setq lcs nil))
    lcs
    ))



;;
;;  shu-longest-common-prefix
;;
(defun shu-longest-common-prefix (strings)
  "Return the longest common prefix of the list of STRINGS.  Return nil if there
is no common prefix."
  (interactive)
  (let ((strs strings)
        (ref (car strings))
        (i)
        (s)
        (lcs ""))
      (setq i 1)
      (while (< i (length ref))
        (setq s (substring ref 0 i))
        (when (shu-is-common-substring s strings)
          (when (> (length s) (length lcs))
            (setq lcs s)))
        (setq i (1+ i)))
    (when (string= lcs "")
      (setq lcs nil))
    lcs
    ))




;;
;;  shu-is-common-substring
;;
(defun shu-is-common-substring (substring strings)
  "If SUBSTRING is a common substring in the list of STRINGS, return SUBSTRING,
else, return nil."
  (let ((strs strings)
        (excluded)
        (string)
        (sstring (regexp-quote substring))
        (common))
    (while (and strs (not excluded))
      (setq string (car strs))
      (when (not (string-match-p sstring string))
        (setq excluded t))
      (setq strs (cdr strs)))
    (when (not excluded)
      (setq common substring))
    common
    ))




;;
;;  shu-is-common-prefix
;;
(defun shu-is-common-prefix (prefix strings)
  "If PREFIX is a common prefix in the list of STRINGS, return PREFIX,
else, return nil."
  (let ((strs strings)
        (excluded)
        (string)
        (prefix-length (length prefix))
        (common))
    (while (and strs (not excluded))
      (setq string (car strs))
      (when (not (string= prefix (substring string 0 prefix-length)))
        (setq excluded t))
      (setq strs (cdr strs)))
    (when (not excluded)
      (setq common prefix))
    common
    ))



;;
;;  shu-prepare-for-rename
;;
(defun shu-prepare-for-rename (old-namespace new-namespace)
  "This is a function that helps to rename a list of files that share one common
part of a name to a list of files that have a different common part of the name.
For example, given the following set of files:

      aaaa_mumble.cpp
      aaaa_mumble.h
      aaaa_mumble.t.cpp

it is not uncommon to want to change those file names to something like

      abcdef_mumble.cpp
      abcdef_mumble.h
      abcdef_mumble.t.cpp

This can be done with the following work flow:

      1. ls \"aaaa*\" >cf.txt

      2. Edit cf.txt to turn it into a script that renames the files from
         \"aaaa*\ to \"abcdef*\".

The editing steps are relatively straightforward, but take a small number of
minutes.

This function automates the editing steps.

When invoked interactively, it first prompts for the old common part and then
for the new common part."
  (interactive "*sOld common part?: \nsNew common part?: ")
  (let ((line)
        (line-length)
        (max-line-length 0)
        (line-diff 0)
        (pad-length 0)
        (pad "")
        (all-lines)
        (spoint))
    (when (> (buffer-size) 0)
      (delete-trailing-whitespace (point-min))
      (goto-char (point-min))
      (while (and (= line-diff 0)
                  (not (= (point) (point-max))))
        (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (setq line-length (length line))
        (when (> line-length max-line-length)
          (setq max-line-length line-length))
        (setq line-diff (forward-line 1)))
      (when (> max-line-length line-length)
        (setq pad-length (- max-line-length line-length)))
      (setq pad-length (+ pad-length 2))
      (setq pad (make-string pad-length ? ))
      (setq all-lines (concat (buffer-substring-no-properties (point-min) (1- (point-max))) pad))
      (setq spoint (point-max))
      (goto-char spoint)
      (insert all-lines)
      (goto-char spoint)
      (while (search-forward old-namespace nil t)
        (replace-match new-namespace t t))
      (kill-rectangle spoint (point-max))
      (goto-char (point-min))
      (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      (setq line-length (length line))
      (setq pad-length 0)
      (when (> max-line-length line-length)
        (setq pad-length (- max-line-length line-length)))
      (setq pad-length (+ pad-length 2))
      (setq pad (make-string pad-length ? ))
      (end-of-line)
      (insert pad)
      (yank-rectangle)
      (goto-char (point-min))
      (delete-trailing-whitespace (point-min))
      (goto-char (point-min))
      (while (and (= line-diff 0)
                  (not (= (point) (point-max))))
        (beginning-of-line)
        (insert "mv ")
        (setq line-diff (forward-line 1)))
      (goto-char (point-min)))
    ))




;;
;;  shu-get-name-and-version
;;
(defun shu-get-name-and-version ()
  "When positioned anywhere on a line that looks like

        Published version 1.2.9 of library

return a string of the form \"library=1.2.9\".  If the current line does not
match the required pattern, return nil."
  (let ((ss "Published version\\s-+\\([.0-9]+\\)\\s-+of\\s-+\\([_-0-9a-zA-Z]+\\)")
        (line)
        (name)
        (version)
        (nv))
    (save-excursion
      (beginning-of-line)
      (if (not (re-search-forward ss (line-end-position) t))
          (progn
            (ding)
            (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
            (message "Library name and version not found in '%s'/'" line))
        (setq version (match-string 1))
        (setq name (match-string 2))
        (setq nv (concat name "=" version))))
    nv
    ))



;;
;;  shu-getnv
;;
(defun shu-getnv ()
  "When positioned anywhere on a line that looks like

        Published version 1.2.9 of library

put into the kill ring  a string of the form \"library=1.2.9\"."
  (interactive)
  (let ((nv (shu-get-name-and-version)))
    (when nv
        (message "%s" nv)
        (shu-kill-new nv))
    ))




;;
;;  shu-srs-create-prompt
;;
(defun shu-srs-create-prompt ()
  "This function creates the prompt for the shu-srs replacement function.
SHU-SRS-LAST-REPLACE is nil, this function prints the default prompt and returns
whatever the user types in.  If SHU-SRS-LAST-REPLACE is non-nil, the prompt
offers to replicate the last change made by SHU-SRS.  If the user types nothing,
the last replacement string is returned.  If the user types something, that is
returned instead."
  (let* ((default-prompt "/x/y?")
         (new-prompt (concat default-prompt ": "))
         (initial-contents)
         (keymap)
         (read)
         (hist)
         (input)
         (retval))
    (barf-if-buffer-read-only)
    (when shu-srs-last-replace
      (setq retval (shu-extract-replacement-strings shu-srs-last-replace))
      (when retval
        (setq new-prompt (concat default-prompt " (default " (car retval) " -> " (cdr retval) "): "))))
    (setq input (read-from-minibuffer new-prompt initial-contents keymap read hist shu-srs-last-replace))
    (when (and (= (length input) 0) shu-srs-last-replace)
      (setq input shu-srs-last-replace))
    input
    ))



;;
;;  shu-srs
;;
(defun shu-srs (rstring)
  "A sed-like version of REPLACE-STRING.  REPLACE-STRING requires two arguments,
which are read interactively, one at a time.  This works well for normal
interactive use.

But sometimes you actually create the search and replacement strings in another
buffer to be fed to REPLACE-STRING.  To use these two strings you have to do the
following:

     1. Invoke REPLACE-STRING
     2. Switch to the other buffer
     3. Copy the search string from the other buffer
     4. Switch back to the main buffer
     5. Paste the search string from the kill ring
     6. Hit enter
     7. Switch to the other buffer
     8. Copy the replacement string from the other buffer
     9. Switch back to the main buffer
    10. Paste the replacement string from the kill ring
    11. Hit enter

This function allows you to enter both strings at one prompt using a sed-like
syntax, such as

     /abc/defg

This specifies a search string of \"abc\ and a replacement string of \"defg\".

The work flow now becomes

     1. Invoke SHU-SRS
     2. Switch to the other buffer
     3. Copy the search and replacement string from the other buffer
     4. Switch back to the main buffer
     5. Paste the search and replacement string from the kill ring
     6. Hit enter

You only have to go through six steps instead of eleven."
  (interactive (list (shu-srs-create-prompt)))
  (let ((rval (shu-extract-replacement-strings rstring))
        (p1)
        (p2)
        (count 0)
        (ess "s"))
    (if (not rval)
        (progn
          (ding)
          (message "Cannot parse '%s' to find the two replacement strings" rstring))
      (setq p1 (car rval))
      (setq p2 (cdr rval))
      (while (search-forward p1 nil t)
        (replace-match p2 nil t)
        (setq count (1+ count)))
      (when (= count 1)
        (setq ess ""))
      (message "shu-srs: Replaced %d occurrence%s" count ess)
      (setq shu-srs-last-replace rstring))
    ))



;;
;;  shu-extract-replacement-strings
;;
(defun shu-extract-replacement-strings (rstring)
  "Parse a sed-like search and replacement string such as \"/abc/defg\".

This function parses such a string.  The first character in the string is the
delimiter.  The delimiter character is used to break the string into two
strings, in this case \"abc\" and \"defg\".  If this can be done successfully,
the two strings are returned in a cons cell.  If the string cannot be parsed,
nil is returned."
  (let ((sc)
        (ss)
        (p1)
        (p2)
        (rval)
        (case-fold-search nil))
    (when (> (length rstring) 3)
      (setq sc (substring rstring 0 1))
      (setq ss (concat sc "\\([^" sc "]+\\)" sc "\\([^" sc "]+\\)" sc "*"))
      (when (string-match ss rstring)
        (setq p1 (match-string 1 rstring))
        (setq p2 (match-string 2 rstring))
        (setq rval (cons p1 p2))))
    rval
    ))



;;
;;  shu-fix-header-line
;;
(defun shu-fix-header-line ()
  "If the first line of the buffer contains the sentinel \"-*-C++-*-\", adjust
the line length to be SHU-CPP-COMMENT-END in length, adding or removing
internal space as necessary.

If the first line of the buffer does not contain the sentinel \"-*-C++-*-\",
do nothing.

Return the number of spaces actually adjusted.  0 means no adjustment made.
A positive number represents the number of spaces added.  A negative number
represents the number of spaces removed."
  (interactive)
  (let ((right-end (1+ shu-cpp-comment-end))
        (sentinel (concat " " shu-cpp-edit-sentinel))
        (count 0)
        (end-pos 0)
        (diff 0))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward sentinel (line-end-position) t)
        (setq end-pos (match-end 0))
        (if (< end-pos right-end)
            (progn
              (setq diff (- right-end end-pos))
              (setq count (shu-expand-header-line diff)))
          (when (> end-pos right-end)
            (setq diff (- end-pos right-end))
            (setq count (- (shu-trim-header-line diff)))))))
    count
    ))



;;
;;  shu-trim-header-line
;;
(defun shu-trim-header-line (trim-count)
  "If the first line of the buffer contains the sentinel \"-*-C++-*-\", remove
TRIM-COUNT number of spaces from in front of the sentinel.

If the first line of the buffer does not contain the sentinel \"-*-C++-*-\",
do nothing.

If there do not exist enough spaces to remove TRIM-COUNT of them, remove
as many as possible.

Return the number of spaces actually removed."
  (let* ((sentinel (concat " " shu-cpp-edit-sentinel))
         (new-sentinel (concat " " sentinel))
         (end-pos)
         (something t)
         (count 0))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward sentinel (line-end-position) t)
        (while something
          (if (= count trim-count)
              (setq something nil)
            (goto-char (point-min))
            (if (not (search-forward new-sentinel (line-end-position) t))
                (setq something nil)
              (replace-match sentinel t t)
              (setq count (1+ count)))))))
    count
    ))




;;
;;  shu-expand-header-line
;;
(defun shu-expand-header-line (expand-count)
  "If the first line of the buffer contains the sentinel \"-*-C++-*-\", add
EXPAND-COUnt spaces in front of it.

If the first line of the buffer does not contain the sentinel \"-*-C++-*-\",
do nothing.

Return the number of spaces actually added."
  (let* ((pad (make-string expand-count ? ))
         (sentinel (concat " " shu-cpp-edit-sentinel))
         (new-sentinel (concat pad sentinel))
         (count 0))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward sentinel (line-end-position) t)
        (replace-match new-sentinel t t)
        (setq count expand-count)))
    count
    ))




;;
;;  shu-make-header-line
;;
(defun shu-make-header-line ()
  "At the top of the current buffer, insert a string that holds the standard
first line comment in a C++ file, which is of the form:

      \"// file_name                                      -*-C++-*-\"

The inserted line is of length SHU-CPP-COMMENT-END.

Does nothing if the curret buffer does not have an associated file name."
  (interactive)
  (let ((file-name  (file-name-nondirectory (buffer-file-name)))
        (header-line))
    (save-excursion
      (if (not file-name)
          (progn
            (ding)
            (message "%s" "Buffer has no name"))
        (setq header-line (shu-make-file-header-line file-name))
        (goto-char (point-min))
        (insert (concat header-line "\n"))
        (message "%s" header-line)))
    ))




;;
;;  shu-misc-set-alias
;;
(defun shu-misc-set-alias ()
  "Set the common alias names for the functions in shu-misc.
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'set-unix-eol 'shu-set-unix-eol)
  (defalias 'set-dos-eol 'shu-set-dos-eol)
  (defalias 'trim-trailing-blanks 'shu-trim-trailing-blanks)
  (defalias 'winpath 'shu-winpath)
  (defalias 'eld 'shu-save-and-load)
  (defalias 'gf 'shu-gf)
  (defalias 'of 'shu-of)
  (defalias 'gd 'shu-gd)
  (defalias 'gfn 'shu-gfn)
  (defalias 'gfl 'shu-gfl)
  (defalias 'gfc 'shu-gfc)
  (defalias 'gquote 'shu-gquote)
  (defalias 'new-latex 'shu-new-latex)
  (defalias 'dup 'shu-dup)
  (defalias 'tighten-lisp 'shu-tighten-lisp)
  (defalias 'loosen-lisp 'shu-loosen-lisp)
  (defalias 'new-lisp 'shu-new-lisp)
  (defalias 'new-ert 'shu-new-ert)
  (defalias 'new-lisp-while 'shu-new-lisp-while)
  (defalias 'reverse-comma-names 'shu-reverse-comma-names)
  (defalias 'comma-names-to-letter 'shu-comma-names-to-letter)
  (defalias 'remove-test-names 'shu-remove-test-names)
  (defalias 'number-commits 'shu-git-number-commits)
  (defalias 'diff-commits 'shu-git-diff-commits)
  (defalias 'get-pr-url 'shu-git-get-pr-url)
  (defalias 'show-branch 'shu-git-show-branch)
  (defalias 'copy-branch 'shu-git-copy-branch)
  (defalias 'insb 'shu-git-insert-branch)
  (defalias 'inso 'shu-git-insert-origin-branch)
  (defalias 'gco 'shu-git-insert-checkout-default)
  (defalias 'gpl 'shu-git-insert-pull-origin-branch)
  (defalias 'gps 'shu-git-insert-push-origin-branch)
  (defalias 'gcm 'shu-git-insert-git-commit)
  (defalias 'case-sensitive 'shu-case-sensitive)
  (defalias 'case-insensitive 'shu-case-insensitive)
  (defalias 'number-lines 'shu-number-lines)
  (defalias 'buffer-number-lines 'shu-buffer-number-lines)
  (defalias 'add-prefix 'shu-add-prefix)
  (defalias 'de-star 'shu-de-star)
  (defalias 'modified-buffers 'shu-modified-buffers)
  (defalias 'all-quit 'shu-all-quit)
  (defalias 'md-toc 'shu-make-md-toc-entry)
  (defalias 'md-name 'shu-make-md-name-entry)
  (defalias 'make-md-toc 'shu-tocify-markdown-file)
  (defalias 'os-name 'shu-show-os-name)
  (defalias 'show-system-name 'shu-show-system-name)
  (defalias 'kill-system-name 'shu-kill-system-name)
  (defalias 'obfuscate-region 'shu-obfuscate-region)
  (defalias 'af 'shu-adapt-frame)
  (defalias 'scan-grok 'shu-extract-name-open-grok)
  (defalias 'add-alexandria 'shu-add-alexandria)
  (defalias 'get-repo 'shu-get-repo)
  (defalias 'add-alexandria-badge 'shu-add-alexandria-badge)
  (defalias 'fixup-doxyfile 'shu-fixup-doxyfile)
  (defalias 'copy-repo 'shu-copy-repo)
  (defalias 'show-repo 'shu-show-repo)
  (defalias 'unbrace 'shu-unbrace)
  (defalias 'prepare-for-rename 'shu-prepare-for-rename)
  (defalias 'getnv 'shu-getnv)
  (defalias 'srs 'shu-srs)
  (defalias 'fix-header 'shu-fix-header-line)
  (defalias 'make-header 'shu-make-header-line)
  )

(provide 'shu-misc)

;;; shu-misc.el ends here
