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

(provide 'shu-misc)


(defconst shu-dired-mode-name "Dired by date"
  "The name of the mode for a dired buffer")


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
    (shu-kill-new name)))

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
    (shu-kill-new name)))


;;
;;  shu-gfn
;;
(defun shu-gfn()
  "While in a file buffer, put the name of the current file into the kill ring."
  (interactive)
  (let ((name  (file-name-nondirectory (buffer-file-name))))
    (if name
        (shu-kill-new name)
      ;;
      (ding))))


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
      ;;
      (setq fline (format "line %d of %s" (shu-current-line) name))
      (shu-kill-new fline))))


;;
;;  shu-gfc
;;
(defun shu-gfc()
  "While in a file buffer, put both the current line number and
column number and the name of the current file into the kill ring
in the form of \"foo.cpp:123:2\"."
  (interactive)
  (let
      ((debug-on-error t)
       (name  (file-name-nondirectory (buffer-file-name)))
       (curpos (1+ (current-column)))
       (fline))
    (if (not name)
        (ding)
      ;;
      (setq fline (format "%s:%d:%d" name (shu-current-line) curpos))
      (shu-kill-new fline))))


;;
;;  shu-gquote
;;
(defun shu-gquote ()
  "Insert a LaTeX quote environment and position the cursor for typing the quote."
  (interactive)
  (let (
        (ip     )
        )
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
;;  shu-tighten-lisp
;;
(defun shu-tighten-lisp ()
  "Within the bounds of a lisp function or macro, \"tighten\" some lisp code.
Look for any single right parenthesis that is on its own line and move it up to
the end of the previous line.  This function is the opposite of SHU-LOOSEN-LISP"
  (interactive)
  (let ((ssfun
         (concat
          "("
          "\\s-*"
          "\\(defun\\|defsubst\\|defmacro\\|ert-deftest\\|defconst\\)"))
        (bof)
        (eof)
        (ss "\\s-+)$")
        (ss2 "\\s-+($")
        (bob)
        (eob)
        (del-count))
    (save-excursion
      (if (not (re-search-backward ssfun nil t))
          (progn
            (ding)
            (message "%s" "Not inside a macro or function"))
        (setq bof (match-beginning 0))
        (setq eof (shu-point-at-sexp bof))
        (goto-char bof)
        (while (re-search-forward ss eof t)
          (setq eob (1- (point)))
          (forward-line -1)
          (end-of-line)
          (setq bob (point))
          (delete-region bob eob)
          (setq eof (shu-point-at-sexp bof)))
        (goto-char bof)
        (while (re-search-forward ss2 eof t)
          (setq bob (point))
          (forward-line 1)
          (when (re-search-forward "^\\s-+(")
            (setq eob (1- (point)))
            (delete-region bob eob)
            (setq del-count (1+ (- eob bob)))
            (setq eof (- eof del-count))))))
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
  (let ((bof)
        (eof)
        (ssfun
         (concat
          "("
          "\\s-*"
          "\\(defun"
          "\\|defsubst"
          "\\|defmacro"
          "\\|ert-deftest"
          "\\|defconst"
          ")"
          "\\)"))
        (ss (concat
             "("
             "\\s-*"
             "\\(if"
             "\\|save-excursion"
             "\\|save-match-data"
             "\\|save-restriction"
             "\\|unless"
             "\\|when"
             "\\|while"
             "\\|with-temp-buffer"
             "\\)"))
        (sslet
         (concat
          "("
          "\\s-*"
          "\\(if"
          "\\|let"
          "\\|let\\*"
          "\\)"))
        (doing t)
        (p)
        (pad)
        (pad-length)
        (start-col)
        (let-begin))
    (save-excursion
      (if (not (re-search-backward ssfun nil t))
          (progn
            (ding)
            (message "%s" "Not inside a macro or function"))
        (setq bof (match-beginning 0))
        (setq eof (shu-point-at-sexp bof))
        ;; Handle all containing functions other than "let"
        (goto-char bof)
        (while doing
          (if (not (re-search-forward ss eof t))
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
        (while (re-search-forward sslet eof t)
          (when (re-search-forward "(\\s-*(" eof t)
            (setq let-begin (match-beginning 0))
            (backward-char 1)
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
              (setq eof (+ eof (length pad))))))))
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
    (shu-trim-trailing result)
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
;;  shu-git-show-branch
;;
(defun shu-git-show-branch ()
  "Display the name of the current branch in a git repository."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (message "%s" branch)
    ))




;;
;;  shu-git-branch-to-kill-ring
;;
(defun shu-git-branch-to-kill-ring ()
  "Put the name of the current branch in a git repository into the kill ring."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (shu-kill-new branch)
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
  "Insert at point the name the git command to push the current branch out
to origin."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (insert (concat "git push origin " branch))
    ))




;;
;;  shu-git-insert-pull-origin-branch
;;
(defun shu-git-insert-pull-origin-branch ()
  "Insert at point the name the git command to pull the current branch out
to origin."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (insert (concat "git pull origin " branch))
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
        (buf-name))
    (while buf-list
      (setq buf (car buf-list))
      (setq mode-name (with-current-buffer buf mode-name))
      (if (string= mode-name shu-dired-mode-name)
          (push buf mod-list)
        (setq buf-fn (buffer-file-name buf))
        (when buf-fn
          (when (not (buffer-modified-p buf))
            (push buf mod-list))))
      (setq buf-list (cdr buf-list)))
    (while mod-list
      (setq buf (car mod-list))
      (setq mode-name (with-current-buffer buf mode-name))
      (setq buf-name (buffer-name buf))
      (princ (concat "Killing " buf-name " (" mode-name ")\n") gb)
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
everything up but not including the end point.  The order of START and END does
not matter."
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
system-name.  As of enacs 25.1, system-name is now a function.  Return nil if
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
system-name.  As of enacs 25.1, system-name is now a function.  Unlike
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
  "Place the sytem name (machine name) in the message area."
  (interactive)
    (message "%s" (shu-system-name-string))
    )



;;
;;  shu-kill-system-name
;;
(defun shu-kill-system-name ()
  "Place the sytem name (machine name) in the message area."
  (interactive)
  (setq debug-on-error t)
    (shu-kill-new (shu-system-name-string))
    )




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
  (defalias 'show-branch 'shu-git-show-branch)
  (defalias 'insb 'shu-git-insert-branch)
  (defalias 'inso 'shu-git-insert-origin-branch)
  (defalias 'gpl 'shu-git-insert-pull-origin-branch)
  (defalias 'gps 'shu-git-insert-push-origin-branch)
  (defalias `gcm `shu-git-insert-git-commit)
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
  (defalias 'os-name 'shu-show-os-name)
  (defalias 'show-system-name 'shu-show-system-name)
  (defalias 'kill-system-name 'shu-kill-system-name)
  )

;;; shu-misc.el ends here
