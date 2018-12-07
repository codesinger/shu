;;; shu-cpp-project.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-misc
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

;;; Commentary:

;;
;; A miscellaneous collection of useful functions

;;; Code

(provide 'shu-misc)

;;
;;  Control the EOL terminator
;;
(defun shu-set-buffer-eol-type (eol-type)
  "Define what the end of line delimeter is in a text file."
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
      (save-restriction (widen)
    (goto-char (point-min))

    (insert (concat
      "\\documentclass[12pt,onecolumn]{article}\n"
      "\\begin{document}\n"
      "\n"))
    (setq p (point))
    (insert (concat
      "\n"
      "\n"
      "\n"
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
  (shu-internal-new-lisp "defun" func-name t)
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
(defun shu-internal-new-lisp (func-type func-name &optional interactive)
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
      "(" func-type " " func-name " ()\n"
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
       (setq ostring (concat
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
In git, the most recent commit may be referenced as HEAD.  The one before the
most recent is HEAD~1.  The one before that is HEAD~2, and so on.  This
notation is simple enough for the last small number of commits.  But if you
are going back farther than that, you either have to copy and past each SHA-1
hash or you have to count the commits by hand, which is tedious and error
prone.
This function numbers all of the commits in a log buffer, which allows you to
do things like git diff between HEAD~33 anbs HEAD~35.  This function counts as
a commit any instance of \"commit\" that starts at the beginning of a line and
is followed by some white space and a forty character hexadecimal number.
Returns the count of the number of commits found."
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
\"99.103\" has start 99, end 103.  \"98\" has starrt 98 and end is nil."
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
  (let (
        (gb (get-buffer-create "**slp**"))
        (ss-1 "\\s-*")
        (ss-2 "\\.\\s-*commit\\s-+\\([0-9a-f]\\{40\\}\\)")
        (sss)
        (commit-hash)
        )
    (save-excursion
      (goto-char (point-min))
      (setq sss (concat ss-1 (number-to-string commit-number) ss-2))
      (princ (format "\nsss: \"%s\"\n" sss) gb)
      (when (re-search-forward sss nil t)
        (setq commit-hash (match-string 1))
        )
      )
    (princ commit-hash gb)
    (princ "\n" gb)
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
  (interactive "sCommits?: ")
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
  (defalias 'new-lisp 'shu-new-lisp)
  (defalias 'new-ert 'shu-new-ert)
  (defalias 'reverse-comma-names 'shu-reverse-comma-names)
  (defalias 'comma-names-to-letter 'shu-comma-names-to-letter)
  (defalias 'remove-test-names 'shu-remove-test-names)
  (defalias 'number-commits 'shu-git-number-commits)
  (defalias 'diff-commits 'shu-git-diff-commits)
)

;;; shu-misc.el ends here
