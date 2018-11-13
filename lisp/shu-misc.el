;;; shu-cpp-project.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
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
;; shu-misc.el
;;
;; A miscellaneous collection of useful functions

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
  (let (
    (debug-on-error t)
    (p               )
       )
  (save-excursion (save-restriction (widen)
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
      "\\end{document}\n"))
   ))
  (goto-char p)
  )
)

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
  (defalias 'reverse-comma-names 'shu-reverse-comma-names)
  (defalias 'comma-names-to-letter 'shu-comma-names-to-letter)
  (defalias 'remove-test-names 'shu-remove-test-names)
)
