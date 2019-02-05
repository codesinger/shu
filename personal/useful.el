;;; useful.el --- Shu project code for dealing wth C++ in Emacs
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
;; useful.el
;;
;; A collection of useful functions for random things
;;
;; This is not part of the shu package.  It has some untested functions
;; that I sometimes find useful.  It has global names that do not start
;; with shu-.

(require 'shu-base)

(defconst shu-bib-name (regexp-opt
        (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
              "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
              "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
              "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
              "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
              "_" "$" "-" ":") nil)
"A regular expression to match a name in a .bib file.")


;;
;;  slp-record-visited-file
;;
(defun slp-record-visited-file ()
  "This is a find-file-hook that records in ~/visited-files.log the name
of every visited file.  Makes a useful history of all files visited."
  (let ((name (buffer-file-name))
        (fname (file-name-nondirectory (buffer-file-name)))
        (now (current-time-string))
        (fnow (format-time-string "%Y-%m-%d-%T-(%a)"))
        (line))
    (setq line (concat fnow ": " name "  " fname "\n"))
    (write-region line nil "~/visited-files.log" 'append)
    ))

;;
;;  set-clear.  Mark this as a clear text file that is not
;;              to have emacs backup copies made
;;
(defun set-clear ()
  "Mark this as a clear text file that is not to be backed up"
  (interactive)
  (make-local-variable 'backup-inhibited)
  (setq backup-inhibited t)
)

;;
;;  lcategory  -  Insert BALL_LOG_SET_CATEGORY, etc
;;
(defun lcategory ()
  "Insert BALL_LOG_SET_CATEGORY, etc. at point."
  (interactive)
  (let ((tpoint))
  (insert "BALL_LOG_SET_CATEGORY(__func__);")
  (setq tpoint (point))
  (goto-char tpoint)))

;;
;;  linfo  -  Insert BALL_LOG_INFO, etc
;;
(defun linfo ()
  "Insert BALL_LOG_INFO, etc. at point."
  (interactive)
  (let ((tpoint))
  (insert "BALL_LOG_INFO << ")
  (setq tpoint (point))
  (insert " ;")
  (setq overwrite-mode nil)
  (goto-char tpoint)))

;;
;;  lerror  -  Insert BALL_LOG_ERROR, etc
;;
(defun lerror ()
  "Insert BALL_LOG_INFO, etc. at point."
  (interactive)
  (let ((tpoint))
  (insert "BALL_LOG_ERROR << ")
  (setq tpoint (point))
  (insert " ;")
  (setq overwrite-mode nil)
  (goto-char tpoint)))


;;
;;  lstream
;;
(defun lstream ()
  "Insert a BALL_LOG_INFO_BLOCK at point."
  (interactive)
  (let (
        (pos)
        (pad)
        (pad-count (current-column))
        )
    (setq pad (make-string pad-count ? ))
    (insert
     (concat
      "BALL_LOG_INFO_BLOCK\n"
      pad "{\n"
      pad "    BALL_LOG_OUTPUT_STREAM << "))
    (setq pos (point))
    (insert
     (concat
      ";\n"
      pad "}\n"))
    (goto-char pos)
    ))


;;
;;  cassert
;;
(defun cassert ()
  "Insert an empty assert statement."
  (interactive)
  (let
   ((pad-count (current-column))
    (start ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert "BSLS_ASSERT();\n")
    (goto-char (+ start pad-count 12))))


;;
;;  yfall - Visit the course directory for Fall 2013
;;
(defun yfall ()
  "Visit the course directory for Fall 2013."
  (interactive)
  (dired "~/projects/personal/yale/fall-2013")
)

;;
;;  yspring - Visit the course directory for Spring 2014
;;
(defun yspring ()
  "Visit the course directory for Spring 2014."
  (interactive)
  (dired "~/projects/personal/yale/spring-2014")
)

;;
;;  ysummer - Visit the course directory for Summer 2012
;;
(defun ysummer ()
  "Visit the course directory for Summer 2012."
  (interactive)
  (dired "~/projects/personal/yale/summer-2012")
)

;;
;;  ngf
;;
(defun ngf()
  "While in dired, put the full path to the current file in the kill ring (quoted for shell)"
  (interactive)
  (let (
    (debug-on-error t)
    (name     (dired-get-filename))
       )
    (kill-new (shell-quote-argument name))
  )
)

;;
;;  of
;;
(defun of()
  "While in dired, open the current file (Mac OS X only)"
  (interactive)
  (let (
    (debug-on-error t)
    (name     (dired-get-filename))
       )
    (call-process "open" nil nil nil name)
  )
)


;;
;;  wof
;;
(defun wof()
  "While in dired, open the current file (Windows only)"
  (interactive)
  (let (
    (debug-on-error t)
    (name     (dired-get-filename))
    (cname )
       )
    (with-temp-buffer
        (insert name)
        (goto-char (point-min))
        (while (search-forward "/" nil t)
            (replace-match "\\" nil t))
        (setq cname (buffer-substring-no-properties (point-min) (point-max)))
    )
    (call-process cname nil nil nil)
  )
)

; (1+ (current-column)

;;
;; gorg - Visit the main org file
;;
(defun gorg ()
  (interactive)
  "Visit the main org file (~/data/org/home.org)"
  (find-file "~/data/org/home.org")
)

;;
;; ghist - Visit the two history directories
;;
(defun ghist ()
  (interactive)
  "Visit the two history directories."
  (dired  "~/projects/personal/history/")
  (dired "~/projects/personal/yale/summer-2012/hist-s213/")
)

;;
;; gincl - Visit the equivalent of /usr/include on Mavericks Xcode
;;
(defun gincl ()
  (interactive)
  "Visit the equivalent of /usr/include in MacOS Xcode"
  (dired  "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/")
)


;;
;;  shu-run-all-unit-tests
;;
(defun shu-run-all-unit-tests ()
  "Compile and run all of the unit tests.  This must be run from the test directory."
  (interactive)
  (let
      ((test-files
        (list
         "find.t.el"
         "macros.t.el"
         "s-mode.t.el"
         "shu-base.t.el"
         "shu-cpp-general.t.el"
         "shu-cpp-token.t.el"
         "shu-nvplist.t.el"
         "shu-xref.t.el"))
       (file-name)
       (tlist))
    (setq tlist test-files)
    (while tlist
      (setq file-name (car tlist))
      (byte-recompile-file file-name 0 0 1)
      (setq tlist (cdr tlist)))
    (ert t)
    ))



;;
;;  shu-yank-x-primary
;;
(defun shu-yank-x-primary ()
  "Yank the primary x-get-selection.  Derived from standard emacs
mouse-yank-primary."
  (interactive)
  (let ((get-selection)
        (gui-select
         (and (>= emacs-major-version 25)
              (>= emacs-minor-version 1))))
    (fset 'get-selection
          (if gui-select
              'gui-get-selection
            'x-get-selection))

    ;; Give temporary modes such as isearch a chance to turn off.
    (run-hooks 'mouse-leave-buffer-hook)
    (when select-active-regions
      ;; Without this, confusing things happen upon e.g. inserting into
      ;; the middle of an active region.
      (deactivate-mark))
    (let ((primary (get-selection 'PRIMARY)))
      (if primary
          (insert (get-selection 'PRIMARY))
        (error "%s" "No primary selection")))))


;;
;;
;;
;;
;;  wind - make current window larger or smaller
;;
(defun wind (amount)
  "Make current window larger or smaller by ARG.  Negative ARG
makes it smaller."
  (interactive "*nChange window by: \n") ; Read change count
  (enlarge-window amount)
)

;;
;;  font-size
;;
(defun font-size (scale)
"Interactive function to change the size of the default font.  The size is specified in tenths
of a point.  So 120 is 12 points.  Use prefix argument to set the new size.  If no prefix
argument is used, a prompt is placed in the mini-buffer."
  (interactive "NSize in tenths of points?: ")
  (set-face-attribute 'default nil :height scale)
)

;;
;;  trunc - Set truncate-lines true
;;
(defun trunc ()
  "Set truncate-lines to t so that lines will be truncated."
  (interactive)
  (setq truncate-lines t)
  )

;;
;;  notrunc - Set truncate-lines false
;;
(defun notrunc ()
  "Set truncate-lines to t so that lines will not be truncated."
  (interactive)
  (setq truncate-lines nil)
  )

;;
;;  tabon  -  Set indent-tabs-mode true to turn on tabbing
;;
(defun tabon ()
  "Set indent-tabs-mode true to turn on insert of tab characters."
  (interactive)
  (setq indent-tabs-mode t)
  )

;;
;;  taboff  -  Set indent-tabs-mode false to turn off insert of tabs
;;
(defun taboff ()
  "Set indent-tabs-mode NIL to turn off insert of tab characters."
  (interactive)
  (setq indent-tabs-mode nil)
)
;;
;;  maskon - set s-mode-mask true
;;
(defun maskon ()
  "Set s-mode-mask true - New lines of code have comments."
  (interactive)
  (setq s-mode-mask t)
)

;;
;;  maskoff - set s-mode-mask nil
;;
(defun maskoff ()
  "Set s-mode-mask nil - New lines of code have no comments."
  (interactive)
  (setq s-mode-mask nil)
)


(defun setup-terminal-keys ()
  "Set up the function key and other definitions."
  (interactive)
  (let ((term-type))
    (setq term-type (getenv "TERM"))
    (message "Terminal is %s" term-type)
    (if (string-equal term-type "vt320")
      (setup-base-term)
      (if (string-equal term-type "vt100")
         (setup-base-term)
         (setup-base-term)
         (setup-x-term)
      )
    )
  )
)



(defun setup-base-term ()
;;  (global-set-key [f1]        'buffer-ring)
;;  (global-set-key [f2]        'save-some-buffers)
;;  (global-set-key [f3]        'shu-kill-current-buffer)
;;  (global-set-key [f4]        'beginning-of-buffer)
  (global-set-key [f5]        'shu-put-line-near-top)
  (global-set-key [f6]        'shu-eob)
  (global-set-key [f7]        'shu-kill-current-buffer)
  (global-set-key [f8]        'beginning-of-buffer)
  (global-set-key [f9]        'shu-eob)
  (global-set-key [f10]       'shu-kill-current-buffer)
;;  (global-set-key [f11]       'end-kbd-macro)
;;  (global-set-key [f12]       'call-last-kbd-macro)

  (global-set-key [insert]     'overwrite-mode)
  (global-set-key [home]       'beginning-of-line)
  (global-set-key [end]        'end-of-line)
  (global-set-key [pageup]     'scroll-down)
  (global-set-key [pagedown]   'scroll-up)
  (global-set-key [up]         'previous-line)
  (global-set-key [down]       'next-line)
  (global-set-key [left]       'backward-char)
  (global-set-key [right]      'forward-char)
)

(defun setup-x-term ()
  (global-set-key [backspace] 'backward-delete-char)
  (global-set-key [delete] 'delete-char)

;  (global-set-key [r1] 'compile)
;  (global-set-key [r2] 'next-error)
;  (global-set-key [r3] 'undo)
;  (global-set-key [r4] 'start-kbd-macro)
;  (global-set-key [r5] 'end-kbd-macro)
;  (global-set-key [r6] 'call-last-kbd-macro)
;  (global-set-key [r11] 'recenter)
;  (global-set-key [l3] 'apropos)
;  (global-set-key [l4] 'undo)
;  (global-set-key [l5] 'delete-other-windows)
;  (global-set-key [l6] 'copy-region-as-kill)
;  (global-set-key [l7] 'split-window-vertically)
;  (global-set-key [l8] 'yank)
;  (global-set-key [l9] 're-search-forward)
;  (global-set-key [l10] 'kill-line)

  (global-set-key [help] 'help-command)
)



;;
;;  -  s-mode-hook
;;
;;(setq s-mode-hook
;;  (function
;;    (lambda ()
;;      (fset 'sc 'shift-code)            ; Make sc a synonym for shift-code
;;      (fset 'scn 'shift-code-no-comment)
;;      (fset 'sli 'shift-line)           ; Make sli a synonym for shift-line
;;      (fset 'slin 'shift-line-no-comment)
;;      (fset 'trim 's-mode-trim-on)
;;      (fset 'notrim 's-mode-trim-off)
;;      (if s-mode-global-cleanup         ; If global cleanup is true
;;        (setq s-mode-cleanup t))        ; Trim trailing white space when
;;                                        ;  file is saved
;;    )
;;  )
;;)

;;
;;  -  dired-load-hook
;;
(setq dired-load-hook
  (function
    (lambda ()
      (setq dired-copy-preserve-time t) ; Preserve original timestamp on copied
                                        ;  files
    )
  )
)

;;
;;  -  gdb-mode-hook
;;
(setq gdb-mode-hook
      (function
       (lambda ()

         (def-gdb "break"  [F2] "Set break point at current line")
         (def-gdb "step"   [F4] "Step one source line with display")
         (def-gdb "next"   [F5] "Step one source line (skip functions)")
         (def-gdb "cont"   [F6] "Continue with display")

         (def-gdb "up"     [F7] "Go up N stack frames (numeric arg) with display")
         (def-gdb "down"   [F8] "Go down N stack frames (numeric arg) with display")

         )
       )
      )

(setq mail-mode-hook
  (function
    (lambda ()
      (setq mail-archive-file-name "~/Mail/book")
      (local-unset-key "\C-c\C-c")      ; mail-send-and-exit
      (local-unset-key "\C-c\C-s")      ; mail-send
    )
  )
)

(defun send ()
  "Invoke mail-send-and-exit when in mail-mode."
  (interactive)
  (if (equal major-mode 'mail-mode)
    (mail-send-and-exit nil)
    (message "major-mode is not mail-mode - cannot send.")
  )
)

(defun color(ncolor)
  "Invoke x-set-background-color to set the background color."
  (interactive "*sColor? ")
  (set-background-color ncolor)
)


(defun nt-colors ()
  "Set up colors for emacs on NT"
  (interactive)
   (set-background-color "blue")
   (set-foreground-color "white")
   (set-foreground-color "lemon chiffon")
   (set-cursor-color "light grey"))

;;
;;  fparam - Put the frame parameters in a buffer
;;
(defun fparam ()
  "Put the frame parameters in a buffer called *frame parameters*"
  (interactive)
  (let (
    (gbuf      (get-buffer-create "*frame parameters*"))
    (fp  (frame-parameters))
    (debug-on-error  t)

       )
    (princ fp  gbuf)
    (princ "\n" gbuf)
    (while fp
      (princ (car fp) gbuf)
      (princ "\n" gbuf)
      (setq fp (cdr fp)))
    (switch-to-buffer gbuf)
))


(defun ccol()
  "Show column in which point is sitting."
  (interactive)
  (message "point is in column %s." (1+ (current-column)))
)


;;
;;  cite-bib - Turn a BibTex file into a list of @nocite
;;             for all entries in the file.
;;
;     202:@book{kingsley79-2,
(defun cite-bib()
  "Turn a BibTex file into a list of @nocite, one per BibTex entry."
  (interactive)
  (internal-cite-bib "n")
)


;;
;;  cite-book - Turn a BibTex file into a list of @nocite
;;              but only for all @book entries in the file.
;;              Entries for @article, @inbook, etc are ignored.
;;              As of 13 Nov 2013, this is used to sort a set of
;;              physical books in a library at 2 Academy Street.
;;
;     202:@book{kingsley79-2,
(defun cite-book()
  "Turn a BibTex file into a list of @nocite, one per BibTex entry,
but only for type @book."
  (interactive)
  (internal-cite-bib "b")
)

;;
;;  cite-bib - Turn a BibTex file into a list of @nocite
;;
;     202:@book{kingsley79-2,

(defun internal-cite-bib(type)
  "Turn a BibTex file into a list of @nocite, one per BibTex entry."
  (interactive)
  ( let* (
    (type-char (regexp-opt
        (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
              "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
              "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
              "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z") nil))
    (ss (concat "\\([ ]*[0-9]*:@" type-char "*" "{" "\\)"
                "\\(" shu-bib-name "+" "\\)" ","))
    (octype "@")

; This is what each entry in the occur buffer looks like.
;     202:@book{kingsley79-2,

  (cite  )
  (cite-buffer "*cite*")
        )

  (when (string-equal type "b")
    (setq ss (concat "\\([ ]*[0-9]*:@book{" "\\)"
                "\\(" shu-bib-name "+" "\\)" ","))
    (setq octype "@book")
  )
  ;; Find the name of each entry using "occur"
  (occur octype 0)
  (other-window 1)
  (delete-other-windows)
  (clone-buffer cite-buffer)
  (switch-to-buffer cite-buffer)
  (setq buffer-read-only nil)
  (goto-char (point-min))
  (while (re-search-forward ss (buffer-size) t)
    (setq cite (concat "\\\\nocite{" (match-string 2) "}"))
    (replace-match cite)
  )
  (goto-char (point-min))

))
;;
;; fixup-senior-notes
;;
(defun fixup-senior-notes ()
  "Transform \section{\citetitle{blah}} into \section{\citetitle{blah} (blah)}.  This puts the
   actual BibTex tag names into the headings."
  (interactive)
  (let (
    (gbuf      (get-buffer-create "*slp debug*")) ;
    (new-head  )
    (ss (concat "\\\\section{\\\\citetitle{"
                "\\(" shu-bib-name "+" "\\)" "}}"))
       )

  (princ (concat ss "\n\n") gbuf)
  (setq debug-on-error t)

  (save-excursion (save-restriction (widen)
    (goto-char (point-min))

    (while (re-search-forward ss (buffer-size) t)
;      (setq new-head (concat "\\section{" (match-string 1) " -- \\citetitle{" (match-string 1) "}}"))
;      (setq new-head (concat "\\section{\\citetitle{" (match-string 1) "} (" (match-string 1) ")}"))
      (setq new-head (concat "\\slpnotesection{" (match-string 1) "}"))
      (beginning-of-line)
      (kill-line)
      (insert new-head)
      (princ (concat new-head "\n") gbuf)
    )
  ))
))
;;
;;  Get the names of the directories that hold a set of Java files
;;
(defun java-directory-names ()
  "In a list Java file names produced by find, strip out
the names of all of the Java files, leaving only the directory
names.  This list of directory names can then be run through
uniq to get a list of all of the directories in which Java
files reside."
  (interactive)
  (let
   ((ss "\\.\\([a-zA-Z@$.-_/]*\\)/\\([a-zA-Z@$.-_]*\\)\\.java")
    (last-name "")
    (first-name "")
    (ostring   nil)
    (path-name nil)
    (class-name nil)
    (ccount 0))
   (while (re-search-forward ss (buffer-size) t)
     (progn
       (setq path-name (match-string 1))
       (setq class-name (match-string 2))
       (setq ostring (concat "." path-name))
       (replace-match ostring t t nil)
       (setq ccount (1+ ccount))
     )
   )
   (message (concat "Converted " ccount " occurrences."))
  )
)

;;
;;  Get the names of a set of Java files with the directory
;;  names stripped out.
;;
(defun java-file-names ()
  "In a list Java file names produced by find, strip out
the names of all of the directories leaving only the names
of the individial Java files."
  (interactive)
  (let
   ((ss "\\.\\([a-zA-Z@$.-_/]*\\)/\\([a-zA-Z@$.-_]*\\)\\.java")
    (last-name "")
    (first-name "")
    (ostring   nil)
    (path-name nil)
    (class-name nil)
    (ccount 0))
   (while (re-search-forward ss (buffer-size) t)
     (progn
       (setq path-name (match-string 1))
       (setq class-name (match-string 2))
       (setq ostring (concat class-name ".java"))
       (replace-match ostring t t nil)
       (setq ccount (1+ ccount))
     )
   )
   (message (concat "Converted " ccount " occurrences."))
  )
)
(defun ditdit()
 (interactive)
 (shu-local-replace "6380" "6400")
 (shu-local-replace "6370" "6390")
 (shu-local-replace "6350" "6370")
 (shu-local-replace "6360" "6380")
 (shu-local-replace "6330" "6350")
 (shu-local-replace "6320" "6340")
 (shu-local-replace "6345" "6360")
 (shu-local-replace "6310" "6330")
 (shu-local-replace "6300" "6320")
 (shu-local-replace "6290" "6310")
)


;;
;;   shu-show-current-line
;;
(defun shu-show-current-line(log-buffer &optional prefix)
  (let* (
    (bol (save-excursion (beginning-of-line) (point)))
    (eol (save-excursion (end-of-line) (point)))
    (the-line (buffer-substring bol eol))
    (pline (concat the-line "\n"))
        )
  (when prefix
    (setq pline (concat prefix pline)))
  (princ pline log-buffer)
))

;;
;;   shu-get-original-line
;;
;;   Both the original line and the modified line are stored
;;   in a cons cell.
;;
;;   Then we build a cons cell whose CAR is the line number and whose
;;   CDR is the cons cell contaning both strings that represent the
;;   old and new lines.
;;
;;   -------------------          -------------------
;;   |        |        |          |        |        |
;;   | Line # |   o-------------->|    o   |   o    |
;;   |        |        |          |    |   |   |    |
;;   -------------------          -----|-------|-----
;;                                     |       |
;;                                     |       +-----> Modified version of the line
;;                                     |
;;                                     +-------------> Original version of the line
;;
;;
;;   Then we cons this onto the front of line-list and return line-list
;;
(defun shu-get-original-line(line-list &optional log-buffer)
  (let* (
    (bol (save-excursion (beginning-of-line) (point)))
    (eol (save-excursion (end-of-line) (point)))
    (the-line (buffer-substring bol eol))
    (line-no (save-excursion (save-restriction (widen) (1+ (count-lines (point-min) bol)))))
    (both-lines (cons (shu-get-current-line) "x!x"))
    (line-info (cons line-no both-lines))
        )
  (setq line-list (cons line-info line-list))
  line-list
))

;;
;;   shu-get-modified-line
;;
;;   The CAR of line-list is the line-info, whose CAR is the line
;;   number and whose CDR is the pair of original and modified strings.
;;
;;   Here we make the contents of the current line the CDR of the
;;   cons cell that holds both the original and modified strings
;;
(defun shu-get-modified-line(line-list &optional log-buffer)
  (let* (
    (bol (save-excursion (beginning-of-line) (point)))
    (eol (save-excursion (end-of-line) (point)))
    (the-line (buffer-substring bol eol))
    (line-info (car line-list))
    (both-lines (cdr line-info))
        )
  (setcdr both-lines (shu-get-current-line))
  line-list
))

;;
;;   shu-show-lines
;;
;;   Sort the list of changes by line number and show them all
;;
(defun shu-show-lines(line-list log-buffer)
  (let (
    (pad  nil)
    (show-list (sort line-list (lambda(t1 t2) (< (car t1) (car t2)))))
    (line-info )
    (both-lines )
    (line-no )
    (original-line )
    (modified-line )
       )
  (while show-list
    (setq line-info (car show-list))
    (setq both-lines (cdr line-info))
    (setq line-no (car line-info))
    (setq original-line (car both-lines))
    (setq modified-line (cdr both-lines))
    (if (< line-no 10)
      (setq pad "   ")
      (if (< line-no 100)
        (setq pad "  ")
        (if (< line-no 1000)
          (setq pad " ")
          (setq pad ""))))
    (princ (format "\n%s%d: %s\n" pad line-no original-line) log-buffer)
    (princ (format "%s%d: %s\n" pad line-no modified-line) log-buffer)
  (setq show-list (cdr show-list)))
))
