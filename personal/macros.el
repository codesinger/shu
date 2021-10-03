;;; macros.el --- Shu project code for dealing wth C++ in Emacs
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
;; macros.el
;;
;; A collection of useful functions for random things
;;
;; This is not part of the shu package.  It has some untested functions
;; that I sometimes find useful.  It has global names that do not start
;; with shu-.

(defconst shu-bib-name (regexp-opt
        (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
              "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
              "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
              "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
              "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
              "_" "$" "-" ":") nil)
  "A regular expression to match a name in a .bib file.")


;;
;;  console-to-py
;;
(defun console-to-py ()
  "Function to take a snippet of JavaScript console trace and turn it into a statement that
creates the equivalent as nested python dictionaries, with perhaps arrays of dictionaries as well."
  (interactive)
  (let* (
    (debug-on-error t)
    (dname-rx  "\\([a-z0-9]+\\):")
;; [bcem_Aggregate 0xbe75e03c "AddOrder"]
;; [bcem_Aggregate 0xbe778b8c "HideDisableOut"[19]]
;; [bcem_Aggregate 0xbe74c57c STRING_ARRAY[0]]
;; This matches the above patterns
    (bcem "\\[bcem_Aggregate\\s-+0[xX][0-9a-fA-F]+\\s-+[\\\"a-z0-9A-Z_]+\\s-*\\(\\[[0-9]+\\]\\)*\\]")
;;  10: [bcem_Aggregate 0xbe20d25c "HideDisableOut"]
;; This matches the above pattern
    (bcem-elt (concat "\\s-*[0-9]+:\\s-+" bcem))
;; [object Object]
    (obj-obj "\\s-*\\[object Object\\]")
    (false "\\:\\s-+false,")
    (null  "\\:\\s-+null,")
    (vname )
    (vnc 0)
    (nullc 0)
    (tally )
       )
  (save-excursion
  ;; Remove all strings of the form 10: [bcem_Aggregate 0xbe20d25c "HideDisableOut"]
  (goto-char (point-min))
  (while (re-search-forward bcem-elt nil t)
    (replace-match "" t))
  ;; Remove all strings of the form [bcem_Aggregate 0xbe20d25c "HideDisableOut"]
 (goto-char (point-min))
  (while (re-search-forward bcem nil t)
    (replace-match "" t))
  ;; Remove all strings of the form [object Object]
  (goto-char (point-min))
  (while (re-search-forward obj-obj nil t)
    (replace-match "" t))
  ;; Put single quotes around all the variable names
  (goto-char (point-min))
  (while (re-search-forward dname-rx nil t)
    (setq vnc (1+ vnc))
    (setq vname (match-string 1))
    (replace-match (concat "'" vname "':") t ))
  ;; Turn all values of "false" into "False"
  (goto-char (point-min))
  (while (re-search-forward false nil t)
    (replace-match ": False," t))
  ;; Turn all values of "nil" into "None"
  (goto-char (point-min))
  (while (re-search-forward null nil t)
    (replace-match ": None," t))
  ;; Finally, move the whole thing right by four places
  (goto-char (point-min))
  (shift-code-no-comment 4 (point-min) (point-max))
  (setq tally (format "Converted %d variables." vnc))
  (message "%s" tally)
)))



;;
;;  gw  -  Switch to the emacs lisp subdirectory and load dired
;;
(defun gw ()
  "Visit the Emacs lisp subdirectory and load dired."
  (interactive)
  (let (
    (x )
       )
  (setq x (concat "/Users/slp/scratch/emacs-24.3-source" "/lisp"))
  (dired x)
))

;;
;; wedding - Put the URL for the wedding photos in the kill ring
;;
(defun wedding ()
  (interactive)
  (kill-new "http://www.flickr.com/photos/80722182@N02/sets/72157630425389828/")
)

;;
;; ydnmag - Put the URL for the YDN Magazine article in the kill ring
;;
(defun ydnmag ()
  (interactive)
  (kill-new "http://yaledailynews.com/magazine/2013/02/17/the-second-time-saybrugian/")
)

;;
;; ydn2mag - Put the URL for the YDN Magazine article in the kill ring
;;
(defun ydn2mag ()
  (interactive)
  (kill-new "http://news.yale.edu/2014/05/19/first-person-student-completes-his-ba-48-years-later")
)

;;
;; yammag - Put the URL for the YDN Magazine article in the kill ring
;;
(defun yammag ()
  (interactive)
  (kill-new "http://yalealumnimagazine.com/blog_posts/1795")
)

;;
;; secrets - Put the URL for the Spies, secrets, and science article in the kill ring
;;
(defun secrets ()
  (interactive)
  (kill-new "http://news.yale.edu/2013/12/12/creative-classroom-books-secrets-historian-s-students-find-unexpected-past")
)

;;
;; calliope - Put the URL for Calliopean collection in the kill ring
;;
(defun calliope ()
  (interactive)
  (kill-new "http://hdl.handle.net/10079/fa/mssa.ru.0857")
)

;;
;; linonia - Put the URL for Linonian collection in the kill ring
;;
(defun linonia ()
  (interactive)
  (kill-new "http://hdl.handle.net/10079/fa/mssa.ru.0206")
  )


;;
;;  recruiter
;;
(defun recruiter ()
  "A skeleton response for a recruiter."
  (interactive)
  (let (
        (msg
         (concat
          "Hi ,\n"
          "\n"
          "I am in a job that is a very good fit for my skills and intetests and I "
          "have no interest in exploring another position at this time.\n"
          "\n"
          "Best,\n"
          "\n"
          "Stewart"
          ))
        )
    (with-temp-buffer
      (insert msg)
      (copy-region-as-kill (point-min) (point-max))
      )

    ))


;;
;; Visit some common files and put Eudora pw in the kill ring
;;
(defun startup ()
 (interactive)
 (let (
  (pwbeg  nil)
  (pwend  nil)
      )
;  (find-file "~/projects/stuff/plist.ctx")
;  (set-fill-column 888)
;  (search-forward "Verizon Online")
;  (search-forward "/")
;  (setq pwbeg (point))
;  (end-of-line)
;  (setq pwend (point))
;  (copy-region-as-kill pwbeg pwend)
;  (goto-char (point-min))
  (find-file "~/projects/stuff/may.tex")
  (set-fill-column 84)
  (goto-char (point-max))
;  (find-file "~/projects/stuff/diary3.ctx")
;  (set-fill-column 80)
;  (goto-char (point-max))
))

;;
;;
;;
(defun java-doc()
  "Put the path to the JDK 1.5 documentation in the kill ring as a
Windows path so that it can be pasted into the browser."
  (interactive)
  (let (
       )
  (with-temp-buffer
    (insert "c:\\apps\\JDKs\\docs\\SUN-5.0\\docs\\index.html")
    (copy-region-as-kill (point-min) (point-max)))
))

;;
;;
;;
(defun protocol()
  "Switch to the protocol source directory and put path in kill ring."
  (interactive)
  (let (
    (protocol-dir   "c:/u/spalmer/projects/Distillery/protocol")
    (protocol-dir-w "c:\\u\\spalmer\\projects\\Distillery\\protocol")
;  (debug-on-error t)
       )
  (with-temp-buffer
    (insert protocol-dir-w)
    (copy-region-as-kill (point-min) (point-max)))
  (with-temp-buffer
    (insert-file-contents (concat protocol-dir "/pkg.txt") t)
    (set-project (point-min) (point-max))
  )
  (dired protocol-dir)
))

;;
;; buffer-ring
;;
(defun buffer-ring ()
  "Works around a ring of buffers.
   Steps to the next buffer in list, putting the current buffer
   at the end of the selection list."
  (interactive)
  (bury-buffer (current-buffer))
  (switch-to-buffer (other-buffer)))


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


;;
;;  set-package
;;
(defun obsolete-not-here-set-package (start end)
"Mark a region in a file that contains one subdirecdtory name per line.
Then invoke set-package and it will find and remember all of the java
files in those subdirectories.  You may then subsequently visit any of
those files by invoking M-x vj which will allow you to type in the file
name only (with auto completion) and will then visit the file in the
appropriate subdirectory."
  (interactive "r")                 ; bounds of marked region (start end)
  (save-excursion
  (let ((sline (the-line-at start)) ; Remember start line
        (eline (the-line-at end))   ; Remember end line
        (line-diff 0)               ; The difference between the number of lines we
                                    ;  tried to move and the number we actually moved
        (key-list nil)              ; Used to construct the alist
        (tlist nil)                 ; A copy of key-list we can pull apart
        (slist nil)                 ; A sorted list of alist key values
        (sublist nil)               ; A single entry in the alist
        (subitem nil)               ; A single key value in the alist
     (m (copy-marker 300 ) ) )
    (goto-char start)               ; Move to region start
                                         ; While we have not reached last line and
    (while (and (<= (shu-current-line) eline) (= line-diff 0)) ; there are more lines
      (setq key-list (append key-list (add-package-line)))
      (setq line-diff (forward-line 1))
    )
    ;
    ; shu-java-paclage list is the completed alist
    ;
    (setq shu-java-package-list key-list)
    ;
    ; Now create shu-java-list which is a sorted list of the
    ; keys of the alist and a handy way to see which files
    ; were picked up.
    ;
    (setq tlist (copy-alist shu-java-package-list))
    (while tlist
      (setq sublist (car tlist))
      (setq subitem (car sublist))
      (setq slist (cons subitem slist))
      (setq tlist (cdr tlist))
    )
    (setq shu-java-list (sort slist 'string<))
    ;
    ; Now create shu-java-dlist which is a sorted list of the
    ; values of the alist and a handy way to see which files
    ; were picked up.
    ;
    (setq slist nil)
    (setq tlist (copy-alist shu-java-package-list))
    (while tlist
      (setq sublist (car tlist))
      (setq subitem (car (cdr sublist)))
      (setq slist (cons subitem slist))
      (setq tlist (cdr tlist))
    )
    (setq shu-java-dlist (sort slist 'string<))
  )
  )
)
;;
;;  add-package-line
;;
(defun obsolete-not-here-add-package-line()
  "Do something"
  (let (
     (key-list nil)
     (eol (save-excursion (end-of-line) (point)))
     (dir-name "")
     (m (copy-marker 300 ) ) )
     (setq dir-name (buffer-substring (point) eol))
     (setq key-list (shu-subdir-for-package dir-name))
  key-list
  )
)

;;
;;  vj - Visit a java file in a package
;;
;;    Note: As of November, 2000 there seems to be a bug in
;;          completing-read.  Even when require-match is t
;;          <return> does not complete-and-exit, it exits
;;          with the text in the minibuffer, which may or
;;          may not match an entry in the table.
;;          This is the reason for the while loop that
;;          keeps reading until there is a match.
;;
(defun obsolete-not-here-vj ()
"Visit a java file in a package."
  (interactive)
  (let (
     (key-list nil)
     (file-to-seek nil)
     (default-file-to-seek (find-default-file))
     (real-file-to-seek nil)
     (tfile nil)
     (invitation "Visit java file: ") )
     (if (not (boundp 'shu-java-package-list))
       (progn
       (message "No package files have been defined.")
       (ding))
     ;
       (while (not tfile)
         (setq file-to-seek
               (completing-read
                  (if default-file-to-seek
                    (format "%s(default %s) " invitation default-file-to-seek)
                      invitation)
                  shu-java-package-list
                  nil
                  nil
                  nil
                  nil
                  default-file-to-seek))
         (if (equal file-to-seek "")
           (or default-file-to-seek (error "There is no default Java file")))

         (setq tfile (assoc file-to-seek shu-java-package-list))
         (if (not tfile)
           (ding))
       )

       (setq real-file-to-seek (car (cdr tfile)))

       (find-file real-file-to-seek))
  )
)
;
;  find-default-file - If point is sitting on something that looks like
;    a Java symbol then return it as a default candidate for the file name
;    we wish to visit.
;
(defun obsolete-not-here-find-default-file ()
  (save-excursion
    (if (looking-at "\\sw\\|\\s_")
      (progn
      (while (looking-at "\\sw\\|\\s_")
        (forward-char 1))
      (if (or (re-search-backward "\\sw\\|\\s_"
                                  (save-excursion (beginning-of-line) (point))
                                  t)
              (re-search-forward "\\(\\sw\\|\\s_\\)+"
                                 (save-excursion (end-of-line) (point))
                                 t))
          (progn (goto-char (match-end 0))
                 (buffer-substring (point)
                                   (progn (forward-sexp -1)
                                          (while (looking-at "\\s'")
                                            (forward-char 1))
                                          (point))))))
      nil)))

;;
;;  shu-subdir-for-package
;;
(defun obsolete-not-here-shu-subdir-for-package (directory-name)
"Given a subdirectory name return an alist that contains as keys the
names of all of the java files in the subdirectory, and as values the
fully qualified name and path of the java file.  So if the directory
\"/u/foo/bar\" contains thing.java and what.java the returned
alist would be

      ((\"thing\" \"/u/foo/bar/thing.java\")
       (\"what\" \"/u/foo/bar/what.java\"))

This allows us to associate the key \"thing\" with the fully qualified
name \"/u/foo/bar/thing.java\"."
  (let (
     (file-name "")
     (full-name "")
     (item nil)
     (key-list nil)
     (directory-list (directory-files directory-name t "[^.]\.java$"))
     (m (copy-marker 400 ) ) )
     (while directory-list
       (setq full-name (car directory-list))
       (setq file-name (file-name-sans-extension (file-name-nondirectory full-name)))
       (setq item (list file-name full-name))
       (setq key-list (cons item key-list))
       (setq directory-list (cdr directory-list))
     )
  key-list
  )
  )


;;; macros.el ends here
