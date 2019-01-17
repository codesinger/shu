;;; shu-java-base.el --- Shu project code for dealing wth Java in Emacs
;;
;; Copyright (C) 2013 Stewart L. Palmer
;;
;; Package: shu-java-base
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

;; A collection of useful functions for dealing with Java code.

;;; Code:




;;NOTES FOR FUTURE STUFF
;;
;;   1. Use (regexp-opt) to make the value for shu-java-comment-starter-ss
;;
;;
;;  This file contains the class name, package name, and file name
;;  for the entire JDK.  It is generated from the jar file containing
;;  the jdk source by the function shu-parse-base-jar below.
;;
(defvar shu-java-class-list nil
"This is an alist whose keys are unqualified Java class names and whose
values contain information about the class such as the package name of
the class and the file in which the source code resides.  Used by the slp
Java package to locate classes and Java files.")
;
;  Note on data structures.
;
;  Items retrieved from shu-java-class-list via (assoc) are referred to
;  below as 'assoc-item'.
;
;  The car of an assoc-item is the unqualified class name.
;  the cdr of an assoc-item is a list of properties, called 'props-list'.
;
;  Each item in 'props-list' is itself a list called 'props'.
;
;  The members of 'props' are ...
;
;  1. The package name for this instance of the unqualified class name
;  2. The file name that holds the source code
;  3. The file member name if the file name is an archive file (jar, zip,
;     or tar)
;

(defvar shu-java-class-name-list nil
"This is an alist whose keys are fully qualified Java class names and whose
values point to the properties of this instance of the class.  If there exists
more than one instance of an unqualified Java class then the user is invited
to choose the appropriate fully qualified name from the completion buffer.
This alist is then used to find the file associated with the class.")

(defvar shu-java-file-name-list nil
"This is an alist whose keys are fully qualified file names and whose
values point to the properties of this instance of the class.  If there exists
more than one instance of an unqualified Java class then the user is invited
to choose the appropriate fully qualified name from the completion buffer.
This alist is then used to find the file associated with the class.")
;
;  Note on data structures.
;
;  Items retrieved from shu-java-class-name-list via (assoc) are referred to
;  below as 'props-item' (aka 'props').  See discussion above under
;  shu-java-class-list.


(defconst shu-java-buffer-name "*Make Project*"
"The name of the buffer into which messages are placed as Java packages
are being scanned.")

(defconst shu-java-symbol-chars    "a-zA-Z0-9\\_\\$"
"Define the characters allowed in a Java name.  The exclusion of < and >
for generics is deliberate.  Do not add them here.")

(defconst shu-non-java-symbol-chars    "^a-zA-Z0-9\\_\\<>$"
"Define the characters that may not appear in a Java identifier.")

(defconst shu-java-name-in-jar
      (concat
        "\\s-*"
        "\\("  "[" shu-java-symbol-chars  "/" "]*"  "\\)"
        (regexp-quote "/")
        "\\("  "[" shu-java-symbol-chars "]*"       "\\)")
"Regexp to search for a Java source file or class name in a jar file.")

(defconst shu-whitespace-ss
   (regexp-opt  (list "\b" "\t" "\n" "\v" "\f" "\r" " ") t)
"Regular expression to search for whitespace.
Since the syntax table considers newline to be (among other things) a
comment terminator, the usual \\s- won't work for whitespace that includes
newlines.")


(defconst shu-java-main1-ss
  (concat
   "public"
  shu-whitespace-ss "+"
   "static"
  shu-whitespace-ss "+"
   "void"
  shu-whitespace-ss "+"
   "main"
  shu-whitespace-ss "*"
   "("
  shu-whitespace-ss "*"
   "String"
  shu-whitespace-ss "*"
  "\\["
  shu-whitespace-ss "*"
  "\\]"
  shu-whitespace-ss "+"
  "[" shu-java-symbol-chars "]+"
  shu-whitespace-ss "*"
  ")"
  )
"Search string used to look for Java main() within a file.")
(defconst shu-java-main2-ss
  (concat
   "static"
  shu-whitespace-ss "+"
   "public"
  shu-whitespace-ss "+"
   "void"
  shu-whitespace-ss "+"
   "main"
  shu-whitespace-ss "*"
   "("
  shu-whitespace-ss "*"
   "String"
  shu-whitespace-ss "*"
  "\\["
  shu-whitespace-ss "*"
  "\\]"
  shu-whitespace-ss "+"
  "[" shu-java-symbol-chars "]+"
  shu-whitespace-ss "*"
  ")"
  )
"Search string used to look for Java main() within a file.")

(defconst shu-java-package-ss
;          "package\\s-+\\([a-zA-Z0-9\\.\\_\\$]*\\)\\s-*\\;"
  (concat
    "package"
    shu-whitespace-ss "+"
    "\\([" shu-java-symbol-chars "\\.]+\\)"
    shu-whitespace-ss "*"
    ";"
  )
"Search string used to look for package statement within a file.")

(defconst shu-java-import-ss
;          "\\s-*import\\s-+\\([a-zA-Z0-9\\.\\_\\$]*\\)\\s-*\\;"
;         "\\s-*import\\s-+\\([a-zA-Z0-9\\.\\_\\$]*\\)\\*?\\s-*\\;"
  (concat
    "import"
    shu-whitespace-ss "+"
    "\\([" shu-java-symbol-chars "\\.]+\\)"
    "\\(\\.\\*\\)?"
    shu-whitespace-ss "*"
    ";"
  )
"Search string used to look for import statement within a file.")

(defvar shu-java-comment-starter-ss "//\\|/\\*"
  "Search string to define the start of a comment.")

(defvar shu-java-completion-target
"Global variable used to hold the function to be invoked at the end of the
current completion.")

(defvar shu-java-completion-by-file nil
"Set to t if we are resolving a class ambiguity by file name rather
than package name.")

(defvar shu-java-completion-scratch nil
"Scratch buffer used by Java Completions.")

(defvar shu-java-completion-current-buffer nil
"Active buffer just before we have to do a completion.")

(defvar shu-java-point1 -1
"Beginning point in the buffer of the last default value read for completion.")

(defvar shu-java-point2 -1
"End point in the buffer of the last default value read for completion.")

(defconst shu-java-parsed-buffer "*Parsed Base*"
  "Name of the buffer into which the JDK is parsed.")

(defvar shu-base-jar-file-name "c:/apps/JDKs/1.4.2/src.jar"
  "*Name of the jar file that contains the source for the JDK.")

(defvar shu-project-file-list nil
"List of all the files in the project.  Used for global search and replace.")

(defconst shu-global-buffer-name "*Global search/replace*"
"Name of the buffer used to hold the results of global search and replace")

(defconst shu-type-class 1
"Type indicator for a Java class (as opposed to interface or enum)")

(defconst shu-type-interface 2
"Type indicator for a Java interface (as opposed to class or enum)")

(defconst shu-type-enum 3
"Type indicator for a Java enum (as opposed to class or interface)")

(defvar shu-java-project-list nil
"List that holds all of the subdirectories in the current project.")

(defvar shu-project-errors 0
"Count of errors found when doing set-project")

(defvar shu-project-class-count 0
"Count of classes in project")

(defvar shu-project-user-class-count 0
"Count of classes in project")

;;
;;  make-project
;;
(defun make-project (proj-root)
  "Create a project file of all directories containing .java files.
Starts at the specified root directory and searches all subdirectories for
any that contain .java files.  It then inserts all of the directory names
into the current file at point."
  (interactive "DRoot?: ")
  (let (
;     (gbuf      (get-buffer-create "*Project List*")) ;
    (level     1)
    (dtop      nil)
    (tlist     nil)
    (shu-java-final-list nil)
       )
    (shu-java-project-subdirs (expand-file-name proj-root) level)
    (setq shu-java-final-list (sort shu-java-final-list 'string<))
    (while shu-java-final-list
      (insert (concat (car shu-java-final-list) "\n"))
      (setq shu-java-final-list (cdr shu-java-final-list))
    )
;     (switch-to-buffer gbuf) ;
  )
)

;;
;;  shu-java-project-subdirs
;;
(defun shu-java-project-subdirs (dir-name level)
  (let (
    (gbuf      (get-buffer-create "*Project List*")) ;
;    (pfix (make-string (* 2 level) ?\ )) ;
    (dlist     nil)
    (tlist     nil)
    (sname     nil)
    (dir-list  nil)
    (sub-list  nil)
    (cname     nil)
    (dname     nil)
    (got-java  nil)
    (extension nil)
       )
    (setq dlist (directory-files dir-name t nil t))
    (setq tlist dlist)
    (while tlist
      (setq cname (car tlist))
      (setq sname (file-name-nondirectory cname))
      (if (file-directory-p cname)
        (progn
          (unless (or (string= sname  ".")
                      (string= sname ".."))
          (setq dir-list (cons cname dir-list))))
        (when (not got-java)
          (setq extension (file-name-extension sname))
          (when (string= extension "java")
            (setq got-java t))))
;        (princ (concat pfix cname  " [" extension "]\n") (get-buffer gbuf))) ;
      (setq tlist (cdr tlist))
    )
    (when got-java
      (setq shu-java-final-list (cons dir-name shu-java-final-list)))
    (while dir-list
      (setq dname (car dir-list))
;      (princ (concat pfix dname "/\n") (get-buffer gbuf))
      (shu-java-project-subdirs dname (1+ level))
      (setq dir-list (cdr dir-list))
    )
  )
)

;;
;;  set-project
;;
(defun set-project (start end)
"Mark a region in a file that contains one subdirecdtory name per line.
Then invoke set-project and it will find and remember all of the java
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
        (eol       nil)
        (dir-name  nil)
        (shu-java-buffer (get-buffer-create shu-java-buffer-name))
     (m (copy-marker 300 ) ) )
    (setq debug-on-error t)
    (setq shu-java-project-list nil)
    (goto-char start)               ; Move to region start
                                         ; While we have not reached last line and
    (while (and (<= (current-line) eline) (= line-diff 0)) ; there are more lines
      (setq eol (save-excursion (end-of-line) (point)))
      (when (> eol (point))
      (setq dir-name (buffer-substring (point) eol))
(print dir-name (get-buffer-create "*boo*"))
      (setq shu-java-project-list (cons dir-name shu-java-project-list))
      )
;      (setq key-list (append key-list (shu-add-package-line dir-name)))
      (setq line-diff (forward-line 1))
    )
    (renew-project)
  )
  )
)

;;
;;  renew-project
;;
(defun renew-project ()
  "Renew a previously established project to pick up any new files."
  (interactive)
  (let (
    (gbuf      (get-buffer-create "*SLP Debug*")) ;
    (dir-name    nil)
    (ilist       nil)
    (key-list    nil)
    (list1       nil)
    (props       nil)
    (shu-java-buffer (get-buffer-create shu-java-buffer-name))
    (tlist       nil)
        )
    (setq debug-on-error t)
    (setq shu-project-errors 0)
    (setq shu-project-class-count 0)
    (setq shu-project-user-class-count 0)
    (setq shu-project-file-list nil)
    (setq tlist shu-java-project-list)
    (if (not tlist)
      (progn
        (message "There is no project to renew.")
        (ding))
      (while tlist
        (setq dir-name (car tlist))
        (setq key-list (append key-list (shu-add-package-line dir-name)))
        (setq tlist (cdr tlist))
      )
      ;
      ;  Take all of the files we found and put the list of file names
      ;  in shu-project-file-list to be used for project global changes.
      ;
      (setq ilist key-list)
      (while ilist
        (setq list1 (car ilist))
        (setq props (cdr list1))
        (setq shu-project-file-list
              (cons (shu-class-file-name props) shu-project-file-list))
        (setq shu-project-user-class-count (1+ shu-project-user-class-count))
        (setq ilist (cdr ilist))
      )
      (shu-java-finish-project key-list)
      (princ shu-java-class-list gbuf)
    )
))

;;
;;  shu-java-finish-project
;;
(defun shu-java-finish-project (&optional key-list)
  "Finish constructing a Java project from a user class list or from nil.
If from nil then no user classes are known but the JDK classes are known."
  (let (
    (ilist       nil)
    (cname       nil)
    (props       nil)
    (pkg-name    nil)
    (class-name  nil)
    (item        nil)
    (olist       nil)
    (props-list  nil)
    (limit       nil)
    (nname       nil)
    (list1       nil)
       )
    ;
    ;  Add to all of the classes we found all of the classes from the JDK
    ;  If we didn't find any classes then just use the classes from
    ;  the JDK.
    ;
    (if key-list
      (nconc key-list (shu-get-base-java-list))
      (setq key-list (shu-get-base-java-list)))
    ;
    ; Go through the list, creating an equivalent list that has as its
    ; keys the fully qualified class name.  This gives completion on
    ; both the unqualified name and the fully qualified name.
    ;
    (setq ilist key-list)
    (while ilist
      (setq list1 (car ilist))
      (setq cname (car list1))
      (setq props (cdr list1))
      (setq pkg-name (car props))
      (setq class-name (concat pkg-name "." cname))
      (setq item (cons class-name props))
      (setq olist (cons item olist))
      (setq shu-project-class-count (1+ shu-project-class-count))
      (setq ilist (cdr ilist))
    )
    (nconc key-list olist)

    ;
    ; Now sort the list by name and construct a new list.  Any duplicate names
    ; will contain entries for all of the files that contain the duplicate
    ; class.
    ;
    (setq ilist (sort key-list
                        (lambda(obj1 obj2)
                         (string< (car obj1) (car obj2))
                         )))

    ;
    ; Go through the sorted list, merging the information
    ; for duplicate names.
    ; For each entry in the input list we also call shu-add-to-class-name-list
    ; to add an entry to shu-java-class-name-list.
    ;
    (setq shu-java-class-list nil)
    (setq shu-java-class-name-list nil)
    (setq shu-java-file-name-list nil)
    (while ilist
      (setq list1 (car ilist))
      (setq cname (car list1))    ; Current name
      (setq props (cdr list1))    ; 1st set of properties for current name
      (shu-add-to-class-name-list cname props)
      (setq props-list (list props)) ; list of properties lists for current name
      (setq limit ilist)              ; loop until input list is done or
      (while limit                    ; name changes
        (setq ilist (cdr ilist))
        (setq limit ilist)            ; terminate loop if list is done
        (when ilist                   ; have at least one more entry
          (setq list1 (car ilist))
          (setq nname (car list1))    ; next name
          (setq props (cdr list1))    ; 1st set of properties for next name
          (shu-add-to-class-name-list nname props)
          (if (equal cname nname)     ; Name remains the same
            (setq props-list (cons props props-list)) ; More properties for same name
            (setq limit nil))         ; Name is different - quit the loop
        )
      )
      ; Now have all properties for current name
      (setq props-list (nreverse props-list))
      (setq item (cons cname props-list))
      (setq shu-java-class-list (cons item shu-java-class-list))
    )
    (setq shu-java-class-list (nreverse shu-java-class-list))
    (shu-show-classes)
    (if (= shu-project-errors 0)
      (message "%d user classes, %d total classes defined."
               shu-project-user-class-count shu-project-class-count)
      (if (= shu-project-errors 1)
        (message "Error found in files.  See Project log.")
        (message "%d errors found in files.  See Project log."
                    shu-project-errors)))
))

;;
;;  get-shu-java-class-list
;;
(defun get-shu-java-class-list ()
  (let (
       )
  (when (not shu-java-class-list)
    (message "Loading class definitions.")
    (shu-java-finish-project))
  shu-java-class-list
))

;;
;;  shu-add-to-class-name-list
;;
(defun shu-add-to-class-name-list (name props-item)
  "Add a single entry to shu-java-class-name-list."
  (let (
    (class-name   nil)
    (class-item   nil)
       )
    (setq class-name (concat (shu-class-single-pkg-name props-item) "." name))
    (setq class-item (cons class-name (list props-item)))
    (setq shu-java-class-name-list (cons class-item shu-java-class-name-list))
    (shu-add-to-file-name-list name props-item)
  )
)

;;
;;  shu-add-to-file-name-list
;;
(defun shu-add-to-file-name-list (name props-item)
  "Add a single entry to shu-java-file-name-list."
  (let (
    (file-name   nil)
    (file-item   nil)
       )
    (setq file-name (shu-class-file-name props-item))
    (setq file-item (cons file-name (list props-item)))
    (setq shu-java-file-name-list (cons file-item shu-java-file-name-list))
  )
)

;;
;;  shu-show-classes
;;
(defun shu-show-classes ()
  (let ( (tlist nil)
         (name nil)
         (list1 nil)
         (props-list nil)
         (props nil)
         (pkg-name nil)
         (file-name nil)
         (memberp nil)
         (file-member nil)
       )
    (setq tlist shu-java-class-list)
    (while tlist
      (setq list1 (car tlist))
      (setq name  (car list1))
      (setq props-list (cdr list1))
      (princ (concat name "\n") (get-buffer shu-java-buffer-name))
      (while props-list
        (setq props (car props-list))
        (setq pkg-name (car props))
        (setq file-name (car (cdr props)))
        (setq memberp (cdr (cdr props)))
        (when memberp
          (setq file-member (car memberp)))
        (princ (format "   pkg = %s, file = %s.\n" pkg-name file-name)
               (get-buffer shu-java-buffer-name))
        (setq props-list (cdr props-list))
      )
      (setq tlist (cdr tlist))
    )
  )
)

;;
;;  shu-class-file-name
;;
(defun shu-class-file-name (props-item)
  "Extracts from props the name of the file associated with the class."
  (let (
    (file-name  nil) )
    (setq file-name (car (cdr props-item)))
  file-name
  )
)

;;
;;  shu-class-file-member
;;
(defun shu-class-file-member (props-item)
  "Called with the return value from assoc.  Extracts from it the name
of the file member (if any) associated with the class.  The file member is
the name of the element within a jar file.  If an entry hs no file member
then it is contained within a single file in the file system.  If it has
a member then it is an element in a jar file and must be extracted to be
visited."
  (let (
      (memberp      nil)
      (file-member  nil) )
    (setq memberp (cdr (cdr props-item)))
    (when memberp
      (setq file-member (car memberp)))
  file-member
  )
)

;;
;;  shu-name-has-period
;;
(defun shu-name-has-period (name)
  "Return t if the string has a period in it anywhere."
  (let (
      (i          0)
      (limit      t)
      (np         nil)
       )
    (when (= 0 (length name))
      (setq limit nil))
    (while limit
      (when (equal (aref name i) ?.)
        (setq np t)
        (setq limit nil))
      (if (= i (1- (length name)))
        (setq limit nil)
        (setq i (1+ i)))
    )
  np
  )
)

;;
;;  shu-class-all-file-names
;;
(defun shu-class-all-file-names (assoc-item)
  "Called with the return value from assoc.  Returns a list of all
of the file names associated with the name."
  (let
    ( (props-list nil)
      (props      nil)
      (file-list nil)
      (file-name  nil) )
    (setq props-list (cdr assoc-item))
    (while props-list
      (setq props (car props-list))
      (setq file-name (shu-class-file-name props))
      (setq file-list (cons file-name file-list))
      (setq props-list (cdr props-list))
    )
    (setq file-list (sort file-list 'string<))
  file-list
  )
)

;;
;;  shu-class-class-names
;;
(defun shu-class-class-names (assoc-item)
  "Called with the return value from assoc.  Returns a list of all
of the class names associated with the name."
  (let
    ( (props-list nil)
      (props      nil)
      (name       nil)
      (np         nil)
      (class-list nil)
      (pkg-name   nil)
      (class-name nil)
      (file-name  nil) )
    (setq name (car assoc-item))
    (setq np (shu-name-has-period name))
    (setq props-list (cdr assoc-item))
    (while props-list
      (setq props (car props-list))
      (setq pkg-name (car props))
      (if np
        (setq class-name name)
        (setq class-name (concat pkg-name "." name)))
      (setq class-list (cons class-name class-list))
      (setq props-list (cdr props-list))
    )
    (setq class-list (sort class-list 'string<))
  class-list
  )
)

;;
;;  shu-list-has-duplicates
;;
(defun shu-list-has-duplicates (dlist)
  "Return t if a sorted list of strings contains any duplicate values."
  (let (
    (tlist      dlist)
    (last1      nil)
    (limit      t)
    (has-dups   nil)
       )
    (when (not tlist)
      (setq limit nil))
    (while limit
      (when last1
        (when (string= last1 (car tlist))
          (setq limit nil)
          (setq has-dups t)))
      (setq last1 (car tlist))
      (setq tlist (cdr tlist))
      (when (not tlist)
        (setq limit nil))
    )
  has-dups
  )
)

;;
;;  shu-class-single-class-name
;;
(defun shu-class-single-class-name (assoc-item)
  "Called with the return value from assoc.  Returns a list of all
of the class names associated with the name."
  (let
    ( (props-list nil)
      (props      nil)
      (name       nil)
      (np         nil)
      (pkg-name   nil)
      (class-name nil)
      (file-name  nil) )
    (setq name (car assoc-item))
    (setq np (shu-name-has-period name))
    (setq props-list (cdr assoc-item))
    (setq props (car props-list))
    (setq pkg-name (car props))
    (if np
      (setq class-name name)
      (setq class-name (concat pkg-name "." name)))
  class-name
  )
)

;;
;;  shu-class-single-pkg-name
;;
(defun shu-class-single-pkg-name (props-item)
  "Extract the package name from a single props-item."
  (let
    ( (pkg-name   nil) )
    (setq pkg-name (car props-item))
  pkg-name
  )
)

;;
;;  shu-class-multiple
;;
(defun shu-class-multiple (assoc-item)
  "Called with the return value from assoc.  Determines whether or
not the class has multiple files associated with it."
  (let
    ( (props-list nil)
      (next-file  nil) )
    (setq props-list (cdr assoc-item))
    (setq next-file (cdr props-list))
  next-file
  )
)

;;
;;  shu-add-package-line
;;
(defun shu-add-package-line (dir-name)
  "Called with point at the beginning of the line.  Take the whole line as the
name of a directory, look into the directory, and create an alist of all of the
files in the directory as described in shu-subdir-for-package."
  (let (
     (key-list nil)
     (m (copy-marker 300 ) ) )
     (setq key-list (shu-subdir-for-package dir-name))
  key-list
  )
)

;;
;;  shu-subdir-for-package
;;
(defun shu-subdir-for-package (directory-name)
"Given a subdirectory name return an alist that contains as keys the
names of all of the java files in the subdirectory, and as values the
package name from the source file and the fully qualified name and path
of the java file.  So if the directory \"/u/foo/bar\" contains thing.java
and what.java the returned
alist would be

      ( (\"thing\" (\"foo.bar\"  \"/u/foo/bar/thing.java\"))
        (\"what\"  (\"foo.bar\"  \"/u/foo/bar/what.java\" )) )

This allows us to associate the key \"thing\" with the fully qualified
name \"/u/foo/bar/thing.java\"."
  (let (
     (file-name "")
     (full-name "")
     (item nil)
     (pkg-name nil)
     (class-name nil)
     (key-list nil)
     (directory-list (directory-files directory-name t "[^.]\.java$"))
     (m (copy-marker 400 ) ) )
     (while directory-list
       (setq full-name (car directory-list))
       (setq file-name (file-name-sans-extension (file-name-nondirectory full-name)))
       (setq pkg-name (shu-get-package-name full-name))
       (setq class-name (concat pkg-name "." file-name))
       (setq item (cons file-name (list pkg-name full-name)))
       (setq key-list (cons item key-list))
       (setq directory-list (cdr directory-list))
       (when (not (shu-java-pkg-dir-equal (file-name-directory full-name) pkg-name))
         (setq shu-project-errors (1+ shu-project-errors))
         (princ "***error***\n" (get-buffer shu-java-buffer-name))
         (princ (format "  Package name %s\n" pkg-name)
                                  (get-buffer shu-java-buffer-name))
         (princ (format "  in file %s\n" full-name)
                                  (get-buffer shu-java-buffer-name))
         (princ "  Does not match directory name.\n"
                                  (get-buffer shu-java-buffer-name)))
     )
  key-list
  )
)

;;
;; shu-get-package-name
;;
(defun shu-get-package-name (file-name)
"Given the name of a Java file search it to look for the package statement,
then extract and return the package name.  This always uses a new buffer to
read and scan the file and then kills the buffer when it is done so it has no
affect on any buffers opened by the user."
  (let (
     (pmsg     nil)
     (pkg-name nil)
     (lcount   -1)
     (pkg-buf  (create-file-buffer file-name)))
    (save-current-buffer
      (set-buffer pkg-buf)
      (insert-file-contents file-name t)
      (if (> (buffer-size) 0)
        (setq lcount (count-lines (point-min) (buffer-size)))
        (setq lcount 0))
      (setq pkg-name (shu-get-package-from-buffer))
      (if pkg-name
         (setq pmsg (format "File %s (%d lines) is in package [%s].\n"
                             file-name lcount pkg-name))
         (setq shu-project-errors (1+ shu-project-errors))
         (setq pmsg
             (format "***error*** No package name found in file: %s (%d lines).\n"
                     file-name lcount)))
      (princ pmsg (get-buffer shu-java-buffer-name))
      (kill-buffer pkg-buf)
  pkg-name)
  )
)

;;
;;  shu-get-package-from-buffer
;;
(defun shu-get-package-from-buffer ()
"In the current buffer find the package name from the package statement."
  (let (
     (pkg-name nil)
       )
  (save-excursion
  (save-restriction
    (widen)
    (goto-char (point-min))
    (when (visible-re-search-forward shu-java-package-ss nil)
      (setq pkg-name (match-string 2)))
  ))
  pkg-name
  )
)

;;
;;  shu-java-pkg-dir-equal
;;
(defun shu-java-pkg-dir-equal (dir-name pkg-name)
  "Return t if pkg-name is a subset of the end of dir-name.
Used to verify that package foo.mumble.bar is stored in a directory name
that ends in foo/mumble/bar."
  (let (
    (pkg-list (shu-tokenize-backwards pkg-name ?.))
    (dir-list (shu-tokenize-backwards dir-name ?/))
    (result   t)
    (limit    t)
       )
    (when (or (not pkg-list) (not dir-list))
      (setq limit nil) ; One of them empty, nothing to compare
      (setq result nil)) ; So names don't match
    (while limit
      (when (not (string= (car dir-list) (car pkg-list)))
        (setq result nil)  ; A name component differs
        (setq limit nil))
      (setq dir-list (cdr dir-list))
      (setq pkg-list (cdr pkg-list))
      (when (or (not pkg-list) (not dir-list))
        (setq limit nil)  ; One of the names has ended, so we are done
        (when (and pkg-list (not dir-list)) ; Package has more, Directory doesn't
          (setq result nil))) ; So names don't match
    )
  result
  )
)

;;
;;  shu-tokenize-backwards
;;
(defun shu-tokenize-backwards (name sep-char)
  "Turn a string into a list of tokens delimited by sep-char.
The list is returned in reverse order.  If the input string is
\"abc.def.ghi\" and the separator character is a period, then the
returned list will be (\"ghi\" \"def\" \"abc\")."
  (let (
    (i         0)
    (j         0)
    (c         0)
    (limit     t)
    (current   (copy-sequence name))
    (cstring   "")
    (tok-list  nil)
       )
    (when (= 0 (length name))
      (setq limit nil))
    (while limit
      (setq c (aref name i))      ; If current char is the separator or
      (if (or (equal c sep-char)  ; the input string has ended
              (= i (1- (length name))))
        (progn
          (when (= i (1- (length name))) ; Input string terminates with
            (when (not (equal c sep-char)) ; non-separator
              (aset current j c)         ; Gather last char into last token
              (setq j (1+ j))
            )
          )
          (setq cstring (substring current 0 j))  ; String for this component
          (setq tok-list (cons cstring tok-list)) ; Push it onto the list
          (setq j 0)                     ; Reset output index for next component
        ) ; Separator char or end of input
        (aset current j c)   ; Gather next character of current component
        (setq j (1+ j))
      )
      (if (= i (1- (length name)))       ; End of input
        (setq limit nil)                 ; Terminate the loop
        (setq i (1+ i)))                 ; Not end - bump for next character fetch
    )
  tok-list
  )
)

;;
;;  shu-java-choose-completion
;;
(defadvice choose-completion (after shu-java-choose-completion () disable)
  "Advice that runs after choose-completion to grab the users selected
choice out of the buffer in which choose-completion inserts it."
  (shu-java-common-completion)
)

;;
;;  shu-java-mouse-choose-completion
;;
(defadvice mouse-choose-completion (after shu-java-mouse-choose-completion () disable)
  "Advice that runs after mouse-choose-completion to grab the users selected
choice out of the buffer in which mouse-choose-completion inserts it."
  (shu-java-common-completion)
)

;;
;;  shu-java-common-completion
;;
(defun shu-java-common-completion ()
  "Called when the user hits enter or clicks mouse button 2 on completion window.
At this point the users selected choice is in the current bufffer.  We get the
answer from the current buffer and call the function that is currently
pointed to by shu-java-completion-target."
  (let (
     (eol          (point))
     (bol          nil)
     (answer       nil)
        )
;  (save-excursion
    (beginning-of-line)
    (setq bol (point))
    (setq answer (buffer-substring bol eol))
    (ad-disable-advice 'choose-completion
                       'after 'shu-java-choose-completion)
    (ad-activate 'choose-completion)
    (ad-disable-advice 'mouse-choose-completion
                       'after 'shu-java-mouse-choose-completion)
    (ad-activate 'mouse-choose-completion)

    (delete-other-windows)  ; Get rid of completion window
    (kill-buffer shu-java-completion-scratch)
    (switch-to-buffer shu-java-completion-current-buffer)

    (funcall shu-java-completion-target answer)
    (setq shu-java-completion-target nil)
  )

)

;;
;;  shu-java-file-find-completion
;;
(defun shu-java-file-find-completion (answer)
  "Use answer in completion buffer to determine which file to visit.
We find its key in shu-java-class-name-list.  This gives us the properties
for the class (package name, file name, member name)
which we pass to shu-java-find-file to visit the file."
  (let (
     (assoc-item   nil)
     (props-list   nil)
     (props-item   nil)
     (debug-on-error  t)
       )
    (if shu-java-completion-by-file
      (setq assoc-item (assoc answer shu-java-file-name-list))
      (setq assoc-item (assoc answer shu-java-class-name-list)))

    (setq props-list (cdr assoc-item))
    (setq props-item (car props-list))

    (shu-java-find-file props-item)
  )
)

;;
;;  sop
;;
(defun sop ()
  "Insert System.out.println( in the current buffer at point."
  (interactive)
  (insert "System.out.println(")
)

;;
;;  java-class
;;
(defun java-class ()
  "Place a skeleton class definition in the current buffer at point."
  (interactive)
  (let (
  (bfn         (buffer-file-name))
  (class-name   nil)
       )
  (when (not bfn)
    (setq bfn "class-name.java"))
  (setq class-name (file-name-sans-extension (file-name-nondirectory bfn)))
  (insert (concat
    "\n\n"
    "/**\n"
    " *\n"
    " */\n"
    "public class " class-name "\n"
    "{\n\n"
    "  /**\n"
    "   *  Construct\n"
    "   */\n"
    "  public " class-name "()\n"
    "  {\n\n"
    "  }\n\n"
    "}"))
  (search-backward "{" nil t)
  (forward-char 2)
))

;;
;; Java-main
;;
(defun java-main ()
  "Insert a main method."
  (interactive)
  (let (
    (mainp  nil)
    (debug-on-error t)
       )
  (setq mainp (shu-java-main-location))
  (if mainp
    (progn
      (message "Already have a main() at line %d." (the-line-at mainp))
      (ding))
    (beginning-of-line)
    (insert
      "\n  /**\n   *\n   */\n  public static void main(\n    String[]    args)\n  {\n\n  }\n")
    (goto-char (- (point) 5)))
))

;;
;;  shu-java-main-location
;;
(defun shu-java-main-location ()
  "Return the character location of the main(), if any."
  (let (
(lbuf (get-buffer-create "*foo*"))
    (limit   t)
    (mp      nil)
    (vc      nil)
    (fp      nil)
    (vl      nil)
       )
  (save-excursion
  (save-restriction
    (widen)
      (goto-char (point-max))
      (while limit
        ; First try for "public static void main"
        (setq vc (cs-visible-re-search-backward shu-java-main1-ss nil nil))
        (setq fp (car vc))
        (if fp          ; Found "public static void main"
          (progn
            (setq mp fp) ; Remember where it started
            (setq limit nil))
                              ; No find for "public static void main"
          (setq vl (cdr vc))  ; Get the visibility list and look for
                              ; "static public void main"
          (setq vc (cs-visible-re-search-backward shu-java-main2-ss nil vl))
          (setq fp (car vc))
          (setq mp fp)        ; Point we found (if any)
          (setq limit nil))
     )))
  mp
))

;;
;; try-catch
;;
(defun try-catch (ename start end)
  "Wrap a try/catch around a marked region in a file.
Mark the first line of code to be wrapped in the try block.  Go to the last
line of code to be wrapped in the try block and call this function.  It
will prompt for the name of the Exception to catch."
  (interactive "*sException to catch?: \nr")
  (let (
    (sline (the-line-at start)) ; start line
    (eline (the-line-at end))   ; end line
    (pfix  nil)
       )
    (goto-line sline)
    (beginning-of-line)
    (setq pfix (make-string (minimum-leading-space 24) ?\ ))
    (goto-line (1- sline))
    (end-of-line)
    (insert (concat "\n" pfix "try\n" pfix "{"))
    (goto-line (+ eline 2))
    (end-of-line)
    (insert (concat "\n" pfix "}\n" pfix "catch ( ex)\n" pfix "{\n" pfix "}"))
    (search-backward "catch")
    (search-forward "(")
    (insert ename)
    (search-backward "(")
    (forward-char 1)
  )
)

;;
;;  global-search
;;
(defun global-search (starget)
"Global search"
  (interactive "sSearch target?: ")
  (let (
    (debug-on-error t)
    (spoint    nil)
    (doc       nil)
       )
;      (setq spoint (with-current-buffer gbuf (point)))
      (setq doc (format "global search for [%s]***" starget) )
      (shu-global-operation doc 'shu-gsearch starget)
  )
)

;;
;;  global-trim-blanks
;;
(defun global-trim-blanks ()
"Global trim trailing blanks from all files."
  (interactive)
  (let (
    (debug-on-error t)
    (starget    "xx")
    (replace    t)
    (doc       nil)
       )
      (setq doc "Global trim blanks from all files.")
      (shu-global-operation doc 'shu-global-trim-blanks starget replace)
  )
)

;;
;;  shu-gsearch
;;
(defun shu-gsearch (file target &optional replace)
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
       )
    (goto-char (point-min))
    (when (search-forward target nil t)
      (princ (concat file "\n") (get-buffer gbuf)))
  )
)

;;
;;  global-re-search
;;
(defun global-re-search (starget)
"Global regular expression search"
  (interactive "sRE search target?: ")
  (let (
    (debug-on-error t)
    (spoint    nil)
    (doc       nil)
       )
;      (setq spoint (with-current-buffer gbuf (point)))
      (setq doc (format "global RE search for [%s]***" starget) )
      (shu-global-operation doc 'shu-g-re-search starget)
  )
)

;;
;;  shu-g-re-search
;;
(defun shu-g-re-search (file target &optional replace)
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
       )
    (goto-char (point-min))
    (when (re-search-forward target nil t)
      (princ (concat file "\n") (get-buffer gbuf)))
  )
)

;;
;;  shu-v-re-search-v
;;
(defun shu-v-re-search-v (file target &optional replace)
  "Do a visible-re-search-forward for TARGET, showing each line on
which TARGET appears."
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
    (found1    nil)
    (fp        nil) ;; Point found by search
    (vl        nil) ;; visbility list
    (vc        nil) ;; cons cell returned by cs-visible-re-search-forward
    (eol       nil)
    (bol       nil)
    (line-no   nil)
    (old-line  nil)
    (limit     t)
       )
    (goto-char (point-min))
    (while limit
      (setq vc (cs-visible-re-search-forward target nil vl))
      (setq fp (car vc))
      (setq vl (cdr vc))
      (if (not fp)
        (setq limit nil)
      (when (not found1)
        (princ (concat file "\n") gbuf)
        (setq found1 t))
      (setq eol (save-excursion (end-of-line) (point)))
      (setq bol (save-excursion (beginning-of-line) (point)))
      (setq line-no (the-line-at (point)))
      (setq old-line (buffer-substring bol eol))
      (princ (format "  %d: %s\n" line-no old-line) gbuf)))
  )
)

;;
;;  fixup-protocol
;;
(defun fixup-protocol()
  "Change all of the package names in protocol."
  (interactive)
  (let (
    (debug-on-error t)
    (doc       nil)
    (target "com.ibm.distillery.jmn.nc.mncif")
    (replace "com.ibm.distillery.jmn.comm.reliable")
       )
      (setq doc (format "Fixup protocol for [%s] to [%s]***" target replace) )
      (shu-global-operation doc 'shu-global-replace-string target replace)
  )
)


;;
;;  shu-global-replace-string
;;
(defun shu-global-replace-string (file target replace)
  "Replace all occurrences of TARGET by REPLACE.
Print the name of the file if anything was replaced."
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
    (found1  nil)
       )
  (goto-char (point-min))
  (while (search-forward target nil t)
    (replace-match replace t t)
    (setq found1 t))
  (if found1
    (princ (concat file "\n") (get-buffer gbuf)))
))


;;
;;  shu-global-re-replace-string
;;
(defun shu-global-re-replace-string (file target replace)
  "Replace all occurrences of TARGET by REPLACE using regular expression replacement.
Print the name of the file if anything was replaced."
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
    (found1  nil)
       )
  (goto-char (point-min))
  (while (re-search-forward target nil t)
    (replace-match replace t t)
    (setq found1 t))
  (if found1
    (princ (concat file "\n") (get-buffer gbuf)))
))

;;
;;  shu-global-replace-string-v
;;
(defun shu-global-replace-string-v (file target replace)
  "Replace all occurrences of TARGET by REPLACE.
Print the name of the file if anything was replaced
and show each changed line with its line number."
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
    (found1  nil)
    (eol     nil)
    (bol     nil)
    (line-no  nil)
    (old-line nil)
    (new-line nil)
       )
  (goto-char (point-min))
  (while (search-forward target nil t)
    (when (not found1)
      (princ (concat file "\n") gbuf)
      (setq found1 t))
    (setq eol (save-excursion (end-of-line) (point)))
    (setq bol (save-excursion (beginning-of-line) (point)))
    (setq line-no (the-line-at (point)))
    (setq old-line (buffer-substring bol eol))
    (replace-match replace t t)
    (setq eol (save-excursion (end-of-line) (point)))
    (setq bol (save-excursion (beginning-of-line) (point)))
    (setq new-line (buffer-substring bol eol))
    (princ (format "  %d: %s\n" line-no old-line) gbuf)
    (princ (format "  %d: %s\n" line-no new-line) gbuf))
))

;;
;;  shu-global-trim-blanks
;;
(defun shu-global-trim-blanks (file target &optional replace)
  "Remove all trailing blanks from a file.  TARGET is a dummy
argument and not used.
Print the name of the file if anything was changed."
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
    (found1  nil)
       )
  (make-local-variable 'backup-inhibited)
  (setq backup-inhibited t)
  (goto-char (point-min))
  (while (re-search-forward "[ \t][ \t]*$" nil t)
    (delete-region (match-beginning 0) (point))
    (setq found1 t))
  (if found1
    (princ (concat file "\n") (get-buffer gbuf)))
))

;;
;;  shu-global-operation
;;
(defun shu-global-operation (documentation function-to-call
                             search-target &optional replace)
"Invoke a function on every file in the project.
documentation is the string to put in the buffer to describe the operation."
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
    (spoint    nil)
    (tlist     shu-project-file-list)
    (file      nil)
    (fbuf      nil)
    (file-buf  nil)
       )
    (save-excursion
      (setq spoint (with-current-buffer gbuf (point)))
      (print (concat "***start " documentation) (get-buffer gbuf))
    (switch-to-buffer gbuf)
      (while tlist
        (setq file (car tlist))
        (setq fbuf (get-file-buffer file))
        (if fbuf
          (setq file-buf fbuf)
          (setq file-buf (find-file-noselect file)))

        (set-buffer file-buf)
        (when (not fbuf)
          (make-local-variable 'backup-inhibited)
          (setq backup-inhibited t))
        (funcall function-to-call file search-target replace)
        (if replace  ; Might have modified the file
          (basic-save-buffer))
        (when (not fbuf)  ; We created the file buffer
          (kill-buffer file-buf))
        (setq tlist (cdr tlist))
      )
      (print (concat "***end " documentation) (get-buffer gbuf))
    )
    (switch-to-buffer gbuf)
    (goto-char spoint)
  )
)

;;
;;  shu-global-apply
;;
(defun shu-global-apply (documentation function-to-call)
"Invoke a function on every file in the project.
documentation is the string to put in the buffer to describe the operation.
The function is called once per file.  It is passed two arguments:
The first is the name of the file.  The second is the print buffer
used to log the results of the global operation."
  (let (
    (gbuf      (get-buffer-create shu-global-buffer-name))
    (spoint    nil)
    (tlist     shu-project-file-list)
    (file      nil)
    (fbuf      nil)
    (file-buf  nil)
       )
    (save-excursion
      (setq spoint (with-current-buffer gbuf (point)))
      (print (concat "***start " documentation) (get-buffer gbuf))
    (switch-to-buffer gbuf)
      (while tlist
        (setq file (car tlist))
        (setq fbuf (get-file-buffer file))
        (if fbuf
          (setq file-buf fbuf)
          (setq file-buf (find-file-noselect file)))
        (set-buffer file-buf)
        (funcall function-to-call file gbuf)
        (basic-save-buffer)
        (when (not fbuf)  ; We created the file buffer
          (kill-buffer file-buf))
        (setq tlist (cdr tlist))
      )
      (print (concat "***end " documentation) (get-buffer gbuf))
    )
    (switch-to-buffer gbuf)
    (goto-char spoint)
  )
)

;;
;; import
;;
(defun import ()
"Add an import statement to the current Java file."
  (interactive)
  (let (
    (debug-on-error t)
       )
    (shu-java-resolve "import: " 'shu-java-import-resolve)
  )
)

;;
;;  shu-java-import-resolve
;;
(defun shu-java-import-resolve (answer)
"Do the real work of adding an import statement to a Java file.
This function is called by shu-java-resolve with the fully qualified
name of the class to be imported.  It searches backward for what should
be the last import statement in the file.  If it can't find an import
statement by serching backward, it goes to the top of the file and
searches forward for a package statement.  If it can't find a package
statement it goes to the top of the file.  After establishing the
insert point for the import statement it then searches the whole
file to see if the statement is there already before inserting it."
;
;  See the discussion in Section 29.4 (Narrowing) in the GNU Emacs
;  Lisp programming manual on the way that this function manually
;  saves and restores the buffer instead of using save-restriction.
;  Have to do this because we may modify a part of the buffer that
;  is not visible.
;
  (let (
    (beg (point-min-marker))
    (end (point-max-marker))
    (opos (point-marker))
    (import-point  nil)
    (add-new-line  nil)
    (import-stmt (concat "import " answer ";"))
    (import-search (concat
                       "import"
                       shu-whitespace-ss "+"
                       answer
                       shu-whitespace-ss "*"
                       ";"))
     (post-cr      "")
       )
    (unwind-protect
     (progn
      (save-excursion
      (widen)
      (goto-char (point-max))
      (setq import-point (shu-java-last-import-point))
      (unless import-point
        (setq import-point (shu-java-package-point))
        (if import-point
          (setq add-new-line t)
          (goto-char (point-min))
          (setq add-new-line t)
          (setq post-cr "\n")
          (setq import-point (point))))
      (goto-char (point-min))
      (if (shu-j-all-statement-warning "Already imported at " import-search)
        (ding)
        (goto-char import-point)
        (when add-new-line
          (insert "\n"))
        (insert (concat "\n" import-stmt post-cr))
        (message "At line %d: %s" (current-line) import-stmt))))
      (progn
        (with-current-buffer (marker-buffer beg)
          (narrow-to-region beg end))
        (goto-char (marker-position opos))))
))


;;
;;  shu-java-last-import-point
;;
(defun shu-java-last-import-point ()
  "Return the character location of the end of the last import statement,
if any or nil if none."
  (let (
    (import-point  nil)
       )
  (save-excursion
    (save-restriction
      (widen)
      (when (visible-re-search-backward shu-java-import-ss nil)
        (goto-char (match-end 0))
        (end-of-line)
        (setq import-point (point)))
  ))
  import-point
))

;;
;;  shu-java-package point
;;
(defun shu-java-package-point ()
"In the current buffer return location of package statement or nil"
  (let (
     (ppoint    nil)
     (limit    t)
       )
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (visible-re-search-forward shu-java-package-ss nil)
        (setq ppoint (point))
        (setq limit nil))
    ))
   ppoint
  )
)

;;
;;  shu-j-all-statement-warning
;;
(defun shu-j-all-statement-warning (prefix search-string)
  "Warn if SEARCH-STRING is visible or in a comment in the buffer.
If SEARCH-STRING is either visible in the buffer or found in a comment
in the buffer emit a message telling the user all of the lines on which
the string appears and return t.  If SEARCH-STRING is neither visible
nor in a comment, return nil and don't emit a warning message."
  (let (
    (got1     nil)
    (limit    t)
    (vc       nil)
    (fp       nil)
    (ilist    nil)
    (item     nil)
    (msg      nil)
    (icount   0)
    (gc       nil)
    (got-c    nil)
    (vlist    nil)
       )
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      ; Make a list of cons cells representing all of the matching
      ; statements either in visible code or in comments.  The car of
      ; each cell is the point at which the statement starts.  The cdr
      ; of the same cell is non-nil if the statement is embedded in a
      ; comment.
      (while limit
        (setq vc (cs-visible-re-search-backward-comment search-string nil))
        (setq fp (car vc))
        (if (not fp)
          (setq limit nil)
          (setq vlist (cdr vc))
          (setq gc (visible-in-comment vlist fp))
          (when gc (setq got-c t))
          (setq item (cons fp gc))
          (setq icount (1+ icount))
          (setq ilist (cons item ilist)))
      )
  ))
  (when ilist
    (setq got1 t)
    (setq msg (concat prefix "line"))
    (when (> icount 1)
      (setq msg (concat msg "s")))
    (setq msg (concat msg ": "))
    (setq icount 0)
    (while ilist
      (when (> icount 0)
        (setq msg (concat msg ", ")))
      (setq item (car ilist))
      (setq msg (concat msg (format "%d" (the-line-at (car item)))))
      (when (cdr item)
        (setq msg (concat msg "*")))
      (setq ilist (cdr ilist))
      (setq icount (1+ icount))
    )
    (when got-c
      (setq msg (concat msg " (* = in comment)")))
    (message msg)
  )
 got1
))


;;
;; resolve
;;
(defun resolve ()
"Resolve a Java class name."
  (interactive)
  (let (
    (debug-on-error t)
       )
    (shu-java-resolve "resolve: " 'shu-java-resolve-resolve)
  )
)

;;
;;  shu-java-resolve-resolve
;;
(defun shu-java-resolve-resolve (answer)
  "Do the real work of resolving a Java class name.
If there was no default value read from the buffer then simply insert the
resolved name at point.  If we did read a default value from the buffer
then check to see if the last part of the name is equal to the default
we read from the buffer.  If so we replace the unqualified name with the
fully qualified name."
  (let (
    (endp        -1)
    (resolved    nil)
    (to-delete   nil)
    (lname       nil)
       )
  (if buffer-read-only
    (message answer)
    (if (< shu-java-point1 0)
      (insert answer)
      (setq lname (shu-last-name answer))
      (save-excursion
        (setq endp (- shu-java-point2 (length answer)))
        (when (< endp (point-min))
          (setq endp (point-min)))
        (goto-char shu-java-point2)
        (if (search-backward answer endp t)
          (setq resolved t)
          (when (equal (buffer-substring shu-java-point1 shu-java-point2) lname)
            (setq to-delete t)))
      )
      (if resolved
        (message "Already resolved to: %s" answer)
        (when to-delete
          (goto-char shu-java-point1)
          (delete-region shu-java-point1 shu-java-point2))
        (insert answer))
    )
  )
  )
)

;;
;;  shu-last-name
;;
(defun shu-last-name (name)
  "Return last name in a series of dot separated names."
  (let (
    (lname   name)
    (x          0)
       )
    (setq x (shu-last-period name))
    (when (> x 0)
      (setq lname (substring name (1+ x))))
  lname
  )
)

;;
;;  shu-last-period
;;
(defun shu-last-period (name)
  "Return index to last period in string."
  (let (
      (i          (1- (length name)))
      (limit      t)
      (lp         -1)
       )
    (unless (= 0 (length name))
      (while limit
        (when (equal (aref name i) ?.)
          (setq lp i)
          (setq limit nil))
        (if (= i 0)
          (setq limit nil)
          (setq i (1- i)))
      )
    )
  lp
  )
)

;;
;;  shu-inside-comment
;;
(defun shu-inside-comment ()
  "Return t if point is inside a comment."
  (let (
    (cpoint    (point))
    (spoint    nil)
    (epoint    nil)
    (inside    nil)
       )
    (save-excursion
      (save-restriction
        (widen)
        (setq epoint (buffer-size))
        (if (re-search-backward shu-java-comment-starter-ss nil t)
          (progn
            (setq spoint (point))
            (if (looking-at "//")
              (progn
              (end-of-line)
              (setq epoint (point)))
              (if (looking-at "/\\*")
                (progn
                  (when (re-search-forward "\\*/" nil t)
                    (setq epoint (1- (point)))))))))
      )
    )
    (when spoint
      (when (and (<= spoint cpoint)
                 (>= epoint cpoint))
        (setq inside t)))
    inside
  )
)

;;
;;  shu-java-resolve
;;
(defun shu-java-resolve (invitation target)
"Visit a java file in a package."
;  (interactive)
  (let (
     (key-list nil)
     (file-to-seek nil)
     (default-thing-to-resolve (shu-find-default-symbol))
     (real-thing-to-resolve nil)
     (assoc-item nil)
     (thing-to-resolve nil)
     (assoc-item nil)
;     (invitation "Visit java file: ")
        )
     (if (not target)
       (error "target is nil")
       (when (not (fboundp target))
         (error "target not bound")))
;     (if (not shu-java-class-list)
     (if (not (get-shu-java-class-list))
       (progn
       (message "No project files have been defined.")
       (ding))
     ;
       (while (not assoc-item)
         (setq thing-to-resolve
               (completing-read
                  (if default-thing-to-resolve
                    (format "%s(default %s) "
                                 invitation default-thing-to-resolve)
                      invitation)
                  shu-java-class-list
                  nil
                  nil
                  nil
                  nil
                  default-thing-to-resolve))
         (if (equal thing-to-resolve "")
           (or default-thing-to-resolve (error "There is no default Java file")))

         (setq assoc-item (assoc thing-to-resolve shu-java-class-list))
         (if (not assoc-item)
           (ding))
       )

      (if (not (shu-class-multiple assoc-item) ) ; Single class - no choice
        (funcall target (shu-class-single-class-name assoc-item))
      ; Have multiple classes from which to choose
                                    ; Ask user which one to visit
        (shu-java-resolve-choice assoc-item target)
       )
     )
))

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
(defun vj ()
"Visit a java file in a package."
  (interactive)
  (let (
     (key-list nil)
     (file-to-seek nil)
     (default-file-to-seek (shu-find-default-symbol))
     (real-file-to-seek nil)
     (tfile nil)
     (invitation "Visit java file: ") )

;     (if (not shu-java-class-list)
     (if (not (get-shu-java-class-list))
       (progn
       (message "No project files have been defined.")
       (ding))
     ;
       (while (not tfile)
         (setq file-to-seek
               (completing-read
                  (if default-file-to-seek
                    (format "%s(default %s) " invitation default-file-to-seek)
                      invitation)
                  shu-java-class-list
                  nil
                  nil
                  nil
                  nil
                  default-file-to-seek))
         (if (equal file-to-seek "")
           (or default-file-to-seek (error "There is no default Java file")))

         (setq tfile (assoc file-to-seek shu-java-class-list))
         (if (not tfile)
           (ding))
       )
       (shu-java-choose-file tfile)
  )
))

;
;  shu-find-default-symbol - If point is sitting on something that looks like
;    a Java symbol then return it as a default candidate for the file name
;    we wish to visit.
;
(defun shu-find-default-symbol ()
  (let ((t1 0))
  (setq shu-java-point1 -1)
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
                 (setq shu-java-point1 (point))
                 (setq shu-java-point2 (progn (forward-sexp -1)
                                 (while (looking-at "\\s'")
                                   (forward-char 1))
                                 (point)))
                 (when (> shu-java-point1 shu-java-point2)
                   (progn
                     (setq t1 shu-java-point2)
                     (setq shu-java-point2 shu-java-point1)
                     (setq shu-java-point1 t1)))
                 (buffer-substring shu-java-point1 shu-java-point2 )))
   )

   nil))
))

;;
;;  shu-java-choose-file
;;
(defun shu-java-choose-file (assoc-item)
  "Choose the file to visit for a given assoc-item.  If there is only one
file associated with the class then visit it.  If there are multiple files
put all of the class names in the completion buffer and give the user the
opportunity to select the desired class.  Then visit the file associated
with that class."
  (let (
    (props-list nil)
    (props-item nil)
       )
    (if (not (shu-class-multiple assoc-item) ) ; Single class - no choice
      (progn
        (setq props-list (cdr assoc-item))
        (setq props-item (car props-list))     ; This holds the single file
        (shu-java-find-file props-item)
      )
    ; Have multiple classes from which to choose
                                  ; Ask user which one to visit
      (shu-java-resolve-choice assoc-item 'shu-java-file-find-completion)
    )
  )
)

;;
;;  shu-java-resolve-choice
;;
(defun shu-java-resolve-choice (assoc-item target)
  "Choose from a number of possible Java classes.
We have found an unqualified class name of interest but it resolves to multiple
fully qualified class names.  Display all of the possibilities in a completion
buffer and ask the user to choose the desired one.  The string contaning the
chosen fully qualified class name will then be passed to the function pointed
to by target."
  (let (
    (choice-list (shu-class-class-names assoc-item))
       )
    (setq shu-java-completion-by-file nil)
    (when (shu-list-has-duplicates choice-list)
      (setq shu-java-completion-by-file t)
      (setq choice-list (shu-class-all-file-names assoc-item)))
    (ad-enable-advice 'choose-completion
                      'after 'shu-java-choose-completion)
    (ad-activate 'choose-completion)
    (ad-enable-advice 'mouse-choose-completion
                      'after 'shu-java-mouse-choose-completion)
    (ad-activate 'mouse-choose-completion)
    (setq shu-java-completion-target target)
    (setq shu-java-completion-current-buffer (current-buffer))
    (setq shu-java-completion-scratch (generate-new-buffer "*Java Scratch*"))
    (set-buffer shu-java-completion-scratch)
    (with-output-to-temp-buffer "*Java Completions*"
      (display-completion-list choice-list))
    ; At this point this function exits and control resumes at
    ; the function shu-java-common-completion, which is called
    ; by either shu-java-choose-completion or shu-java-mouse-choose-completion
    ; when the user hits enter or clicks mouse button 2.
  )
)

;;
;;  shu-java-find-file
;;
(defun shu-java-find-file (props-item)
  "Find and open the file or archive member that holds a particular java class."
  (let (
    (file-name nil)
    (file-member nil)
       )
    (setq file-name (shu-class-file-name props-item))
    (setq file-member (shu-class-file-member props-item))
    (if (not file-member)   ; file is not a member of a jar or zip file
      (find-file file-name) ; visit the single file
      (with-temp-buffer     ; File is a member of an archive
        (insert-file-contents file-name t)
        (after-find-file)   ; Required to parse a jar file
        (if (search-forward file-member (buffer-size) t)
          (progn
          (save-excursion
            (archive-extract)
          ))
          (error "Cannot find member %s in file %s" file-member file-name)
        )
      )
    )
  )
)

;;
;;  count-project
;;
(defun count-project ()
  "Count the number of lines of code in a project."
  (interactive)
  (let (
    (pbuf    (get-buffer-create "*foo*"))
    (tlist    shu-java-project-list)
    (tdirs    0)
    (tfiles   0)
    (dfiles   0)
    (dcount   0)
    (tcount   0)
    (rlist)
    (dir-name)
       )
    (if (not tlist)
      (progn
        (message "There is no project to count.")
        (ding))
      (princ "\n" pbuf)
      (princ "   Files    Lines   Directory\n" pbuf)
      (princ "   -----    -----   ---------\n" pbuf)
      (while tlist
        (setq dir-name (car tlist))
        (setq rlist (shu-count-in-directory dir-name pbuf tdirs tfiles tcount))
        (setq dfiles (car rlist))
        (setq rlist  (cdr rlist))
        (setq dcount (car rlist))
        (setq rlist  (cdr rlist))
        (setq tdirs  (car rlist))
        (setq rlist  (cdr rlist))
        (setq tfiles (car rlist))
        (setq tcount (cadr rlist))

        (princ (format "%s, %s,  %s\n"
                   (shu-fixed-format-num dfiles 8)
                   (shu-fixed-format-num dcount 8)
                    dir-name) pbuf)
        (setq tlist (cdr tlist))))
))
;;
;;  shu-count-in-directory
;;
(defun shu-count-in-directory (directory-name pbuf tdirs tfiles tcount)
  (let (
     (full-name "")
     (directory-list (directory-files directory-name t "[^.]\.java$"))
     (xf)
     (nbytes)
     (lcount)
     (pcount)
     (dfiles    0)
     (dcount    0)
        )
     (setq tdirs (1+ tdirs))
     (while directory-list
       (setq full-name (car directory-list))
       (setq dfiles (1+ dfiles))
       (setq tfiles (1+ tfiles))
       (with-temp-buffer
         (setq xf (insert-file-contents full-name))
         (setq nbytes (cadr xf))
         (setq lcount 0)
         (when (> nbytes 0)
           (setq lcount (count-lines (point-min) (point-max))))
         (setq dcount (+ dcount lcount))
         (setq tcount (+ tcount lcount))
         (message "Directories: %d, Files: %d, Lines: %d."
                   tdirs tfiles tcount)
       )
       (setq directory-list (cdr directory-list))
     )
  (list dfiles dcount tdirs tfiles tcount)
  )
)


;;
;;  jar-index
;;
(defun jar-index (jar-root)
  "Insert into the current buffer the contents of all jar files.
All jar files are found and parsed.  In the current buffer we insert the
name of the jar file and its table of contents.  This gives one buffer
that can be searched with regular edit commands."
  (interactive "DRoot?: ")
  (let (
;     (gbuf      (get-buffer-create "*Project List*")) ;
    (level     1)
    (dtop      nil)
    (tlist     nil)
    (shu-java-final-list nil)
       )
    (shu-java-jar-subdirs (expand-file-name jar-root) level)
    (setq shu-java-final-list (sort shu-java-final-list 'string<))
    (while shu-java-final-list
      (insert (concat (car shu-java-final-list) "\n"))
      (setq shu-java-final-list (cdr shu-java-final-list))
    )
;     (switch-to-buffer gbuf) ;
  )
)

;;
;;  shu-java-jar-subdirs
;;
(defun shu-java-jar-subdirs (dir-name level)
  (let (
    (gbuf      (get-buffer-create "*Project List*")) ;
;    (pfix (make-string (* 2 level) ?\ )) ;
    (cbuf      (current-buffer))
    (dlist     nil)
    (tlist     nil)
    (sname     nil)
    (dir-list  nil)
    (sub-list  nil)
    (cname     nil)
    (dname     nil)
    (got-java  nil)
    (extension nil)
    (jar-list  nil)
    (jx        nil)
       )
    (setq dlist (directory-files dir-name t nil t))
    (setq tlist dlist)
    (while tlist
      (setq cname (car tlist))
      (setq sname (file-name-nondirectory cname))
      (if (file-directory-p cname)
        (progn
          (unless (or (string= sname  ".")
                      (string= sname ".."))
          (setq dir-list (cons cname dir-list))))
        (when (not got-java)
          (setq extension (file-name-extension sname))
          (when (or (string= (file-name-extension sname) "jar")
                    (string= (file-name-extension sname) "zip"))
            (setq jar-list (cons cname jar-list)))))
;        (princ (concat pfix cname  " [" extension "]\n") (get-buffer gbuf))) ;
      (setq tlist (cdr tlist))
    )
    (while dir-list
      (setq dname (car dir-list))
;      (princ (concat pfix dname "/\n") (get-buffer gbuf))
      (shu-java-jar-subdirs dname (1+ level))
      (setq dir-list (cdr dir-list))
    )
    (setq jar-list (sort jar-list 'string<))
    (while jar-list
      (setq cname (car jar-list))
      (insert (concat "\n\n**** " cname " ****\n\n"))
      (with-temp-buffer
        (insert-file-contents cname t)
        (after-find-file)   ; Required to parse a jar file
        (setq jx (buffer-string)))
      (insert jx)
      (setq jar-list (cdr jar-list))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;UNDER CONSTRUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;UNDER CONSTRUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;UNDER CONSTRUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This part of the file is a collection of functions that will probably
;; be useful in the future but are not yet used for antything.


;;
;;  shu-print-classes-in-buffer
;;
(defun shu-print-classes-in-buffer ()
  "Find and print the names of all the classes and interfaces in buffer."
  (let (
    (gg (get-buffer-create "*foo*"))
    (vc     nil)
    (bl     nil)
    (vl     nil)
       )
  (save-excursion
;  (setq vc (shu-get-block-list))
;  (setq bl (car vc))
;  (setq vl (cdr vc))
;  (print bl gg)
  (shu-get-class-list)
)))

;;
;;  shu-get-class-list
;;
(defun shu-get-class-list (&optional bl vlist)
  "Get a list of all the classes or interfaces in a Java program.
BL is the block list.  VLIST is an optional visibility list.
At the moment all this function does is print the names of any classes
or interfaces found in the file.  Need to extend it to look for an
extends list and an implements list after the class or interface name
and then return some useful data structure."
;
;  Need to do something about the fact that this doesn't yet handle
;  inner classes.  All found classes appear to be at the same level.
;  Can use the block list for this.  For each found class save its
;  block list entry.  Then after finding all of the classes use their
;  respective block list entries to see which ones are inside others.
;
;  As of 13 Dec 05 this remains a work in progress.  There is code
;  here to find the block for each class.  Once we have the block for
;  each class then we can, for each found class, find the containing
;  class.
;
  (let (
    (gg (get-buffer-create "*foo*"))
    (cint-ss
       (concat
         shu-whitespace-ss "+"
         (regexp-opt (list "class" "interface" "enum") t)
         shu-whitespace-ss "+"))
    (cname-ss    (concat "\\("  "[" shu-java-symbol-chars "]+"  "\\)"))
    (b-ss        (regexp-opt (list "{" "}") nil))
    (limit       t)        ; Loop guard
    (vl          vlist)    ; Visibility list
    (vc          nil)      ; cons cell returned by visible searches
    (fp          nil)      ; Point found by a visible search
    (sp          nil)      ; Starting point for "class/interface" search
    (ep          nil)      ; Ending point for "class/interface" search
    (type-name   nil)      ; The found key word "class" or "interface"
    (class-name  nil)      ; Name of class or interface found
    (pm          nil)      ; Used to format error message
    (xtype       nil)      ; 1 for "class", 2 for "interface"
    (ecn         nil)      ; Point following class name
    (cbound      nil)      ; cons cell to define spase between class
                           ;  name and opening brace for class
    (ctype       nil)      ; cons cell holding xtype
    (clist       nil)      ; Information about one class
    (class-list  nil)      ; Returned class list
    (bl          nil)      ; Block list
    (tlist       nil)
    (tl          nil)
    (ccontain    nil)
    (item        nil)
    (blist       nil)
    (bbound      nil)
    (lastbl      nil)
    (debug-on-error t)
    (case-fold-search nil) ; Search respects case
       )

  (while limit
    (setq vc (cs-visible-re-search-forward cint-ss nil vl))
    (setq fp (car vc))
    (setq vl (cdr vc))
    (if (not fp)
      (setq limit nil)
      (setq sp (match-beginning 1))
      (setq vc (cs-visible-re-search-forward b-ss nil vl))
      (setq fp (car vc))
      (setq vl (cdr vc))
      (backward-char 1)
      (when (and fp (looking-at "{"))
      (setq ep (point))
      (goto-char ep)            ; End of search space
      ;
      ; We are now positioned at an opening brace, which is the starting
      ; point for the backward search.  The ending point for the search is
      ; either the end of the last block (closing brace of a block that
      ; does not contain this block) or the start of the last block
      ; (opening brace of a block that does contain us), whichever one of
      ; them is closest to the starting point for the search.  If within
      ; this spece a visible-re-search-backward yields the word class or
      ; interface then this block is the start of a class or interface
      ; declaration.
      ;
      (setq class-name nil)
      (setq vc (cs-visible-re-search-backward cint-ss sp vl))
      (setq fp (car vc))
      (setq vl (cdr vc))
      (when fp
        (setq type-name (match-string 2))  ; The key word we found
        (goto-char (1+ (match-end 2)))
        (setq vc (cs-visible-re-search-forward cname-ss ep vl))
        (setq fp (car vc))
        (setq vl (cdr vc))
        (if fp
          (progn
            (setq class-name (match-string 1)) ; Name of the class or interface
            (setq ecn (match-end 1))      ; Point following name
          )
          (setq pm
            (format "***error*** At line %d, \"%s\" not followed by name."
               (the-line-at (point)) type-name))
          (print pm gg)))
      (when class-name
      (setq xtype shu-type-class)
      (when (string= type-name "interface")
        (setq xtype shu-type-interface))
      (when (string= type-name "enum")
        (setq xtype shu-type-enum))
      ; cbound defines the region in which any "extends" or "implements"
      ; will be found.  It is the point following the class name up to
      ; the open brace for the class
      (setq cbound (cons ecn ep))
      (setq ccontain (cons nil cbound))
      (setq ctype (cons xtype ccontain))
      ;
      ; clist is now a list containing four cons cells
      ;
      ; (car 0) - Name of the class
      ; (car 1) - 1 for "class", 2 for "interface", 3 for "enum"
      ; (car 2) - Containing class if inner class
      ; (car 3) - point following the class name
      ; (cdr 3) - Point of opening brace for class
      (setq clist (cons class-name ctype))
      (setq class-list (cons clist class-list))

        (print (format "   %s %s" type-name class-name) gg))
  )))
  (when class-list)
    (setq class-list (nreverse class-list))

  (print (length class-list) gg)

  ; If we found more than one class in the file then one of the classes
  ; might be an inner class.  We can only discover this by getting the
  ; block list for the buffer and then finding the block that
  ; corresponds to each class.  Once we know the scope of each class we
  ; can discover which class might contain others.
  (when (> (length class-list) 1)
    (setq vc (shu-get-block-list vl))
    (setq bl (car vc))
    (setq vl (cdr vc))
    (setq lastbl bl)
    ; For all the classes that we found
    (setq tlist class-list)
    (while tlist
      (setq clist (car tlist))
      ; For all remaining entries in the block list
      (setq limit t)
      (setq tl lastbl)    ; Start with the last block matched
      (while limit
        ; Bound for this block
        (setq bbound (car tl))
        ; The two points for the class
        (setq cbound (cdr (cdr (cdr clist))))
        ; Opening brace for class is at same point as block start
        (when (= (cdr cbound) (car bbound))
          (setq limit nil)
          (setq lastbl tl)
          (setq item (cons clist bbound))
          (setq blist (cons item blist)))
        (setq tl (cdr tl))
        (when (not tl)
          (setq limit nil))
      )
      (setq tlist (cdr tlist)))
      (when blist
        (setq blist (nreverse blist)))
(print blist gg)
  )
  (print class-list gg)
  (print bl gg)
))


;;
;;  shu-get-block-list
;;
(defun shu-get-block-list (&optional vl)
  "Get a list of all the blocks in a C++ or Java program.
VL is an optional visibility list.  Returns a cons cell.  The car
contains a list of cons cells, each of which is the start and end
point of a block (delimited by {}) in the program.  The cdr is the
visibility list, which has been updated."
  (let (
    (limit     t)       ; Loop guard
    (vc        nil)     ; Cons cell returned by vislible search
    (fp        nil)     ; Point at which visible search found "{"
    (pl        nil)     ; List of points containing "{"
    (lcnt      0)       ; Count of "{"
    (rcnt      0)       ; Count of "}"
    (b-ss      (regexp-opt (list "{" "}") nil))
    (bl        nil)     ; Block list
    (sp        nil)     ; Start point of current block
    (ep        nil)     ; End point of current block
    (item      nil)
    (bstack    nil)
    (depth     0)
       )
  (save-excursion
  (save-restriction
  (widen)
  (goto-char (point-min))
  ;
  ; First find all of the visible open and close braces.  If the open
  ; count does not match the close count there is a problem with the
  ; file and we can't get the block list from it.
  (while limit
    (setq vc (cs-visible-re-search-forward b-ss nil vl))
    (setq fp (car vc))
    (if (not fp)
      (setq limit nil)
      (setq vl (cdr vc))
      (backward-char 1)
      (if (looking-at "{")
        (progn
          (setq item (cons (point) nil))
          (push item bstack)
          (setq depth (1+ depth)))
        (if (< depth 1)
          (progn
            (error "slp stack underflow"))
          (setq depth (1- depth))
          (setq item (pop bstack))
          (setcdr item (point))
          (setq bl (cons item bl))))
      (forward-char 1))
  )
    (when bl
      (setq bl (sort bl
                        (lambda(obj1 obj2)
                         (< (car obj1) (car obj2))
                         ))))
  (cons bl vl)
))))


;;
;;  shu-parse-class-jar
;;
(defun shu-parse-class-jar (jar-file-name)
  "Parse a jar file full of classes to extract all of the class names."
  (interactive)
  (let (
(lbuf (get-buffer-create "*foo*"))
    (shu-java-buffer (get-buffer-create shu-java-parsed-buffer))
    (file-ss
      (concat
        shu-java-name-in-jar
        (regexp-quote ".class")))
    (pkg-ss   shu-java-package-ss)
    (pkg-buf  (create-file-buffer jar-file-name))
    (file-name nil)
    (root-name nil)
    (class-name nil)
    (pkg-list nil)
    (pkg-name nil)
    (item nil)
    (key-list nil)
    (tlist nil)
    (lname nil)
    (pmsg nil)
    (props nil)
    (em nil)
    (nf   0)
    (ec   0)
    (no-inner  t)
    (inner-chars   nil)
    (cname   nil)
    (cname-ss   nil)
    (nname   nil)
    (limit   nil)
    (nitem   nil)
       )
    (if (not no-inner)
      (setq inner-chars
        (concat (regexp-quote "$")
                "[0-9]+"))
      (setq inner-chars
        (concat (regexp-quote "$")
                "[" shu-java-symbol-chars "]+")))

    (with-temp-buffer
    (insert-file-contents jar-file-name t)
    (after-find-file)   ; Required to parse a jar file
    (while (re-search-forward file-ss nil t)
      (save-excursion
        (setq root-name (match-string-no-properties 1))
        (setq class-name (match-string-no-properties 2))
        (setq file-name (concat root-name "/" class-name ".class"))
        (setq pkg-list (nreverse (shu-tokenize-backwards root-name ?/)))
        (setq pkg-name nil)
        (when pkg-list
          (setq pkg-name (concat (car pkg-list)))
          (setq pkg-list (cdr pkg-list))
          (while pkg-list
            (setq pkg-name (concat pkg-name "." (car pkg-list)))
            (setq pkg-list (cdr pkg-list))))
          (setq pmsg (format "File %s is in package [%s].\n"
                              file-name pkg-name))
        (setq nf (1+ nf))
        (princ pmsg shu-java-buffer)
        (if (= ec 0)
          (message "Files: %d." nf)
          (message "Files: %d, Errors: %d." nf ec))
        (setq props (list pkg-name root-name))
        (setq item (cons class-name (list props)))
        (setq tlist (cons item tlist))
  )))
   ; tlist now has an entry for every Java class in the file including
   ; inner classes, both named and anonymous
   (setq tlist (sort tlist
                     (lambda(obj1 obj2)
                      (string< (car obj1) (car obj2))
                       )))
    ; Don't know of a reliable way to eliminate inner classes.  Dollar
    ; sign is legal in a Java name.  Certainly in classes in the JDK a
    ; dollar sign in the name indicates an inner class.  So for the
    ; moment we assume that and go through the list throwing out all
    ; classes that match the previous class name if one were to remove
    ; the dollar sign suffix.

    (setq key-list nil)
    (setq item (car tlist))
    (while tlist
      (setq key-list (cons item key-list))
      (setq cname (car item))    ; Current name
      (setq cname-ss
        (concat cname inner-chars))
      (setq limit tlist)              ; loop until input list is done or
      (while limit                    ; name changes
        (setq tlist (cdr tlist))
        (setq limit tlist)            ; terminate loop if list is done
        (when tlist                   ; have at least one more entry
          (setq nitem (car tlist))
          (setq nname (car nitem))    ; next name
          (unless (posix-string-match cname-ss nname)
            (setq limit nil))         ; Name is different - quit the loop
        )
      )
      ; Now have all properties for current name
      (setq item nitem)
    )
  (setq key-list (sort key-list
                      (lambda(obj1 obj2)
                       (string< (car obj1) (car obj2))
                       )))
  key-list
))

;;;;;;;;;;;;;;;;;;;;;;;;;;UNDER CONSTRUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;UNDER CONSTRUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;UNDER CONSTRUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;;  shu-parse-base-jar
;;
(defun shu-parse-base-jar ()
  "Parse a jar file full of Java source to extract all of the class names."
  (interactive)
  (let (
    (debug-on-error t)
    (shu-java-buffer (get-buffer-create shu-java-parsed-buffer))
    (file-ss
      (concat
        shu-java-name-in-jar
        (regexp-quote ".java")))
    (pkg-ss   shu-java-package-ss)
    (pkg-buf  (create-file-buffer shu-base-jar-file-name))
    (file-name nil)
    (root-name nil)
    (class-name nil)
    (pkg-name nil)
    (item nil)
    (key-list nil)
    (lcount -1)
    (pmsg nil)
    (props nil)
    (em nil)
    (nf   0)
    (ec   0)
(gg (get-buffer-create "*foo*"))
       )
     (save-current-buffer
       (set-buffer pkg-buf)
       (insert-file-contents shu-base-jar-file-name t)
       (after-find-file)   ; Required to parse a jar file
       (while (re-search-forward file-ss nil t)
         (save-excursion
           (setq root-name (match-string-no-properties 1))
           (setq class-name (match-string-no-properties 2))
           (setq file-name (concat root-name "/" class-name ".java"))
           (archive-extract)
(print file-name gg)
(shu-print-classes-in-buffer)

          (setq pkg-name (shu-get-package-from-buffer))
          (if pkg-name
            (progn
              (setq pmsg (format "File %s is in package [%s].\n"
                                 file-name pkg-name))
              (when (not (shu-java-pkg-dir-equal
                           (file-name-directory file-name) pkg-name))
                (setq ec (1+ ec))
                (setq em (concat
                      "***error***\n"
                      "  Package name %s\n"
                      "  in file %s\n"
                      "  Does not match directory name %s.\n"))
                (setq pmsg
                      (format em
                              pkg-name file-name
                              (file-name-directory file-name)))))
            (setq ec (1+ ec))
            (setq pmsg
                (format
                 "***error*** No package name found in file: %s.\n"
                     file-name))
           )
           (setq nf (1+ nf))
           (princ pmsg shu-java-buffer)
           (if (= ec 0)
             (message "Files: %d." nf)
             (message "Files: %d, Errors: %d." nf ec))
           (kill-current-buffer)
           (setq props (list pkg-name root-name))
           (setq item (cons class-name (list props)))
           (setq key-list (cons item key-list))
         )
       )
     )
    (setq key-list (sort key-list
                        (lambda(obj1 obj2)
                         (string< (car obj1) (car obj2))
                         )))
    (print key-list shu-java-buffer)
    (with-current-buffer shu-java-buffer
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" nil t))
    )
    (switch-to-buffer shu-java-buffer)
    (goto-char (point-min))
    (search-forward "\"" nil t)
    (end-of-line) (insert "\"")
    (beginning-of-line) (insert "\"")
    (beginning-of-line)
    (insert "\n***THIS LINE OF CODE DEFINES THE JDK CLASSES***\n")
    (beginning-of-line)
  )
)

;;
;; shu-get-base-java-list
;;
(defun shu-get-base-java-list ()
  "Return the list of all JDK classes, package names, and member names.
shu-base-java-list contains most of the information needed.  Each entry in
it is of the form (name (pkg-name partial-member-name)).  We need a list whose
items are (name (pkg-name file-name member-name)).  We construct a list of
such items and return it."
  (let (
    (tlist        nil)      ; Pointer to current entry in input list
    (titem        nil)      ; One item from the input list
    (name         nil)      ; Name of current entry
    (props-list   nil)      ; Input properties list ((pkg-name partial-member-name))
    (props        nil)      ; Input properties (pkg-name partial-member-name)
    (pkg-name     nil)      ; Package name
    (mem-name     nil)      ; Partial member name
    (member-name  nil)      ; Full member name
    (file-name    nil)      ; Containing archive file name
    (new-props    nil)      ; Output properties (pkg-name file-name member-name)
    (item         nil)      ; Output item (name (pkg-name file-name member-name))
    (olist        nil)      ; Output list
    (shu-java-buffer-name "*Package*")
       )
    (setq tlist shu-base-java-list)
    (while tlist
      (setq titem (car tlist))
      (setq name (car titem))
      (setq props-list (cdr titem))
      (setq props (car props-list))
      (setq pkg-name (car props))
      (setq mem-name (car (cdr props)))
      (setq member-name (concat mem-name "/" name ".java"))
      (setq file-name shu-base-jar-file-name)
      (setq new-props (list pkg-name file-name member-name))
      (setq item (cons name new-props))
      (setq olist (cons item olist))
      (setq tlist (cdr tlist))
    )
  olist
  )
)

(defconst shu-base-java-list (read

"((\"ASAttributeDeclaration\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASCII\" (\"java.util.regex\" \"src/java/util/regex\")) (\"ASCIIReader\" (\"org.apache.xerces.impl.io\" \"src/org/apache/xerces/impl/io\")) (\"ASContentModel\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASDOMImplementationImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ASDataType\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASElementDeclaration\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASEntityDeclaration\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASModel\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASModelImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ASNamedObjectMap\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASNotationDeclaration\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASObject\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ASObjectList\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"AVT\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"AVTPart\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"AVTPartSimple\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"AVTPartXPath\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"AWTError\" (\"java.awt\" \"src/java/awt\")) (\"AWTEvent\" (\"java.awt\" \"src/java/awt\")) (\"AWTEventListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"AWTEventListenerProxy\" (\"java.awt.event\" \"src/java/awt/event\")) (\"AWTEventMulticaster\" (\"java.awt\" \"src/java/awt\")) (\"AWTException\" (\"java.awt\" \"src/java/awt\")) (\"AWTKeyStroke\" (\"java.awt\" \"src/java/awt\")) (\"AWTPermission\" (\"java.awt\" \"src/java/awt\")) (\"AbsPathChecker\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"AbsoluteIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"AbsoluteLocationPath\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"AbsolutePathPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"AbstractAction\" (\"javax.swing\" \"src/javax/swing\")) (\"AbstractActionPropertyChangeListener\" (\"javax.swing\" \"src/javax/swing\")) (\"AbstractBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"AbstractButton\" (\"javax.swing\" \"src/javax/swing\")) (\"AbstractCellEditor\" (\"javax.swing\" \"src/javax/swing\")) (\"AbstractCollection\" (\"java.util\" \"src/java/util\")) (\"AbstractColorChooserPanel\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"AbstractDOMParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"AbstractDateTimeDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"AbstractDocument\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"AbstractFilter\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"AbstractInterruptibleChannel\" (\"java.nio.channels.spi\" \"src/java/nio/channels/spi\")) (\"AbstractLayoutCache\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"AbstractList\" (\"java.util\" \"src/java/util\")) (\"AbstractListModel\" (\"javax.swing\" \"src/javax/swing\")) (\"AbstractMap\" (\"java.util\" \"src/java/util\")) (\"AbstractMethodError\" (\"java.lang\" \"src/java/lang\")) (\"AbstractPreferences\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"AbstractSAXParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"AbstractSelectableChannel\" (\"java.nio.channels.spi\" \"src/java/nio/channels/spi\")) (\"AbstractSelectionKey\" (\"java.nio.channels.spi\" \"src/java/nio/channels/spi\")) (\"AbstractSelector\" (\"java.nio.channels.spi\" \"src/java/nio/channels/spi\")) (\"AbstractSequentialList\" (\"java.util\" \"src/java/util\")) (\"AbstractSet\" (\"java.util\" \"src/java/util\")) (\"AbstractSpinnerModel\" (\"javax.swing\" \"src/javax/swing\")) (\"AbstractTableModel\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"AbstractTranslet\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"AbstractUndoableEdit\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"AbstractView\" (\"org.w3c.dom.views\" \"src/org/w3c/dom/views\")) (\"AbstractWriter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"AbstractXMLDocumentParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"AccessControlContext\" (\"java.security\" \"src/java/security\")) (\"AccessControlException\" (\"java.security\" \"src/java/security\")) (\"AccessController\" (\"java.security\" \"src/java/security\")) (\"AccessException\" (\"java.rmi\" \"src/java/rmi\")) (\"Accessible\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleAction\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleBundle\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleComponent\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleContext\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleEditableText\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleExtendedComponent\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleExtendedTable\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleHTML\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"AccessibleHyperlink\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleHypertext\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleIcon\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleKeyBinding\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleObject\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"AccessibleRelation\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleRelationSet\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleResourceBundle\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleRole\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleSelection\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleState\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleStateSet\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleTable\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleTableModelChange\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleText\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"AccessibleValue\" (\"javax.accessibility\" \"src/javax/accessibility\")) (\"Acl\" (\"java.security.acl\" \"src/java/security/acl\")) (\"AclEntry\" (\"java.security.acl\" \"src/java/security/acl\")) (\"AclNotFoundException\" (\"java.security.acl\" \"src/java/security/acl\")) (\"Action\" (\"javax.swing\" \"src/javax/swing\")) (\"ActionEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ActionListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ActionMap\" (\"javax.swing\" \"src/javax/swing\")) (\"ActionMapUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Activatable\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivateFailedException\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationDesc\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationException\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationGroup\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationGroupDesc\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationGroupID\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationID\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationInstantiator\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationMonitor\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActivationSystem\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"Activator\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"ActiveEvent\" (\"java.awt\" \"src/java/awt\")) (\"AdaptiveResultTreeImpl\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"Adjustable\" (\"java.awt\" \"src/java/awt\")) (\"AdjustmentEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"AdjustmentListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"Adler32\" (\"java.util.zip\" \"src/java/util/zip\")) (\"AffineTransform\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"AffineTransformOp\" (\"java.awt.image\" \"src/java/awt/image\")) (\"AlgorithmParameterGenerator\" (\"java.security\" \"src/java/security\")) (\"AlgorithmParameterGeneratorSpi\" (\"java.security\" \"src/java/security\")) (\"AlgorithmParameterSpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"AlgorithmParameters\" (\"java.security\" \"src/java/security\")) (\"AlgorithmParametersSpi\" (\"java.security\" \"src/java/security\")) (\"AllPermission\" (\"java.security\" \"src/java/security\")) (\"AlphaComposite\" (\"java.awt\" \"src/java/awt\")) (\"AlreadyBoundException\" (\"java.rmi\" \"src/java/rmi\")) (\"AlreadyConnectedException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"AlternativePattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"AncestorEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"AncestorListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"AncestorNotifier\" (\"javax.swing\" \"src/javax/swing\")) (\"AncestorPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"And\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Annotation\" (\"java.text\" \"src/java/text\")) (\"AnyNodeCounter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"AnySimpleDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"AnyURIDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"Applet\" (\"java.applet\" \"src/java/applet\")) (\"AppletContext\" (\"java.applet\" \"src/java/applet\")) (\"AppletInitializer\" (\"java.beans\" \"src/java/beans\")) (\"AppletStub\" (\"java.applet\" \"src/java/applet\")) (\"ApplyImports\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ApplyTemplates\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Arc2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"ArcIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"Area\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"AreaAveragingScaleFilter\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Arg\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"ArgumentList\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ArithmeticException\" (\"java.lang\" \"src/java/lang\")) (\"Array\" (\"java.sql\" \"src/java/sql\")) (\"Array\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"ArrayIndexOutOfBoundsException\" (\"java.lang\" \"src/java/lang\")) (\"ArrayList\" (\"java.util\" \"src/java/util\")) (\"ArrayStoreException\" (\"java.lang\" \"src/java/lang\")) (\"Arrays\" (\"java.util\" \"src/java/util\")) (\"AssertionError\" (\"java.lang\" \"src/java/lang\")) (\"AssertionStatusDirectives\" (\"java.lang\" \"src/java/lang\")) (\"AsyncBoxView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"AsynchronousCloseException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"AttList\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"Attr\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"AttrImpl\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"AttrImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"AttrNSImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"Attribute\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Attribute\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"Attribute\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"AttributeException\" (\"javax.print\" \"src/javax/print\")) (\"AttributeInUseException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"AttributeIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"AttributeList\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"AttributeList\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"AttributeList\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"AttributeListImpl\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"AttributeMap\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"AttributeModificationException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"AttributePSVI\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"AttributePSVImpl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"AttributeSet\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"AttributeSet\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"AttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"AttributeSetMethodGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"AttributeSetUtilities\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"AttributeValue\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"AttributeValue\" (\"java.awt\" \"src/java/awt\")) (\"AttributeValueTemplate\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"AttributedCharacterIterator\" (\"java.text\" \"src/java/text\")) (\"AttributedString\" (\"java.text\" \"src/java/text\")) (\"Attributes\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"Attributes\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"Attributes\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"Attributes\" (\"java.util.jar\" \"src/java/util/jar\")) (\"AttributesImpl\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"AttributesImplSerializer\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"AudioClip\" (\"java.applet\" \"src/java/applet\")) (\"AudioFileFormat\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"AudioFileReader\" (\"javax.sound.sampled.spi\" \"src/javax/sound/sampled/spi\")) (\"AudioFileWriter\" (\"javax.sound.sampled.spi\" \"src/javax/sound/sampled/spi\")) (\"AudioFormat\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"AudioInputStream\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"AudioPermission\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"AudioSystem\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"Augmentations\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"AugmentationsImpl\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"AuthenticationException\" (\"javax.naming\" \"src/javax/naming\")) (\"AuthenticationNotSupportedException\" (\"javax.naming\" \"src/javax/naming\")) (\"Authenticator\" (\"java.net\" \"src/java/net\")) (\"Autoscroll\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"Autoscroller\" (\"javax.swing\" \"src/javax/swing\")) (\"AxesWalker\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"Axis\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"Axis\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"BMPattern\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"BRView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"BackingStoreException\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"BadLocationException\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"BandCombineOp\" (\"java.awt.image\" \"src/java/awt/image\")) (\"BandedSampleModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Base64\" (\"org.apache.xerces.impl.dv.util\" \"src/org/apache/xerces/impl/dv/util\")) (\"Base64\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"Base64BinaryDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"BaseDVFactory\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"BaseMarkupSerializer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"BasicArrowButton\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicAttribute\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"BasicAttributes\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"BasicBorders\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicButtonListener\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicButtonUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicCheckBoxMenuItemUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicCheckBoxUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicColorChooserUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicComboBoxEditor\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicComboBoxRenderer\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicComboBoxUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicComboPopup\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicDesktopIconUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicDesktopPaneUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicDirectoryModel\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicDragGestureRecognizer\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicDropTargetListener\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicEditorPaneUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicFileChooserUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicFormattedTextFieldUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicGraphicsUtils\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicHTML\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicIconFactory\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicInternalFrameTitlePane\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicInternalFrameUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicLabelUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicListUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicLookAndFeel\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicMenuBarUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicMenuItemUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicMenuUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicOptionPaneUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicPanelUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicParserConfiguration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"BasicPasswordFieldUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicPermission\" (\"java.security\" \"src/java/security\")) (\"BasicPopupMenuSeparatorUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicPopupMenuUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicProgressBarUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicRadioButtonMenuItemUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicRadioButtonUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicRootPaneUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicScrollBarUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicScrollPaneUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicSeparatorUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicSliderUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicSpinnerUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicSplitPaneDivider\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicSplitPaneUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicStroke\" (\"java.awt\" \"src/java/awt\")) (\"BasicTabbedPaneUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicTableHeaderUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicTableUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicTestIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"BasicTextAreaUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicTextFieldUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicTextPaneUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicTextUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicToggleButtonUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicToolBarSeparatorUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicToolBarUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicToolTipUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicTransferable\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicTreeUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasicViewportUI\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"BasisLibrary\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"BatchUpdateException\" (\"java.sql\" \"src/java/sql\")) (\"BeanContext\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextChild\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextChildComponentProxy\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextChildSupport\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextContainerProxy\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextEvent\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextMembershipEvent\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextMembershipListener\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextProxy\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextServiceAvailableEvent\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextServiceProvider\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextServiceProviderBeanInfo\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextServiceRevokedEvent\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextServiceRevokedListener\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextServices\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextServicesListener\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextServicesSupport\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanContextSupport\" (\"java.beans.beancontext\" \"src/java/beans/beancontext\")) (\"BeanDescriptor\" (\"java.beans\" \"src/java/beans\")) (\"BeanInfo\" (\"java.beans\" \"src/java/beans\")) (\"Beans\" (\"java.beans\" \"src/java/beans\")) (\"BevelBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"Bidi\" (\"java.text\" \"src/java/text\")) (\"BigDecimal\" (\"java.math\" \"src/java/math\")) (\"BigInteger\" (\"java.math\" \"src/java/math\")) (\"BinOpExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"BinaryRefAddr\" (\"javax.naming\" \"src/javax/naming\")) (\"BindException\" (\"java.net\" \"src/java/net\")) (\"Binding\" (\"javax.naming\" \"src/javax/naming\")) (\"BitArray\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"BitSet\" (\"java.util\" \"src/java/util\")) (\"BitSieve\" (\"java.math\" \"src/java/math\")) (\"Bits\" (\"java.nio\" \"src/java/nio\")) (\"Bits\" (\"java.io\" \"src/java/io\")) (\"Blob\" (\"java.sql\" \"src/java/sql\")) (\"BlockView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"Book\" (\"java.awt.print\" \"src/java/awt/print\")) (\"Bool\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"BoolStack\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"Boolean\" (\"java.lang\" \"src/java/lang\")) (\"BooleanCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"BooleanControl\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"BooleanDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"BooleanExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"BooleanType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"Border\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"BorderFactory\" (\"javax.swing\" \"src/javax/swing\")) (\"BorderLayout\" (\"java.awt\" \"src/java/awt\")) (\"BorderUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"BoundedRangeModel\" (\"javax.swing\" \"src/javax/swing\")) (\"Box\" (\"javax.swing\" \"src/javax/swing\")) (\"BoxLayout\" (\"javax.swing\" \"src/javax/swing\")) (\"BoxView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"BreakDictionary\" (\"java.text\" \"src/java/text\")) (\"BreakIterator\" (\"java.text\" \"src/java/text\")) (\"Buffer\" (\"java.nio\" \"src/java/nio\")) (\"BufferCapabilities\" (\"java.awt\" \"src/java/awt\")) (\"BufferOverflowException\" (\"java.nio\" \"src/java/nio\")) (\"BufferStrategy\" (\"java.awt.image\" \"src/java/awt/image\")) (\"BufferUnderflowException\" (\"java.nio\" \"src/java/nio\")) (\"BufferedImage\" (\"java.awt.image\" \"src/java/awt/image\")) (\"BufferedImageFilter\" (\"java.awt.image\" \"src/java/awt/image\")) (\"BufferedImageOp\" (\"java.awt.image\" \"src/java/awt/image\")) (\"BufferedInputStream\" (\"java.io\" \"src/java/io\")) (\"BufferedOutputStream\" (\"java.io\" \"src/java/io\")) (\"BufferedReader\" (\"java.io\" \"src/java/io\")) (\"BufferedWriter\" (\"java.io\" \"src/java/io\")) (\"Button\" (\"java.awt\" \"src/java/awt\")) (\"ButtonGroup\" (\"javax.swing\" \"src/javax/swing\")) (\"ButtonModel\" (\"javax.swing\" \"src/javax/swing\")) (\"ButtonPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"ButtonUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Byte\" (\"java.lang\" \"src/java/lang\")) (\"ByteArrayInputStream\" (\"java.io\" \"src/java/io\")) (\"ByteArrayOutputStream\" (\"java.io\" \"src/java/io\")) (\"ByteBuffer\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsCharBufferB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsCharBufferL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsCharBufferRB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsCharBufferRL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsDoubleBufferB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsDoubleBufferL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsDoubleBufferRB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsDoubleBufferRL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsFloatBufferB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsFloatBufferL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsFloatBufferRB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsFloatBufferRL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsIntBufferB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsIntBufferL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsIntBufferRB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsIntBufferRL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsLongBufferB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsLongBufferL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsLongBufferRB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsLongBufferRL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsShortBufferB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsShortBufferL\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsShortBufferRB\" (\"java.nio\" \"src/java/nio\")) (\"ByteBufferAsShortBufferRL\" (\"java.nio\" \"src/java/nio\")) (\"ByteChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"ByteLookupTable\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ByteOrder\" (\"java.nio\" \"src/java/nio\")) (\"CDATASection\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"CDATASectionImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"CMAny\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"CMBinOp\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"CMBuilder\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"CMLeaf\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"CMMException\" (\"java.awt.color\" \"src/java/awt/color\")) (\"CMNode\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"CMNodeFactory\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"CMStateSet\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"CMUniOp\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"CRC32\" (\"java.util.zip\" \"src/java/util/zip\")) (\"CRL\" (\"java.security.cert\" \"src/java/security/cert\")) (\"CRLException\" (\"java.security.cert\" \"src/java/security/cert\")) (\"CSS\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"CSS2Properties\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSCharsetRule\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSFontFaceRule\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSImportRule\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSMediaRule\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSPageRule\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSParser\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"CSSPrimitiveValue\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSRule\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSRuleList\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSStyleDeclaration\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSStyleRule\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSStyleSheet\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSUnknownRule\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSValue\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CSSValueList\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"CachedNodeListIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"CachedXPathAPI\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"CachingParserPool\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"Calendar\" (\"java.util\" \"src/java/util\")) (\"CallFunction\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"CallTemplate\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CallableStatement\" (\"java.sql\" \"src/java/sql\")) (\"CancelablePrintJob\" (\"javax.print\" \"src/javax/print\")) (\"CancelledKeyException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"CannotProceedException\" (\"javax.naming\" \"src/javax/naming\")) (\"CannotRedoException\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"CannotUndoException\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"CanonicalPath\" (\"java.io\" \"src/java/io\")) (\"Canvas\" (\"java.awt\" \"src/java/awt\")) (\"CanvasPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"CardLayout\" (\"java.awt\" \"src/java/awt\")) (\"Caret\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"CaretEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"CaretListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"CastCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CastExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CeilingCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CellEditor\" (\"javax.swing\" \"src/javax/swing\")) (\"CellEditorListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"CellRendererPane\" (\"javax.swing\" \"src/javax/swing\")) (\"CenterLayout\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"CenterLayout\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"Certificate\" (\"java.security.cert\" \"src/java/security/cert\")) (\"Certificate\" (\"java.security\" \"src/java/security\")) (\"CertificateEncodingException\" (\"java.security.cert\" \"src/java/security/cert\")) (\"CertificateException\" (\"java.security.cert\" \"src/java/security/cert\")) (\"CertificateExpiredException\" (\"java.security.cert\" \"src/java/security/cert\")) (\"CertificateNotYetValidException\" (\"java.security.cert\" \"src/java/security/cert\")) (\"CertificateParsingException\" (\"java.security.cert\" \"src/java/security/cert\")) (\"ChangeEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"ChangeListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"ChangedCharSetException\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Channel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"Channels\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"CharArrayIterator\" (\"java.awt.font\" \"src/java/awt/font\")) (\"CharArrayReader\" (\"java.io\" \"src/java/io\")) (\"CharArrayWriter\" (\"java.io\" \"src/java/io\")) (\"CharBuffer\" (\"java.nio\" \"src/java/nio\")) (\"CharConversionException\" (\"java.io\" \"src/java/io\")) (\"CharInfo\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"CharKey\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"CharSequence\" (\"java.lang\" \"src/java/lang\")) (\"CharSet\" (\"java.text\" \"src/java/text\")) (\"Character\" (\"java.lang\" \"src/java/lang\")) (\"CharacterBreakData\" (\"java.text\" \"src/java/text\")) (\"CharacterCodingException\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"CharacterData\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"CharacterDataEditAS\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"CharacterDataImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"CharacterIterator\" (\"java.text\" \"src/java/text\")) (\"CharacterIteratorFieldDelegate\" (\"java.text\" \"src/java/text\")) (\"Charset\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"CharsetDecoder\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"CharsetEncoder\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"CharsetProvider\" (\"java.nio.charset.spi\" \"src/java/nio/charset/spi\")) (\"Checkbox\" (\"java.awt\" \"src/java/awt\")) (\"CheckboxGroup\" (\"java.awt\" \"src/java/awt\")) (\"CheckboxMenuItem\" (\"java.awt\" \"src/java/awt\")) (\"CheckboxMenuItemPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"CheckboxPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"CheckedInputStream\" (\"java.util.zip\" \"src/java/util/zip\")) (\"CheckedOutputStream\" (\"java.util.zip\" \"src/java/util/zip\")) (\"Checksum\" (\"java.util.zip\" \"src/java/util/zip\")) (\"ChildIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"ChildNode\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ChildTestIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"Choice\" (\"java.awt\" \"src/java/awt\")) (\"ChoiceFormat\" (\"java.text\" \"src/java/text\")) (\"ChoicePeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"Choose\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Chromaticity\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"ChunkedIntArray\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"Class\" (\"org.apache.xml.utils.synthetic\" \"src/org/apache/xml/utils/synthetic\")) (\"Class\" (\"java.lang\" \"src/java/lang\")) (\"ClassCastException\" (\"java.lang\" \"src/java/lang\")) (\"ClassCircularityError\" (\"java.lang\" \"src/java/lang\")) (\"ClassDesc\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"ClassFormatError\" (\"java.lang\" \"src/java/lang\")) (\"ClassGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ClassLoader\" (\"java.lang\" \"src/java/lang\")) (\"ClassNotFoundException\" (\"java.lang\" \"src/java/lang\")) (\"Clip\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"Clipboard\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"ClipboardOwner\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"Clob\" (\"java.sql\" \"src/java/sql\")) (\"CloneNotSupportedException\" (\"java.lang\" \"src/java/lang\")) (\"Cloneable\" (\"java.lang\" \"src/java/lang\")) (\"ClonedNodeListIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"ClonerToResultTree\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"ClosedByInterruptException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"ClosedChannelException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"ClosedSelectorException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"Closure\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CodeSource\" (\"java.security\" \"src/java/security\")) (\"CoderMalfunctionError\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"CoderResult\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"CodingErrorAction\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"CollationElementIterator\" (\"java.text\" \"src/java/text\")) (\"CollationKey\" (\"java.text\" \"src/java/text\")) (\"CollationRules\" (\"java.text\" \"src/java/text\")) (\"Collator\" (\"java.text\" \"src/java/text\")) (\"CollatorFactory\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"CollatorFactoryBase\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"Collection\" (\"java.util\" \"src/java/util\")) (\"Collections\" (\"java.util\" \"src/java/util\")) (\"Color\" (\"java.awt\" \"src/java/awt\")) (\"ColorChooserComponentFactory\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"ColorChooserUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ColorConvertOp\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ColorModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ColorPaintContext\" (\"java.awt\" \"src/java/awt\")) (\"ColorSelectionModel\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"ColorSpace\" (\"java.awt.color\" \"src/java/awt/color\")) (\"ColorSupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"ColorUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ComboBoxEditor\" (\"javax.swing\" \"src/javax/swing\")) (\"ComboBoxModel\" (\"javax.swing\" \"src/javax/swing\")) (\"ComboBoxUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ComboPopup\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"Comment\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"Comment\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CommentImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"CommentView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"CommunicationException\" (\"javax.naming\" \"src/javax/naming\")) (\"CompactStringArray\" (\"java.text\" \"src/java/text\")) (\"Comparable\" (\"java.lang\" \"src/java/lang\")) (\"Comparator\" (\"java.util\" \"src/java/util\")) (\"CompareGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"Compile\" (\"org.apache.xalan.xsltc.cmdline\" \"src/org/apache/xalan/xsltc/cmdline\")) (\"Compiler\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"Compiler\" (\"java.lang\" \"src/java/lang\")) (\"CompilerException\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Component\" (\"java.awt\" \"src/java/awt\")) (\"ComponentAdapter\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ComponentColorModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ComponentEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ComponentInputMap\" (\"javax.swing\" \"src/javax/swing\")) (\"ComponentInputMapUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ComponentListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ComponentOrientation\" (\"java.awt\" \"src/java/awt\")) (\"ComponentPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"ComponentSampleModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ComponentUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ComponentView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Composite\" (\"java.awt\" \"src/java/awt\")) (\"CompositeContext\" (\"java.awt\" \"src/java/awt\")) (\"CompositeName\" (\"javax.naming\" \"src/javax/naming\")) (\"CompositeView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"CompoundBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"CompoundControl\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"CompoundEdit\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"CompoundName\" (\"javax.naming\" \"src/javax/naming\")) (\"Compression\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"ConcatCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ConcurrentModificationException\" (\"java.util\" \"src/java/util\")) (\"Conditional\" (\"java.awt\" \"src/java/awt\")) (\"ConfigurationException\" (\"javax.naming\" \"src/javax/naming\")) (\"ConnectException\" (\"java.rmi\" \"src/java/rmi\")) (\"ConnectException\" (\"java.net\" \"src/java/net\")) (\"ConnectIOException\" (\"java.rmi\" \"src/java/rmi\")) (\"Connection\" (\"java.sql\" \"src/java/sql\")) (\"ConnectionEvent\" (\"javax.sql\" \"src/javax/sql\")) (\"ConnectionEventListener\" (\"javax.sql\" \"src/javax/sql\")) (\"ConnectionPendingException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"ConnectionPool\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"ConnectionPoolDataSource\" (\"javax.sql\" \"src/javax/sql\")) (\"ConnectionPoolManager\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"ConsoleHandler\" (\"java.util.logging\" \"src/java/util/logging\")) (\"Constants\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"Constants\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"Constants\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"Constants\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Constants\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"Constants\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"Constructor\" (\"org.apache.xml.utils.synthetic.reflection\" \"src/org/apache/xml/utils/synthetic/reflection\")) (\"Constructor\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"Container\" (\"java.awt\" \"src/java/awt\")) (\"ContainerAdapter\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ContainerEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ContainerListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ContainerOrderFocusTraversalPolicy\" (\"java.awt\" \"src/java/awt\")) (\"ContainerPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"ContainsCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ContentHandler\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"ContentHandler\" (\"java.net\" \"src/java/net\")) (\"ContentHandlerFactory\" (\"java.net\" \"src/java/net\")) (\"ContentModel\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"ContentModelState\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"ContentModelValidator\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"Context\" (\"javax.naming\" \"src/javax/naming\")) (\"ContextMatchStepPattern\" (\"org.apache.xpath.patterns\" \"src/org/apache/xpath/patterns\")) (\"ContextNodeList\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"ContextNotEmptyException\" (\"javax.naming\" \"src/javax/naming\")) (\"ContextualRenderedImageFactory\" (\"java.awt.image.renderable\" \"src/java/awt/image/renderable\")) (\"ContinuationContext\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"ContinuationDirContext\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"Control\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"Control\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"ControlFactory\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"ControllerEventListener\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"ConvolveOp\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Copies\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"CopiesSupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Copy\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CopyOf\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CoreDOMImplementationImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"CoreDocumentImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"CoroutineManager\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"CoroutineParser\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"Counter\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"Counter\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"CountersTable\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"CropImageFilter\" (\"java.awt.image\" \"src/java/awt/image\")) (\"CubicCurve2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"CubicIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"Currency\" (\"java.util\" \"src/java/util\")) (\"CurrencyData\" (\"java.util\" \"src/java/util\")) (\"CurrentCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"CurrentNodeListFilter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"CurrentNodeListIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"Cursor\" (\"java.awt\" \"src/java/awt\")) (\"CustomStringPool\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"Customizer\" (\"java.beans\" \"src/java/beans\")) (\"DFAContentModel\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"DGC\" (\"java.rmi.dgc\" \"src/java/rmi/dgc\")) (\"DOM\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"DOM2DTM\" (\"org.apache.xml.dtm.ref.dom2dtm\" \"src/org/apache/xml/dtm/ref/dom2dtm\")) (\"DOM2DTMdefaultNamespaceDeclarationNode\" (\"org.apache.xml.dtm.ref.dom2dtm\" \"src/org/apache/xml/dtm/ref/dom2dtm\")) (\"DOM2Helper\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"DOM2SAX\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"DOM2TO\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"DOMASBuilder\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"DOMASBuilderImpl\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"DOMASException\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"DOMASWriter\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"DOMAdapter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"DOMBuilder\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"DOMBuilder\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"DOMCache\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"DOMConfiguration\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"DOMConfigurationImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMEnhancedForDTM\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"DOMEntityResolverWrapper\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"DOMError\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"DOMErrorHandler\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"DOMErrorHandlerWrapper\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"DOMErrorImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMException\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"DOMHelper\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"DOMImplementation\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"DOMImplementationAS\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"DOMImplementationCSS\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"DOMImplementationImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMImplementationList\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"DOMImplementationListImpl\" (\"org.apache.xerces.dom3.bootstrap\" \"src/org/apache/xerces/dom3/bootstrap\")) (\"DOMImplementationRegistry\" (\"org.apache.xerces.dom3.bootstrap\" \"src/org/apache/xerces/dom3/bootstrap\")) (\"DOMImplementationSource\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"DOMImplementationSourceImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMInputImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMLocator\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"DOMLocator\" (\"javax.xml.transform.dom\" \"src/javax/xml/transform/dom\")) (\"DOMLocatorImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMMessageFormatter\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMNormalizer\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMOrder\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"DOMOutputImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"DOMParserImpl\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"DOMResult\" (\"javax.xml.transform.dom\" \"src/javax/xml/transform/dom\")) (\"DOMSerializer\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"DOMSerializer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"DOMSerializer\" (\"org.apache.xalan.serialize\" \"src/org/apache/xalan/serialize\")) (\"DOMSerializerImpl\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"DOMSource\" (\"javax.xml.transform.dom\" \"src/javax/xml/transform/dom\")) (\"DOMStringList\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"DOMStringListImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DOMUtil\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"DOMWSFilter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"DOMXSImplementationSourceImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DSAKey\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"DSAKeyPairGenerator\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"DSAParameterSpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"DSAParams\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"DSAPrivateKey\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"DSAPrivateKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"DSAPublicKey\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"DSAPublicKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"DTD\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"DTDConfiguration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"DTDConstants\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"DTDDVFactory\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"DTDDVFactoryImpl\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"DTDGrammar\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"DTDGrammarBucket\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"DTDHandler\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"DTDParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"DTM\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMAxisIterNodeList\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMAxisIterator\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMAxisIteratorBase\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMAxisTraverser\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMChildIterNodeList\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMConfigurationException\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMDOMException\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMDefaultBase\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMDefaultBaseIterators\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMDefaultBaseTraversers\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMDocument\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"DTMDocumentImpl\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMException\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMFilter\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMIterator\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMManager\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMManagerDefault\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMNamedNodeMap\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMNodeIterator\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMNodeList\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMNodeListBase\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMNodeProxy\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMSafeStringPool\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMStringPool\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMTreeWalker\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"DTMWSFilter\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"DTMXRTreeFrag\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"DVFactoryException\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"DataBuffer\" (\"java.awt.image\" \"src/java/awt/image\")) (\"DataBufferByte\" (\"java.awt.image\" \"src/java/awt/image\")) (\"DataBufferDouble\" (\"java.awt.image\" \"src/java/awt/image\")) (\"DataBufferFloat\" (\"java.awt.image\" \"src/java/awt/image\")) (\"DataBufferInt\" (\"java.awt.image\" \"src/java/awt/image\")) (\"DataBufferShort\" (\"java.awt.image\" \"src/java/awt/image\")) (\"DataBufferUShort\" (\"java.awt.image\" \"src/java/awt/image\")) (\"DataFlavor\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"DataFormatException\" (\"java.util.zip\" \"src/java/util/zip\")) (\"DataInput\" (\"java.io\" \"src/java/io\")) (\"DataInputStream\" (\"java.io\" \"src/java/io\")) (\"DataLine\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"DataOutput\" (\"java.io\" \"src/java/io\")) (\"DataOutputStream\" (\"java.io\" \"src/java/io\")) (\"DataSource\" (\"javax.sql\" \"src/javax/sql\")) (\"DataTruncation\" (\"java.sql\" \"src/java/sql\")) (\"DatabaseMetaData\" (\"java.sql\" \"src/java/sql\")) (\"DatagramChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"DatagramPacket\" (\"java.net\" \"src/java/net\")) (\"DatagramSocket\" (\"java.net\" \"src/java/net\")) (\"DatagramSocketImpl\" (\"java.net\" \"src/java/net\")) (\"DatagramSocketImplFactory\" (\"java.net\" \"src/java/net\")) (\"DatatypeException\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"DatatypeValidator\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"Date\" (\"java.util\" \"src/java/util\")) (\"Date\" (\"java.sql\" \"src/java/sql\")) (\"DateDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"DateFormat\" (\"java.text\" \"src/java/text\")) (\"DateFormatSymbols\" (\"java.text\" \"src/java/text\")) (\"DateFormatter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DateTimeAtCompleted\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"DateTimeAtCreation\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"DateTimeAtProcessing\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"DateTimeDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"DateTimeSyntax\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"DayDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"DebugGraphics\" (\"javax.swing\" \"src/javax/swing\")) (\"DebugGraphicsFilter\" (\"javax.swing\" \"src/javax/swing\")) (\"DebugGraphicsInfo\" (\"javax.swing\" \"src/javax/swing\")) (\"DebugGraphicsObserver\" (\"javax.swing\" \"src/javax/swing\")) (\"DecimalDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"DecimalFormat\" (\"java.text\" \"src/java/text\")) (\"DecimalFormatProperties\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"DecimalFormatSymbols\" (\"java.text\" \"src/java/text\")) (\"DecimalFormatting\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"DecimalToRoman\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"DeclHandler\" (\"org.xml.sax.ext\" \"src/org/xml/sax/ext\")) (\"DeepNodeListImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DefaultBoundedRangeModel\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultButtonModel\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultCaret\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DefaultCellEditor\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultColorSelectionModel\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"DefaultComboBoxModel\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultConnectionPool\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"DefaultDesktopManager\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultDocument\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"DefaultEditorKit\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DefaultElement\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"DefaultErrorHandler\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"DefaultErrorHandler\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"DefaultFocusManager\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultFocusTraversalPolicy\" (\"java.awt\" \"src/java/awt\")) (\"DefaultFormatter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DefaultFormatterFactory\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DefaultHSBChooserPanel\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"DefaultHandler\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"DefaultHighlighter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DefaultKeyboardFocusManager\" (\"java.awt\" \"src/java/awt\")) (\"DefaultListCellRenderer\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultListModel\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultListSelectionModel\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultMenuLayout\" (\"javax.swing.plaf.basic\" \"src/javax/swing/plaf/basic\")) (\"DefaultMetalTheme\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"DefaultMutableTreeNode\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"DefaultNode\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"DefaultPersistenceDelegate\" (\"java.beans\" \"src/java/beans\")) (\"DefaultPreviewPanel\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"DefaultRGBChooserPanel\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"DefaultSingleSelectionModel\" (\"javax.swing\" \"src/javax/swing\")) (\"DefaultStyledDocument\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DefaultSwatchChooserPanel\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"DefaultTableCellRenderer\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"DefaultTableColumnModel\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"DefaultTableModel\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"DefaultText\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"DefaultTextUI\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DefaultTreeCellEditor\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"DefaultTreeCellRenderer\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"DefaultTreeModel\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"DefaultTreeSelectionModel\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"DefaultValidationErrorHandler\" (\"org.apache.xerces.jaxp\" \"src/org/apache/xerces/jaxp\")) (\"DefaultXMLDocumentHandler\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"DeferredAttrImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredAttrNSImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredCDATASectionImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredCommentImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredDocumentImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredDocumentTypeImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredElementDefinitionImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredElementImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredElementNSImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredEntityImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredEntityReferenceImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredNode\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredNotationImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredProcessingInstructionImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DeferredTextImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"Deflater\" (\"java.util.zip\" \"src/java/util/zip\")) (\"DeflaterOutputStream\" (\"java.util.zip\" \"src/java/util/zip\")) (\"DelegatingDefaultFocusManager\" (\"javax.swing\" \"src/javax/swing\")) (\"DescendantIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"DesignMode\" (\"java.beans\" \"src/java/beans\")) (\"DesktopIconUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"DesktopManager\" (\"javax.swing\" \"src/javax/swing\")) (\"DesktopPaneUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Destination\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Dialog\" (\"java.awt\" \"src/java/awt\")) (\"DialogPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"Dictionary\" (\"java.util\" \"src/java/util\")) (\"DictionaryBasedBreakIterator\" (\"java.text\" \"src/java/text\")) (\"DigestException\" (\"java.security\" \"src/java/security\")) (\"DigestInputStream\" (\"java.security\" \"src/java/security\")) (\"DigestOutputStream\" (\"java.security\" \"src/java/security\")) (\"DigitList\" (\"java.text\" \"src/java/text\")) (\"DigraphNode\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"Dimension\" (\"java.awt\" \"src/java/awt\")) (\"Dimension2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"DimensionUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"DirContext\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"DirObjectFactory\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"DirStateFactory\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"DirectByteBuffer\" (\"java.nio\" \"src/java/nio\")) (\"DirectByteBufferR\" (\"java.nio\" \"src/java/nio\")) (\"DirectCharBufferRS\" (\"java.nio\" \"src/java/nio\")) (\"DirectCharBufferRU\" (\"java.nio\" \"src/java/nio\")) (\"DirectCharBufferS\" (\"java.nio\" \"src/java/nio\")) (\"DirectCharBufferU\" (\"java.nio\" \"src/java/nio\")) (\"DirectColorModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"DirectDoubleBufferRS\" (\"java.nio\" \"src/java/nio\")) (\"DirectDoubleBufferRU\" (\"java.nio\" \"src/java/nio\")) (\"DirectDoubleBufferS\" (\"java.nio\" \"src/java/nio\")) (\"DirectDoubleBufferU\" (\"java.nio\" \"src/java/nio\")) (\"DirectFloatBufferRS\" (\"java.nio\" \"src/java/nio\")) (\"DirectFloatBufferRU\" (\"java.nio\" \"src/java/nio\")) (\"DirectFloatBufferS\" (\"java.nio\" \"src/java/nio\")) (\"DirectFloatBufferU\" (\"java.nio\" \"src/java/nio\")) (\"DirectIntBufferRS\" (\"java.nio\" \"src/java/nio\")) (\"DirectIntBufferRU\" (\"java.nio\" \"src/java/nio\")) (\"DirectIntBufferS\" (\"java.nio\" \"src/java/nio\")) (\"DirectIntBufferU\" (\"java.nio\" \"src/java/nio\")) (\"DirectLongBufferRS\" (\"java.nio\" \"src/java/nio\")) (\"DirectLongBufferRU\" (\"java.nio\" \"src/java/nio\")) (\"DirectLongBufferS\" (\"java.nio\" \"src/java/nio\")) (\"DirectLongBufferU\" (\"java.nio\" \"src/java/nio\")) (\"DirectShortBufferRS\" (\"java.nio\" \"src/java/nio\")) (\"DirectShortBufferRU\" (\"java.nio\" \"src/java/nio\")) (\"DirectShortBufferS\" (\"java.nio\" \"src/java/nio\")) (\"DirectShortBufferU\" (\"java.nio\" \"src/java/nio\")) (\"DirectoryManager\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"DisplayMode\" (\"java.awt\" \"src/java/awt\")) (\"Div\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"DnDConstants\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DnDEventMulticaster\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"Doc\" (\"javax.print\" \"src/javax/print\")) (\"DocAttribute\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"DocAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"DocFlavor\" (\"javax.print\" \"src/javax/print\")) (\"DocPrintJob\" (\"javax.print\" \"src/javax/print\")) (\"Document\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"Document\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DocumentAS\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"DocumentBuilder\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"DocumentBuilderFactory\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"DocumentBuilderFactoryImpl\" (\"org.apache.xerces.jaxp\" \"src/org/apache/xerces/jaxp\")) (\"DocumentBuilderImpl\" (\"org.apache.xerces.jaxp\" \"src/org/apache/xerces/jaxp\")) (\"DocumentCSS\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"DocumentCache\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"DocumentCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"DocumentEditAS\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"DocumentEvent\" (\"org.w3c.dom.events\" \"src/org/w3c/dom/events\")) (\"DocumentEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"DocumentFilter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"DocumentFragment\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"DocumentFragmentImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DocumentHandler\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"DocumentImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DocumentListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"DocumentName\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"DocumentParser\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"DocumentRange\" (\"org.w3c.dom.ranges\" \"src/org/w3c/dom/ranges\")) (\"DocumentStyle\" (\"org.w3c.dom.stylesheets\" \"src/org/w3c/dom/stylesheets\")) (\"DocumentTraversal\" (\"org.w3c.dom.traversal\" \"src/org/w3c/dom/traversal\")) (\"DocumentType\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"DocumentTypeImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"DocumentView\" (\"org.w3c.dom.views\" \"src/org/w3c/dom/views\")) (\"DomainCombiner\" (\"java.security\" \"src/java/security\")) (\"DontCareFieldPosition\" (\"java.text\" \"src/java/text\")) (\"Double\" (\"java.lang\" \"src/java/lang\")) (\"DoubleBuffer\" (\"java.nio\" \"src/java/nio\")) (\"DoubleDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"DragGestureEvent\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragGestureListener\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragGestureRecognizer\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragSource\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragSourceAdapter\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragSourceContext\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragSourceContextPeer\" (\"java.awt.dnd.peer\" \"src/java/awt/dnd/peer\")) (\"DragSourceDragEvent\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragSourceDropEvent\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragSourceEvent\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragSourceListener\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DragSourceMotionListener\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"Driver\" (\"java.sql\" \"src/java/sql\")) (\"DriverManager\" (\"java.sql\" \"src/java/sql\")) (\"DriverPropertyInfo\" (\"java.sql\" \"src/java/sql\")) (\"DropTarget\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DropTargetAdapter\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DropTargetContext\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DropTargetContextPeer\" (\"java.awt.dnd.peer\" \"src/java/awt/dnd/peer\")) (\"DropTargetDragEvent\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DropTargetDropEvent\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DropTargetEvent\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DropTargetListener\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"DropTargetPeer\" (\"java.awt.dnd.peer\" \"src/java/awt/dnd/peer\")) (\"DupFilterIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"DurationDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"ENTITYDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"EOFException\" (\"java.io\" \"src/java/io\")) (\"EditableView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"EditorKit\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"ElemApplyImport\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemApplyTemplates\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemAttribute\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemAttributeSet\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemCallTemplate\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemChoose\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemComment\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemContext\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ElemCopy\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemCopyOf\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemDesc\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"ElemDesc\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ElemElement\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemEmpty\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemExsltFuncResult\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemExsltFunction\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemExtensionCall\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemExtensionDecl\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemExtensionScript\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemFallback\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemForEach\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemIf\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemLiteralResult\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemMessage\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemNumber\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemOtherwise\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemPI\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemParam\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemSort\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemTemplate\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemTemplateElement\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemText\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemTextLiteral\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemUnknown\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemUse\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemValueOf\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemVariable\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemVariablePsuedo\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemWhen\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"ElemWithParam\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"Element\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"Element\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"Element\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"ElementAvailableCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ElementCSSInlineStyle\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"ElementDefinitionImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ElementEditAS\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"ElementImpl\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"ElementImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ElementIterator\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"ElementNSImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ElementPSVI\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"ElementPSVImpl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"ElementState\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"Ellipse2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"EllipseIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"EmptyBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"EmptyFilter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"EmptyIterator\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"EmptySerializer\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"EmptyStackException\" (\"java.util\" \"src/java/util\")) (\"EncodedKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"Encoder\" (\"java.beans\" \"src/java/beans\")) (\"EncodingInfo\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"EncodingInfo\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"EncodingMap\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"Encodings\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"Encodings\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"EndSelectionEvent\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"Entity\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"Entity\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"EntityDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"EntityImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"EntityReference\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"EntityReferenceImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"EntityResolver\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"EntityResolverWrapper\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"EntityState\" (\"org.apache.xerces.impl.validation\" \"src/org/apache/xerces/impl/validation\")) (\"EntryPair\" (\"java.text\" \"src/java/text\")) (\"EntryPoint\" (\"org.apache.xml.utils.synthetic.reflection\" \"src/org/apache/xml/utils/synthetic/reflection\")) (\"EnumControl\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"EnumSyntax\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"Enumeration\" (\"java.util\" \"src/java/util\")) (\"EnvironmentCheck\" (\"org.apache.xalan.xslt\" \"src/org/apache/xalan/xslt\")) (\"EqualityExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Equals\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Error\" (\"java.lang\" \"src/java/lang\")) (\"ErrorHandler\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"ErrorHandlerWrapper\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"ErrorListener\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"ErrorManager\" (\"java.util.logging\" \"src/java/util/logging\")) (\"ErrorMessages\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_ca\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_ca\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_cs\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_cs\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_de\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_de\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_es\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_es\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_fr\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_fr\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_hu\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_hu\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_it\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_it\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_ja\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_ja\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_ko\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_ko\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_pl\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_pl\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_pt_BR\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_pt_BR\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_ru\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_ru\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_sk\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_sk\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_sl\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_sl\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_tr\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_tr\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_zh\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_zh\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMessages_zh_TW\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ErrorMessages_zh_TW\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ErrorMsg\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"EtchedBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"Event\" (\"org.w3c.dom.events\" \"src/org/w3c/dom/events\")) (\"Event\" (\"java.awt\" \"src/java/awt\")) (\"EventContext\" (\"javax.naming.event\" \"src/javax/naming/event\")) (\"EventDirContext\" (\"javax.naming.event\" \"src/javax/naming/event\")) (\"EventDispatchThread\" (\"java.awt\" \"src/java/awt\")) (\"EventException\" (\"org.w3c.dom.events\" \"src/org/w3c/dom/events\")) (\"EventHandler\" (\"java.beans\" \"src/java/beans\")) (\"EventImpl\" (\"org.apache.xerces.dom.events\" \"src/org/apache/xerces/dom/events\")) (\"EventListener\" (\"org.w3c.dom.events\" \"src/org/w3c/dom/events\")) (\"EventListener\" (\"java.util\" \"src/java/util\")) (\"EventListenerList\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"EventListenerProxy\" (\"java.util\" \"src/java/util\")) (\"EventObject\" (\"java.util\" \"src/java/util\")) (\"EventQueue\" (\"java.awt\" \"src/java/awt\")) (\"EventSetDescriptor\" (\"java.beans\" \"src/java/beans\")) (\"EventTarget\" (\"org.w3c.dom.events\" \"src/org/w3c/dom/events\")) (\"Exception\" (\"java.lang\" \"src/java/lang\")) (\"ExceptionInInitializerError\" (\"java.lang\" \"src/java/lang\")) (\"ExceptionListener\" (\"java.beans\" \"src/java/beans\")) (\"ExpandVetoException\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"ExpandedNameTable\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"ExpiringCache\" (\"java.io\" \"src/java/io\")) (\"ExportException\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"Expression\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"Expression\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Expression\" (\"java.beans\" \"src/java/beans\")) (\"ExpressionContext\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExpressionNode\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"ExpressionOwner\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"ExpressionVisitor\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExsltBase\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ExsltCommon\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ExsltDatetime\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ExsltDynamic\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ExsltMath\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ExsltSets\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ExsltStrings\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ExtendedContentHandler\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ExtendedLexicalHandler\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ExtendedRequest\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"ExtendedResponse\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"ExtendedSAX\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"ExtendedType\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"ExtensionEvent\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"ExtensionHandler\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExtensionHandlerExsltFunction\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExtensionHandlerGeneral\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExtensionHandlerJava\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExtensionHandlerJavaClass\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExtensionHandlerJavaPackage\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExtensionNamespaceSupport\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ExtensionNamespacesManager\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"Extensions\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ExtensionsProvider\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"ExtensionsProvider2\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"ExtensionsTable\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"Externalizable\" (\"java.io\" \"src/java/io\")) (\"FactoryConfigurationError\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"FactoryFinder\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"FactoryFinder\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"Fallback\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FastStringBuffer\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"FeatureDescriptor\" (\"java.beans\" \"src/java/beans\")) (\"Fidelity\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Field\" (\"org.apache.xml.utils.synthetic.reflection\" \"src/org/apache/xml/utils/synthetic/reflection\")) (\"Field\" (\"org.apache.xerces.impl.xs.identity\" \"src/org/apache/xerces/impl/xs/identity\")) (\"Field\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"FieldActivator\" (\"org.apache.xerces.impl.xs.identity\" \"src/org/apache/xerces/impl/xs/identity\")) (\"FieldPosition\" (\"java.text\" \"src/java/text\")) (\"FieldView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"File\" (\"java.io\" \"src/java/io\")) (\"FileCacheImageInputStream\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"FileCacheImageOutputStream\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"FileChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"FileChooserUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"FileDescriptor\" (\"java.io\" \"src/java/io\")) (\"FileDialog\" (\"java.awt\" \"src/java/awt\")) (\"FileDialogPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"FileFilter\" (\"javax.swing.filechooser\" \"src/javax/swing/filechooser\")) (\"FileFilter\" (\"java.io\" \"src/java/io\")) (\"FileHandler\" (\"java.util.logging\" \"src/java/util/logging\")) (\"FileImageInputStream\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"FileImageOutputStream\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"FileInputStream\" (\"java.io\" \"src/java/io\")) (\"FileLock\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"FileLockInterruptionException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"FileNameMap\" (\"java.net\" \"src/java/net\")) (\"FileNotFoundException\" (\"java.io\" \"src/java/io\")) (\"FileOutputStream\" (\"java.io\" \"src/java/io\")) (\"FilePathToURI\" (\"javax.xml.transform.stream\" \"src/javax/xml/transform/stream\")) (\"FilePathToURI\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"FilePermission\" (\"java.io\" \"src/java/io\")) (\"FileReader\" (\"java.io\" \"src/java/io\")) (\"FileSystem\" (\"java.io\" \"src/java/io\")) (\"FileSystemView\" (\"javax.swing.filechooser\" \"src/javax/swing/filechooser\")) (\"FileView\" (\"javax.swing.filechooser\" \"src/javax/swing/filechooser\")) (\"FileWriter\" (\"java.io\" \"src/java/io\")) (\"FilenameFilter\" (\"java.io\" \"src/java/io\")) (\"Filter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"Filter\" (\"java.util.logging\" \"src/java/util/logging\")) (\"FilterExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FilterExprIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"FilterExprIteratorSimple\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"FilterExprWalker\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"FilterGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"FilterInputStream\" (\"java.io\" \"src/java/io\")) (\"FilterIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"FilterOutputStream\" (\"java.io\" \"src/java/io\")) (\"FilterParentPath\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FilterReader\" (\"java.io\" \"src/java/io\")) (\"FilterWriter\" (\"java.io\" \"src/java/io\")) (\"FilteredAbsoluteLocationPath\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FilteredImageSource\" (\"java.awt.image\" \"src/java/awt/image\")) (\"FilteredStepIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"FinalReference\" (\"java.lang.ref\" \"src/java/lang/ref\")) (\"Finalizer\" (\"java.lang.ref\" \"src/java/lang/ref\")) (\"Finishings\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"FixedHeightLayoutCache\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"FlatteningPathIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"FlavorException\" (\"javax.print\" \"src/javax/print\")) (\"FlavorMap\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"FlavorTable\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"Float\" (\"java.lang\" \"src/java/lang\")) (\"FloatBuffer\" (\"java.nio\" \"src/java/nio\")) (\"FloatControl\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"FloatDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"FloatingDecimal\" (\"java.lang\" \"src/java/lang\")) (\"FloorCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FlowLayout\" (\"java.awt\" \"src/java/awt\")) (\"FlowList\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FlowView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"FocusAdapter\" (\"java.awt.event\" \"src/java/awt/event\")) (\"FocusEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"FocusListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"FocusManager\" (\"javax.swing\" \"src/javax/swing\")) (\"FocusTraversalPolicy\" (\"java.awt\" \"src/java/awt\")) (\"Font\" (\"java.awt\" \"src/java/awt\")) (\"FontFormatException\" (\"java.awt\" \"src/java/awt\")) (\"FontMetrics\" (\"java.awt\" \"src/java/awt\")) (\"FontPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"FontRenderContext\" (\"java.awt.font\" \"src/java/awt/font\")) (\"FontUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ForEach\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FormView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"Format\" (\"java.text\" \"src/java/text\")) (\"FormatConversionProvider\" (\"javax.sound.sampled.spi\" \"src/javax/sound/sampled/spi\")) (\"FormatNumberCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Formatter\" (\"java.util.logging\" \"src/java/util/logging\")) (\"ForwardPositionIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"FoundIndex\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"Frame\" (\"java.awt\" \"src/java/awt\")) (\"FramePeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"FrameSetView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"FrameView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"FullDVFactory\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"FuncBoolean\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncCeiling\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncConcat\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncContains\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncCoreExtFunction\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncCount\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncCurrent\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncDoclocation\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncDocument\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"FuncExtElementAvailable\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncExtFunction\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncExtFunctionAvailable\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncFalse\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncFloor\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncFormatNumb\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"FuncGenerateId\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncId\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncKey\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"FuncLang\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncLast\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncLoader\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"FuncLocalPart\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncNamespace\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncNormalizeSpace\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncNot\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncNumber\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncPosition\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncQname\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncRound\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncStartsWith\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncString\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncStringLength\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncSubstring\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncSubstringAfter\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncSubstringBefore\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncSum\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncSystemProperty\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncTranslate\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncTrue\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FuncUnparsedEntityURI\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"Function\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"Function2Args\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"Function3Args\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FunctionAvailableCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FunctionCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"FunctionDef1Arg\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FunctionMultiArgs\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FunctionOneArg\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"FunctionPattern\" (\"org.apache.xpath.patterns\" \"src/org/apache/xpath/patterns\")) (\"FunctionTable\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"GZIPInputStream\" (\"java.util.zip\" \"src/java/util/zip\")) (\"GZIPOutputStream\" (\"java.util.zip\" \"src/java/util/zip\")) (\"GapContent\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"GapVector\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"GatheringByteChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"GeneralPath\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"GeneralPathIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"GeneralSecurityException\" (\"java.security\" \"src/java/security\")) (\"GenerateEvent\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"GenerateIdCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"GetORBPropertiesFileAction\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"GetOpt\" (\"org.apache.xalan.xsltc.cmdline.getopt\" \"src/org/apache/xalan/xsltc/cmdline/getopt\")) (\"GetOptsException\" (\"org.apache.xalan.xsltc.cmdline.getopt\" \"src/org/apache/xalan/xsltc/cmdline/getopt\")) (\"GlyphJustificationInfo\" (\"java.awt.font\" \"src/java/awt/font\")) (\"GlyphMetrics\" (\"java.awt.font\" \"src/java/awt/font\")) (\"GlyphPainter1\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"GlyphPainter2\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"GlyphVector\" (\"java.awt.font\" \"src/java/awt/font\")) (\"GlyphView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"GradientPaint\" (\"java.awt\" \"src/java/awt\")) (\"GradientPaintContext\" (\"java.awt\" \"src/java/awt\")) (\"Grammar\" (\"org.apache.xerces.xni.grammars\" \"src/org/apache/xerces/xni/grammars\")) (\"GraphicAttribute\" (\"java.awt.font\" \"src/java/awt/font\")) (\"Graphics\" (\"java.awt\" \"src/java/awt\")) (\"Graphics2D\" (\"java.awt\" \"src/java/awt\")) (\"GraphicsCallback\" (\"java.awt\" \"src/java/awt\")) (\"GraphicsConfigTemplate\" (\"java.awt\" \"src/java/awt\")) (\"GraphicsConfiguration\" (\"java.awt\" \"src/java/awt\")) (\"GraphicsDevice\" (\"java.awt\" \"src/java/awt\")) (\"GraphicsEnvironment\" (\"java.awt\" \"src/java/awt\")) (\"GraphicsWrapper\" (\"javax.swing\" \"src/javax/swing\")) (\"GrayFilter\" (\"javax.swing\" \"src/javax/swing\")) (\"GregorianCalendar\" (\"java.util\" \"src/java/util\")) (\"GridBagConstraints\" (\"java.awt\" \"src/java/awt\")) (\"GridBagLayout\" (\"java.awt\" \"src/java/awt\")) (\"GridLayout\" (\"java.awt\" \"src/java/awt\")) (\"Group\" (\"java.security.acl\" \"src/java/security/acl\")) (\"Gt\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Gte\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Guard\" (\"java.security\" \"src/java/security\")) (\"GuardedObject\" (\"java.security\" \"src/java/security\")) (\"HRuleView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"HTML\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"HTMLAnchorElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLAnchorElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLAppletElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLAppletElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLAreaElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLAreaElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLBRElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLBRElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLBaseElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLBaseElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLBaseFontElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLBaseFontElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLBodyElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLBodyElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLBuilder\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLButtonElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLButtonElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLCollection\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLCollectionImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLDListElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLDListElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLDOMImplementationImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLDirectoryElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLDirectoryElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLDivElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLDivElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLDocument\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLDocument\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"HTMLDocumentImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLEditorKit\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"HTMLElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLFieldSetElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLFieldSetElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLFontElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLFontElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLFormControl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLFormElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLFormElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLFrameElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLFrameElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLFrameHyperlinkEvent\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"HTMLFrameSetElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLFrameSetElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLHRElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLHRElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLHeadElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLHeadElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLHeadingElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLHeadingElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLHtmlElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLHtmlElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLIFrameElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLIFrameElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLImageElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLImageElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLInputElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLInputElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLIsIndexElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLIsIndexElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLLIElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLLIElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLLabelElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLLabelElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLLegendElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLLegendElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLLinkElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLLinkElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLMapElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLMapElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLMenuElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLMenuElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLMetaElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLMetaElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLModElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLModElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLOListElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLOListElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLObjectElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLObjectElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLOptGroupElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLOptGroupElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLOptionElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLOptionElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLParagraphElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLParagraphElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLParamElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLParamElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLPreElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLPreElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLQuoteElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLQuoteElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLScriptElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLScriptElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLSelectElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLSelectElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLSerializer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"HTMLStyleElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLStyleElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLTableCaptionElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLTableCaptionElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLTableCellElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLTableCellElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLTableColElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLTableColElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLTableElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLTableElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLTableRowElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLTableRowElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLTableSectionElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLTableSectionElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLTextAreaElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLTextAreaElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLTitleElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLTitleElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLUListElement\" (\"org.w3c.dom.html\" \"src/org/w3c/dom/html\")) (\"HTMLUListElementImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"HTMLWriter\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"HTMLdtd\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"Handler\" (\"java.util.logging\" \"src/java/util/logging\")) (\"HandlerBase\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"HasControls\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"HasPositionalPredChecker\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"HashAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"HashDocAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"HashMap\" (\"java.util\" \"src/java/util\")) (\"HashPrintJobAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"HashPrintRequestAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"HashPrintServiceAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"HashSet\" (\"java.util\" \"src/java/util\")) (\"Hashtable\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"Hashtable\" (\"java.util\" \"src/java/util\")) (\"Hashtree2Node\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"HeadlessException\" (\"java.awt\" \"src/java/awt\")) (\"HeapByteBuffer\" (\"java.nio\" \"src/java/nio\")) (\"HeapByteBufferR\" (\"java.nio\" \"src/java/nio\")) (\"HeapCharBuffer\" (\"java.nio\" \"src/java/nio\")) (\"HeapCharBufferR\" (\"java.nio\" \"src/java/nio\")) (\"HeapDoubleBuffer\" (\"java.nio\" \"src/java/nio\")) (\"HeapDoubleBufferR\" (\"java.nio\" \"src/java/nio\")) (\"HeapFloatBuffer\" (\"java.nio\" \"src/java/nio\")) (\"HeapFloatBufferR\" (\"java.nio\" \"src/java/nio\")) (\"HeapIntBuffer\" (\"java.nio\" \"src/java/nio\")) (\"HeapIntBufferR\" (\"java.nio\" \"src/java/nio\")) (\"HeapLongBuffer\" (\"java.nio\" \"src/java/nio\")) (\"HeapLongBufferR\" (\"java.nio\" \"src/java/nio\")) (\"HeapShortBuffer\" (\"java.nio\" \"src/java/nio\")) (\"HeapShortBufferR\" (\"java.nio\" \"src/java/nio\")) (\"HexBin\" (\"org.apache.xerces.impl.dv.util\" \"src/org/apache/xerces/impl/dv/util\")) (\"HexBinaryDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"HiddenTagView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"HierarchyBoundsAdapter\" (\"java.awt.event\" \"src/java/awt/event\")) (\"HierarchyBoundsListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"HierarchyEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"HierarchyListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"Highlighter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"HttpURLConnection\" (\"java.net\" \"src/java/net\")) (\"HyperlinkEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"HyperlinkListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"ICC_ColorSpace\" (\"java.awt.color\" \"src/java/awt/color\")) (\"ICC_Profile\" (\"java.awt.color\" \"src/java/awt/color\")) (\"ICC_ProfileGray\" (\"java.awt.color\" \"src/java/awt/color\")) (\"ICC_ProfileRGB\" (\"java.awt.color\" \"src/java/awt/color\")) (\"IDDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"IDDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"IDREFDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"IDREFDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"IIOByteBuffer\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"IIOException\" (\"javax.imageio\" \"src/javax/imageio\")) (\"IIOImage\" (\"javax.imageio\" \"src/javax/imageio\")) (\"IIOInvalidTreeException\" (\"javax.imageio.metadata\" \"src/javax/imageio/metadata\")) (\"IIOMetadata\" (\"javax.imageio.metadata\" \"src/javax/imageio/metadata\")) (\"IIOMetadataController\" (\"javax.imageio.metadata\" \"src/javax/imageio/metadata\")) (\"IIOMetadataFormat\" (\"javax.imageio.metadata\" \"src/javax/imageio/metadata\")) (\"IIOMetadataFormatImpl\" (\"javax.imageio.metadata\" \"src/javax/imageio/metadata\")) (\"IIOMetadataNode\" (\"javax.imageio.metadata\" \"src/javax/imageio/metadata\")) (\"IIOParam\" (\"javax.imageio\" \"src/javax/imageio\")) (\"IIOParamController\" (\"javax.imageio\" \"src/javax/imageio\")) (\"IIOReadProgressListener\" (\"javax.imageio.event\" \"src/javax/imageio/event\")) (\"IIOReadUpdateListener\" (\"javax.imageio.event\" \"src/javax/imageio/event\")) (\"IIOReadWarningListener\" (\"javax.imageio.event\" \"src/javax/imageio/event\")) (\"IIORegistry\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"IIOServiceProvider\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"IIOWriteProgressListener\" (\"javax.imageio.event\" \"src/javax/imageio/event\")) (\"IIOWriteWarningListener\" (\"javax.imageio.event\" \"src/javax/imageio/event\")) (\"IOException\" (\"java.io\" \"src/java/io\")) (\"Icon\" (\"javax.swing\" \"src/javax/swing\")) (\"IconUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"IconView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"IdKeyPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"IdPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Identity\" (\"java.security\" \"src/java/security\")) (\"IdentityConstraint\" (\"org.apache.xerces.impl.xs.identity\" \"src/org/apache/xerces/impl/xs/identity\")) (\"IdentityHashMap\" (\"java.util\" \"src/java/util\")) (\"IdentityHashtable\" (\"java.beans\" \"src/java/beans\")) (\"IdentityScope\" (\"java.security\" \"src/java/security\")) (\"If\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"IllegalAccessError\" (\"java.lang\" \"src/java/lang\")) (\"IllegalAccessException\" (\"java.lang\" \"src/java/lang\")) (\"IllegalArgumentException\" (\"org.apache.xalan.xsltc.cmdline.getopt\" \"src/org/apache/xalan/xsltc/cmdline/getopt\")) (\"IllegalArgumentException\" (\"java.lang\" \"src/java/lang\")) (\"IllegalBlockingModeException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"IllegalCharException\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"IllegalCharsetNameException\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"IllegalComponentStateException\" (\"java.awt\" \"src/java/awt\")) (\"IllegalMonitorStateException\" (\"java.lang\" \"src/java/lang\")) (\"IllegalPathStateException\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"IllegalSelectorException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"IllegalStateException\" (\"java.lang\" \"src/java/lang\")) (\"IllegalThreadStateException\" (\"java.lang\" \"src/java/lang\")) (\"Image\" (\"java.awt\" \"src/java/awt\")) (\"ImageCapabilities\" (\"java.awt\" \"src/java/awt\")) (\"ImageConsumer\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ImageFilter\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ImageGraphicAttribute\" (\"java.awt.font\" \"src/java/awt/font\")) (\"ImageIO\" (\"javax.imageio\" \"src/javax/imageio\")) (\"ImageIcon\" (\"javax.swing\" \"src/javax/swing\")) (\"ImageInputStream\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"ImageInputStreamImpl\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"ImageInputStreamSpi\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"ImageObserver\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ImageOutputStream\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"ImageOutputStreamImpl\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"ImageOutputStreamSpi\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"ImageProducer\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ImageReadParam\" (\"javax.imageio\" \"src/javax/imageio\")) (\"ImageReader\" (\"javax.imageio\" \"src/javax/imageio\")) (\"ImageReaderSpi\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"ImageReaderWriterSpi\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"ImageTranscoder\" (\"javax.imageio\" \"src/javax/imageio\")) (\"ImageTranscoderSpi\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"ImageTypeSpecifier\" (\"javax.imageio\" \"src/javax/imageio\")) (\"ImageView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"ImageWriteParam\" (\"javax.imageio\" \"src/javax/imageio\")) (\"ImageWriter\" (\"javax.imageio\" \"src/javax/imageio\")) (\"ImageWriterSpi\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"ImagingOpException\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Import\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Include\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"IncompatibleClassChangeError\" (\"java.lang\" \"src/java/lang\")) (\"IncrementalSAXSource\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"IncrementalSAXSource_Filter\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"IncrementalSAXSource_Xerces\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"IndentPrinter\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"IndexColorModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"IndexOutOfBoundsException\" (\"java.lang\" \"src/java/lang\")) (\"IndexedPropertyDescriptor\" (\"java.beans\" \"src/java/beans\")) (\"Inet4Address\" (\"java.net\" \"src/java/net\")) (\"Inet4AddressImpl\" (\"java.net\" \"src/java/net\")) (\"Inet6Address\" (\"java.net\" \"src/java/net\")) (\"Inet6AddressImpl\" (\"java.net\" \"src/java/net\")) (\"InetAddress\" (\"java.net\" \"src/java/net\")) (\"InetAddressImpl\" (\"java.net\" \"src/java/net\")) (\"InetSocketAddress\" (\"java.net\" \"src/java/net\")) (\"Inflater\" (\"java.util.zip\" \"src/java/util/zip\")) (\"InflaterInputStream\" (\"java.util.zip\" \"src/java/util/zip\")) (\"InheritableThreadLocal\" (\"java.lang\" \"src/java/lang\")) (\"InitialContext\" (\"javax.naming\" \"src/javax/naming\")) (\"InitialContextFactory\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"InitialContextFactoryBuilder\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"InitialDirContext\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"InitialLdapContext\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"InlineView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"InputContext\" (\"java.awt.im\" \"src/java/awt/im\")) (\"InputEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"InputMap\" (\"javax.swing\" \"src/javax/swing\")) (\"InputMapUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"InputMethod\" (\"java.awt.im.spi\" \"src/java/awt/im/spi\")) (\"InputMethodContext\" (\"java.awt.im.spi\" \"src/java/awt/im/spi\")) (\"InputMethodDescriptor\" (\"java.awt.im.spi\" \"src/java/awt/im/spi\")) (\"InputMethodEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"InputMethodHighlight\" (\"java.awt.im\" \"src/java/awt/im\")) (\"InputMethodListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"InputMethodRequests\" (\"java.awt.im\" \"src/java/awt/im\")) (\"InputSource\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"InputStream\" (\"java.io\" \"src/java/io\")) (\"InputStreamReader\" (\"java.io\" \"src/java/io\")) (\"InputSubset\" (\"java.awt.im\" \"src/java/awt/im\")) (\"InputVerifier\" (\"javax.swing\" \"src/javax/swing\")) (\"Insets\" (\"java.awt\" \"src/java/awt\")) (\"InsetsUIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"InstantiationError\" (\"java.lang\" \"src/java/lang\")) (\"InstantiationException\" (\"java.lang\" \"src/java/lang\")) (\"Instruction\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Instrument\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"InsufficientResourcesException\" (\"javax.naming\" \"src/javax/naming\")) (\"IntBuffer\" (\"java.nio\" \"src/java/nio\")) (\"IntExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"IntStack\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"IntStack\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"IntType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"IntVector\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"Integer\" (\"java.lang\" \"src/java/lang\")) (\"IntegerArray\" (\"org.apache.xalan.xsltc.util\" \"src/org/apache/xalan/xsltc/util\")) (\"IntegerDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"IntegerSyntax\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"IntegratedParserConfiguration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"InternalError\" (\"java.lang\" \"src/java/lang\")) (\"InternalFrameAdapter\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"InternalFrameEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"InternalFrameFocusTraversalPolicy\" (\"javax.swing\" \"src/javax/swing\")) (\"InternalFrameListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"InternalFrameUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"InternalSHA\" (\"java.io\" \"src/java/io\")) (\"InternationalFormatter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"InterruptedException\" (\"java.lang\" \"src/java/lang\")) (\"InterruptedIOException\" (\"java.io\" \"src/java/io\")) (\"InterruptedNamingException\" (\"javax.naming\" \"src/javax/naming\")) (\"InterruptibleChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"IntrospectionException\" (\"java.beans\" \"src/java/beans\")) (\"Introspector\" (\"java.beans\" \"src/java/beans\")) (\"InvalidAlgorithmParameterException\" (\"java.security\" \"src/java/security\")) (\"InvalidAttributeIdentifierException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"InvalidAttributeValueException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"InvalidAttributesException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"InvalidClassException\" (\"java.io\" \"src/java/io\")) (\"InvalidDatatypeFacetException\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"InvalidDatatypeValueException\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"InvalidDnDOperationException\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"InvalidKeyException\" (\"java.security\" \"src/java/security\")) (\"InvalidKeySpecException\" (\"java.security.spec\" \"src/java/security/spec\")) (\"InvalidMarkException\" (\"java.nio\" \"src/java/nio\")) (\"InvalidMidiDataException\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"InvalidNameException\" (\"javax.naming\" \"src/javax/naming\")) (\"InvalidObjectException\" (\"java.io\" \"src/java/io\")) (\"InvalidParameterException\" (\"java.security\" \"src/java/security\")) (\"InvalidParameterSpecException\" (\"java.security.spec\" \"src/java/security/spec\")) (\"InvalidPreferencesFormatException\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"InvalidSearchControlsException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"InvalidSearchFilterException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"InvalidTransactionException\" (\"javax.transaction\" \"src/javax/transaction\")) (\"InvocationEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"InvocationHandler\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"InvocationTargetException\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"IsindexView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"ItemEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ItemListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"ItemPSVI\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"ItemSelectable\" (\"java.awt\" \"src/java/awt\")) (\"Iterator\" (\"java.util\" \"src/java/util\")) (\"IteratorPool\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"JAXPConstants\" (\"org.apache.xerces.jaxp\" \"src/org/apache/xerces/jaxp\")) (\"JApplet\" (\"javax.swing\" \"src/javax/swing\")) (\"JButton\" (\"javax.swing\" \"src/javax/swing\")) (\"JCheckBox\" (\"javax.swing\" \"src/javax/swing\")) (\"JCheckBoxMenuItem\" (\"javax.swing\" \"src/javax/swing\")) (\"JColorChooser\" (\"javax.swing\" \"src/javax/swing\")) (\"JComboBox\" (\"javax.swing\" \"src/javax/swing\")) (\"JComponent\" (\"javax.swing\" \"src/javax/swing\")) (\"JDesktopPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JDialog\" (\"javax.swing\" \"src/javax/swing\")) (\"JEditorPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JFileChooser\" (\"javax.swing\" \"src/javax/swing\")) (\"JFormattedTextField\" (\"javax.swing\" \"src/javax/swing\")) (\"JFrame\" (\"javax.swing\" \"src/javax/swing\")) (\"JInternalFrame\" (\"javax.swing\" \"src/javax/swing\")) (\"JLabel\" (\"javax.swing\" \"src/javax/swing\")) (\"JLayeredPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JList\" (\"javax.swing\" \"src/javax/swing\")) (\"JMenu\" (\"javax.swing\" \"src/javax/swing\")) (\"JMenuBar\" (\"javax.swing\" \"src/javax/swing\")) (\"JMenuItem\" (\"javax.swing\" \"src/javax/swing\")) (\"JOptionPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JPEGHuffmanTable\" (\"javax.imageio.plugins.jpeg\" \"src/javax/imageio/plugins/jpeg\")) (\"JPEGImageReadParam\" (\"javax.imageio.plugins.jpeg\" \"src/javax/imageio/plugins/jpeg\")) (\"JPEGImageWriteParam\" (\"javax.imageio.plugins.jpeg\" \"src/javax/imageio/plugins/jpeg\")) (\"JPEGQTable\" (\"javax.imageio.plugins.jpeg\" \"src/javax/imageio/plugins/jpeg\")) (\"JPanel\" (\"javax.swing\" \"src/javax/swing\")) (\"JPasswordField\" (\"javax.swing\" \"src/javax/swing\")) (\"JPopupMenu\" (\"javax.swing\" \"src/javax/swing\")) (\"JProgressBar\" (\"javax.swing\" \"src/javax/swing\")) (\"JRadioButton\" (\"javax.swing\" \"src/javax/swing\")) (\"JRadioButtonMenuItem\" (\"javax.swing\" \"src/javax/swing\")) (\"JRootPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JScrollBar\" (\"javax.swing\" \"src/javax/swing\")) (\"JScrollPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JSeparator\" (\"javax.swing\" \"src/javax/swing\")) (\"JSlider\" (\"javax.swing\" \"src/javax/swing\")) (\"JSpinner\" (\"javax.swing\" \"src/javax/swing\")) (\"JSplitPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JTabbedPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JTable\" (\"javax.swing\" \"src/javax/swing\")) (\"JTableHeader\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"JTextArea\" (\"javax.swing\" \"src/javax/swing\")) (\"JTextComponent\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"JTextField\" (\"javax.swing\" \"src/javax/swing\")) (\"JTextPane\" (\"javax.swing\" \"src/javax/swing\")) (\"JToggleButton\" (\"javax.swing\" \"src/javax/swing\")) (\"JToolBar\" (\"javax.swing\" \"src/javax/swing\")) (\"JToolTip\" (\"javax.swing\" \"src/javax/swing\")) (\"JTree\" (\"javax.swing\" \"src/javax/swing\")) (\"JViewport\" (\"javax.swing\" \"src/javax/swing\")) (\"JWindow\" (\"javax.swing\" \"src/javax/swing\")) (\"JarEntry\" (\"java.util.jar\" \"src/java/util/jar\")) (\"JarException\" (\"java.util.jar\" \"src/java/util/jar\")) (\"JarFile\" (\"java.util.jar\" \"src/java/util/jar\")) (\"JarInputStream\" (\"java.util.jar\" \"src/java/util/jar\")) (\"JarOutputStream\" (\"java.util.jar\" \"src/java/util/jar\")) (\"JarURLConnection\" (\"java.net\" \"src/java/net\")) (\"JarVerifier\" (\"java.util.jar\" \"src/java/util/jar\")) (\"JavaCupRedirect\" (\"org.apache.xalan.xsltc.util\" \"src/org/apache/xalan/xsltc/util\")) (\"JavaUtilJarAccessImpl\" (\"java.util.jar\" \"src/java/util/jar\")) (\"JavaUtils\" (\"org.apache.xml.utils.synthetic\" \"src/org/apache/xml/utils/synthetic\")) (\"JobAttributes\" (\"java.awt\" \"src/java/awt\")) (\"JobHoldUntil\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobImpressions\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobImpressionsCompleted\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobImpressionsSupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobKOctets\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobKOctetsProcessed\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobKOctetsSupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobMediaSheets\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobMediaSheetsCompleted\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobMediaSheetsSupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobMessageFromOperator\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobName\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobOriginatingUserName\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobPriority\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobPrioritySupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobSheets\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobState\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobStateReason\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"JobStateReasons\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Kernel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Key\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Key\" (\"java.security\" \"src/java/security\")) (\"KeyAdapter\" (\"java.awt.event\" \"src/java/awt/event\")) (\"KeyCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"KeyDeclaration\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"KeyEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"KeyEventDispatcher\" (\"java.awt\" \"src/java/awt\")) (\"KeyEventPostProcessor\" (\"java.awt\" \"src/java/awt\")) (\"KeyException\" (\"java.security\" \"src/java/security\")) (\"KeyFactory\" (\"java.security\" \"src/java/security\")) (\"KeyFactorySpi\" (\"java.security\" \"src/java/security\")) (\"KeyIndex\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"KeyIterator\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"KeyListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"KeyManagementException\" (\"java.security\" \"src/java/security\")) (\"KeyManager\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"KeyPair\" (\"java.security\" \"src/java/security\")) (\"KeyPairGenerator\" (\"java.security\" \"src/java/security\")) (\"KeyPairGeneratorSpi\" (\"java.security\" \"src/java/security\")) (\"KeyPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"KeyRef\" (\"org.apache.xerces.impl.xs.identity\" \"src/org/apache/xerces/impl/xs/identity\")) (\"KeyRefIterator\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"KeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"KeyStore\" (\"java.security\" \"src/java/security\")) (\"KeyStoreException\" (\"java.security\" \"src/java/security\")) (\"KeyStoreSpi\" (\"java.security\" \"src/java/security\")) (\"KeyStroke\" (\"javax.swing\" \"src/javax/swing\")) (\"KeyTable\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"KeyboardFocusManager\" (\"java.awt\" \"src/java/awt\")) (\"KeyboardManager\" (\"javax.swing\" \"src/javax/swing\")) (\"Keymap\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Keywords\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"LCount\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"LSInputList\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"Label\" (\"java.awt\" \"src/java/awt\")) (\"LabelPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"LabelUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"LabelView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"LangCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"LastCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"LastOwnerException\" (\"java.security.acl\" \"src/java/security/acl\")) (\"LayeredHighlighter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"LayoutComparator\" (\"javax.swing\" \"src/javax/swing\")) (\"LayoutFocusTraversalPolicy\" (\"javax.swing\" \"src/javax/swing\")) (\"LayoutManager\" (\"java.awt\" \"src/java/awt\")) (\"LayoutManager2\" (\"java.awt\" \"src/java/awt\")) (\"LayoutQueue\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"LdapContext\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"LdapReferralException\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"Lease\" (\"java.rmi.dgc\" \"src/java/rmi/dgc\")) (\"LegacyGlueFocusTraversalPolicy\" (\"javax.swing\" \"src/javax/swing\")) (\"Level\" (\"java.util.logging\" \"src/java/util/logging\")) (\"Lexer\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"LexicalHandler\" (\"org.xml.sax.ext\" \"src/org/xml/sax/ext\")) (\"LightweightPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"LimitExceededException\" (\"javax.naming\" \"src/javax/naming\")) (\"Line\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"Line2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"LineBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"LineBreakData\" (\"java.text\" \"src/java/text\")) (\"LineBreakMeasurer\" (\"java.awt.font\" \"src/java/awt/font\")) (\"LineEvent\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"LineIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"LineListener\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"LineMetrics\" (\"java.awt.font\" \"src/java/awt/font\")) (\"LineNumberInputStream\" (\"java.io\" \"src/java/io\")) (\"LineNumberReader\" (\"java.io\" \"src/java/io\")) (\"LineSeparator\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"LineUnavailableException\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"LineView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"LinkException\" (\"javax.naming\" \"src/javax/naming\")) (\"LinkLoopException\" (\"javax.naming\" \"src/javax/naming\")) (\"LinkRef\" (\"javax.naming\" \"src/javax/naming\")) (\"LinkStyle\" (\"org.w3c.dom.stylesheets\" \"src/org/w3c/dom/stylesheets\")) (\"LinkageError\" (\"java.lang\" \"src/java/lang\")) (\"LinkedHashMap\" (\"java.util\" \"src/java/util\")) (\"LinkedHashSet\" (\"java.util\" \"src/java/util\")) (\"LinkedList\" (\"java.util\" \"src/java/util\")) (\"List\" (\"java.util\" \"src/java/util\")) (\"List\" (\"java.awt\" \"src/java/awt\")) (\"ListCellRenderer\" (\"javax.swing\" \"src/javax/swing\")) (\"ListDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"ListDataEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"ListDataListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"ListDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"ListIterator\" (\"java.util\" \"src/java/util\")) (\"ListModel\" (\"javax.swing\" \"src/javax/swing\")) (\"ListPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"ListResourceBundle\" (\"java.util\" \"src/java/util\")) (\"ListSelectionEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"ListSelectionListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"ListSelectionModel\" (\"javax.swing\" \"src/javax/swing\")) (\"ListUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ListView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"ListingErrorHandler\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"LiteralAttribute\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"LiteralElement\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"LiteralExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"LoadDocument\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"LoaderHandler\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"LocPathIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"LocalNameCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Locale\" (\"java.util\" \"src/java/util\")) (\"LocaleUtility\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"LocateRegistry\" (\"java.rmi.registry\" \"src/java/rmi/registry\")) (\"LocationPathPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Locator\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"LocatorImpl\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"LogManager\" (\"java.util.logging\" \"src/java/util/logging\")) (\"LogRecord\" (\"java.util.logging\" \"src/java/util/logging\")) (\"LogStream\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"Logger\" (\"java.util.logging\" \"src/java/util/logging\")) (\"LoggingPermission\" (\"java.util.logging\" \"src/java/util/logging\")) (\"LogicalExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Long\" (\"java.lang\" \"src/java/lang\")) (\"LongBuffer\" (\"java.nio\" \"src/java/nio\")) (\"LookAndFeel\" (\"javax.swing\" \"src/javax/swing\")) (\"LookupOp\" (\"java.awt.image\" \"src/java/awt/image\")) (\"LookupTable\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Lt\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Lte\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"MalformedInputException\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"MalformedLinkException\" (\"javax.naming\" \"src/javax/naming\")) (\"MalformedURLException\" (\"java.net\" \"src/java/net\")) (\"Manifest\" (\"java.util.jar\" \"src/java/util/jar\")) (\"Map\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"Map\" (\"java.util\" \"src/java/util\")) (\"MappedByteBuffer\" (\"java.nio\" \"src/java/nio\")) (\"MarshalException\" (\"java.rmi\" \"src/java/rmi\")) (\"MarshalledObject\" (\"java.rmi\" \"src/java/rmi\")) (\"MaskFormatter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Match\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"MatchGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"MatchPatternIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"Matcher\" (\"java.util.regex\" \"src/java/util/regex\")) (\"MatchingIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"Math\" (\"java.lang\" \"src/java/lang\")) (\"MatteBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"Media\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"MediaList\" (\"org.w3c.dom.stylesheets\" \"src/org/w3c/dom/stylesheets\")) (\"MediaName\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"MediaPrintableArea\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"MediaSize\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"MediaSizeName\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"MediaTracker\" (\"java.awt\" \"src/java/awt\")) (\"MediaTray\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Member\" (\"org.apache.xml.utils.synthetic.reflection\" \"src/org/apache/xml/utils/synthetic/reflection\")) (\"Member\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"MemoryCache\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"MemoryCacheImageInputStream\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"MemoryCacheImageOutputStream\" (\"javax.imageio.stream\" \"src/javax/imageio/stream\")) (\"MemoryHandler\" (\"java.util.logging\" \"src/java/util/logging\")) (\"MemoryImageSource\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Menu\" (\"java.awt\" \"src/java/awt\")) (\"MenuBar\" (\"java.awt\" \"src/java/awt\")) (\"MenuBarPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"MenuBarUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"MenuComponent\" (\"java.awt\" \"src/java/awt\")) (\"MenuComponentPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"MenuContainer\" (\"java.awt\" \"src/java/awt\")) (\"MenuDragMouseEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"MenuDragMouseListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"MenuElement\" (\"javax.swing\" \"src/javax/swing\")) (\"MenuEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"MenuItem\" (\"java.awt\" \"src/java/awt\")) (\"MenuItemPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"MenuItemUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"MenuKeyEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"MenuKeyListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"MenuListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"MenuPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"MenuSelectionManager\" (\"javax.swing\" \"src/javax/swing\")) (\"MenuShortcut\" (\"java.awt\" \"src/java/awt\")) (\"MergeCollation\" (\"java.text\" \"src/java/text\")) (\"Message\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"MessageDigest\" (\"java.security\" \"src/java/security\")) (\"MessageDigestSpi\" (\"java.security\" \"src/java/security\")) (\"MessageFormat\" (\"java.text\" \"src/java/text\")) (\"MessageFormatter\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"MessageHandler\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"MetaData\" (\"java.beans\" \"src/java/beans\")) (\"MetaEventListener\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MetaMessage\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MetalBorders\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalBumps\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalButtonUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalCheckBoxIcon\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalCheckBoxUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalComboBoxButton\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalComboBoxEditor\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalComboBoxIcon\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalComboBoxUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalDesktopIconUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalFileChooserUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalFontDesktopProperty\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalHighContrastTheme\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalIconFactory\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalInternalFrameTitlePane\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalInternalFrameUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalLabelUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalLookAndFeel\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalPopupMenuSeparatorUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalProgressBarUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalRadioButtonUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalRootPaneUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalScrollBarUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalScrollButton\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalScrollPaneUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalSeparatorUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalSliderUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalSplitPaneDivider\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalSplitPaneUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalTabbedPaneUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalTextFieldUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalTheme\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalTitlePane\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalToggleButtonUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalToolBarUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalToolTipUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalTreeUI\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"MetalUtils\" (\"javax.swing.plaf.metal\" \"src/javax/swing/plaf/metal\")) (\"Method\" (\"org.apache.xml.utils.synthetic.reflection\" \"src/org/apache/xml/utils/synthetic/reflection\")) (\"Method\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"Method\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"Method\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"MethodDescriptor\" (\"java.beans\" \"src/java/beans\")) (\"MethodGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"MethodResolver\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"MethodType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"MidiChannel\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MidiDevice\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MidiDeviceProvider\" (\"javax.sound.midi.spi\" \"src/javax/sound/midi/spi\")) (\"MidiEvent\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MidiFileFormat\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MidiFileReader\" (\"javax.sound.midi.spi\" \"src/javax/sound/midi/spi\")) (\"MidiFileWriter\" (\"javax.sound.midi.spi\" \"src/javax/sound/midi/spi\")) (\"MidiMessage\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MidiSystem\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MidiUnavailableException\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"MimeType\" (\"javax.print\" \"src/javax/print\")) (\"MimeType\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"MimeTypeParameterList\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"MimeTypeParseException\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"MinimalHTMLWriter\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"Minus\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"MissingOptArgException\" (\"org.apache.xalan.xsltc.cmdline.getopt\" \"src/org/apache/xalan/xsltc/cmdline/getopt\")) (\"MissingResourceException\" (\"java.util\" \"src/java/util\")) (\"MixedContentModel\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"Mixer\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"MixerProvider\" (\"javax.sound.sampled.spi\" \"src/javax/sound/sampled/spi\")) (\"MockAttributeSet\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"Mod\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Mode\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ModificationItem\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"Modifier\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"MonthDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"MonthDayDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"MouseAdapter\" (\"java.awt.event\" \"src/java/awt/event\")) (\"MouseDragGestureRecognizer\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"MouseEvent\" (\"org.w3c.dom.events\" \"src/org/w3c/dom/events\")) (\"MouseEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"MouseInputAdapter\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"MouseInputListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"MouseListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"MouseMotionAdapter\" (\"java.awt.event\" \"src/java/awt/event\")) (\"MouseMotionListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"MouseWheelEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"MouseWheelListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"MsgMgr\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"Mult\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"MultiButtonUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiColorChooserUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiComboBoxUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiDOM\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"MultiDesktopIconUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiDesktopPaneUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiDoc\" (\"javax.print\" \"src/javax/print\")) (\"MultiDocPrintJob\" (\"javax.print\" \"src/javax/print\")) (\"MultiDocPrintService\" (\"javax.print\" \"src/javax/print\")) (\"MultiFileChooserUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiHashtable\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"MultiInternalFrameUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiLabelUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiListUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiLookAndFeel\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiMenuBarUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiMenuItemUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiOptionPaneUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiPanelUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiPixelPackedSampleModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"MultiPopupMenuUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiProgressBarUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiRootPaneUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiScrollBarUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiScrollPaneUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiSeparatorUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiSliderUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiSpinnerUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiSplitPaneUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiTabbedPaneUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiTableHeaderUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiTableUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiTextUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiToolBarUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiToolTipUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiTreeUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MultiUIDefaults\" (\"javax.swing\" \"src/javax/swing\")) (\"MultiViewportUI\" (\"javax.swing.plaf.multi\" \"src/javax/swing/plaf/multi\")) (\"MulticastSocket\" (\"java.net\" \"src/java/net\")) (\"MultipleDocumentHandling\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"MultipleMaster\" (\"java.awt.font\" \"src/java/awt/font\")) (\"MultipleNodeCounter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"MultipleScopeNamespaceSupport\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"MutableAttrListImpl\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"MutableAttributeSet\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"MutableBigInteger\" (\"java.math\" \"src/java/math\")) (\"MutableComboBoxModel\" (\"javax.swing\" \"src/javax/swing\")) (\"MutableTreeNode\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"MutationEvent\" (\"org.w3c.dom.events\" \"src/org/w3c/dom/events\")) (\"MutationEventImpl\" (\"org.apache.xerces.dom.events\" \"src/org/apache/xerces/dom/events\")) (\"MuxingAttributeSet\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"NMTOKENDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"NOTATIONDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"NSInfo\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"NSItemListImpl\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"Name\" (\"javax.naming\" \"src/javax/naming\")) (\"NameAlreadyBoundException\" (\"javax.naming\" \"src/javax/naming\")) (\"NameBase\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"NameCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"NameClassPair\" (\"javax.naming\" \"src/javax/naming\")) (\"NameGenerator\" (\"java.beans\" \"src/java/beans\")) (\"NameImpl\" (\"javax.naming\" \"src/javax/naming\")) (\"NameNodeListImpl\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"NameNotFoundException\" (\"javax.naming\" \"src/javax/naming\")) (\"NameParser\" (\"javax.naming\" \"src/javax/naming\")) (\"NameSpace\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"NamedMethodGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"NamedNodeMap\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"NamedNodeMapImpl\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"NamedNodeMapImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"NamespaceAlias\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"NamespaceAlias\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"NamespaceChangeListener\" (\"javax.naming.event\" \"src/javax/naming/event\")) (\"NamespaceContext\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"NamespaceMappings\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"NamespaceSupport\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"NamespaceSupport\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"NamespaceSupport2\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"NamespaceUriCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Naming\" (\"java.rmi\" \"src/java/rmi\")) (\"NamingEnumeration\" (\"javax.naming\" \"src/javax/naming\")) (\"NamingEvent\" (\"javax.naming.event\" \"src/javax/naming/event\")) (\"NamingException\" (\"javax.naming\" \"src/javax/naming\")) (\"NamingExceptionEvent\" (\"javax.naming.event\" \"src/javax/naming/event\")) (\"NamingListener\" (\"javax.naming.event\" \"src/javax/naming/event\")) (\"NamingManager\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"NamingSecurityException\" (\"javax.naming\" \"src/javax/naming\")) (\"NavigationFilter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Neg\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"NegativeArraySizeException\" (\"java.lang\" \"src/java/lang\")) (\"NetPermission\" (\"java.net\" \"src/java/net\")) (\"NetworkInterface\" (\"java.net\" \"src/java/net\")) (\"NewInstance\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"NoClassDefFoundError\" (\"java.lang\" \"src/java/lang\")) (\"NoConnectionPendingException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"NoFramesView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"NoInitialContextException\" (\"javax.naming\" \"src/javax/naming\")) (\"NoPermissionException\" (\"javax.naming\" \"src/javax/naming\")) (\"NoRouteToHostException\" (\"java.net\" \"src/java/net\")) (\"NoSuchAlgorithmException\" (\"java.security\" \"src/java/security\")) (\"NoSuchAttributeException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"NoSuchElementException\" (\"java.util\" \"src/java/util\")) (\"NoSuchFieldError\" (\"java.lang\" \"src/java/lang\")) (\"NoSuchFieldException\" (\"java.lang\" \"src/java/lang\")) (\"NoSuchMethodError\" (\"java.lang\" \"src/java/lang\")) (\"NoSuchMethodException\" (\"java.lang\" \"src/java/lang\")) (\"NoSuchObjectException\" (\"java.rmi\" \"src/java/rmi\")) (\"NoSuchProviderException\" (\"java.security\" \"src/java/security\")) (\"Node\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"Node\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"NodeChangeEvent\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"NodeChangeListener\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"NodeConsumer\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"NodeCounter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"NodeCounterGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"NodeEditAS\" (\"org.apache.xerces.dom3.as\" \"src/org/apache/xerces/dom3/as\")) (\"NodeFilter\" (\"org.w3c.dom.traversal\" \"src/org/w3c/dom/traversal\")) (\"NodeImpl\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"NodeImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"NodeInfo\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"NodeIterator\" (\"org.w3c.dom.traversal\" \"src/org/w3c/dom/traversal\")) (\"NodeIterator\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"NodeIteratorBase\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"NodeIteratorImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"NodeList\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"NodeListCache\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"NodeLocator\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"NodeSequence\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"NodeSet\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"NodeSetDTM\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"NodeSetType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"NodeSortKey\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"NodeSortRecord\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"NodeSortRecordFactGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"NodeSortRecordFactory\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"NodeSortRecordGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"NodeSorter\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"NodeTest\" (\"org.apache.xpath.patterns\" \"src/org/apache/xpath/patterns\")) (\"NodeTest\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"NodeTestFilter\" (\"org.apache.xpath.patterns\" \"src/org/apache/xpath/patterns\")) (\"NodeType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"NodeVector\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"NonReadableChannelException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"NonValidatingConfiguration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"NonWritableChannelException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"NoninvertibleTransformException\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"NotActiveException\" (\"java.io\" \"src/java/io\")) (\"NotBoundException\" (\"java.rmi\" \"src/java/rmi\")) (\"NotCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"NotContextException\" (\"javax.naming\" \"src/javax/naming\")) (\"NotEquals\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"NotOwnerException\" (\"java.security.acl\" \"src/java/security/acl\")) (\"NotSerializableException\" (\"java.io\" \"src/java/io\")) (\"NotYetBoundException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"NotYetConnectedException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"Notation\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"NotationImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"NthIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"NullPointerException\" (\"java.lang\" \"src/java/lang\")) (\"Number\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Number\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Number\" (\"java.lang\" \"src/java/lang\")) (\"NumberCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"NumberFormat\" (\"java.text\" \"src/java/text\")) (\"NumberFormatException\" (\"java.lang\" \"src/java/lang\")) (\"NumberFormatter\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"NumberOfDocuments\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"NumberOfInterveningJobs\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"NumberType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"NumberUp\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"NumberUpSupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"NumeratorFormatter\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"NumericShaper\" (\"java.awt.font\" \"src/java/awt/font\")) (\"ObjID\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"Object\" (\"java.lang\" \"src/java/lang\")) (\"ObjectArray\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"ObjectChangeListener\" (\"javax.naming.event\" \"src/javax/naming/event\")) (\"ObjectFactory\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"ObjectFactory\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"ObjectFactory\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"ObjectFactory\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ObjectFactory\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"ObjectFactory\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"ObjectFactory\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"ObjectFactory\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"ObjectFactory\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"ObjectFactory\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"ObjectFactory\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ObjectFactory\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"ObjectFactory\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ObjectFactory\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"ObjectFactory\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ObjectFactory\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ObjectFactory\" (\"org.apache.xalan.xsltc.cmdline\" \"src/org/apache/xalan/xsltc/cmdline\")) (\"ObjectFactory\" (\"org.apache.xalan.xslt\" \"src/org/apache/xalan/xslt\")) (\"ObjectFactory\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"ObjectFactory\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"ObjectFactory\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"ObjectFactory\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"ObjectFactory\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"ObjectFactoryBuilder\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"ObjectInput\" (\"java.io\" \"src/java/io\")) (\"ObjectInputStream\" (\"java.io\" \"src/java/io\")) (\"ObjectInputValidation\" (\"java.io\" \"src/java/io\")) (\"ObjectOutput\" (\"java.io\" \"src/java/io\")) (\"ObjectOutputStream\" (\"java.io\" \"src/java/io\")) (\"ObjectPool\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"ObjectStack\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"ObjectStreamClass\" (\"java.io\" \"src/java/io\")) (\"ObjectStreamConstants\" (\"java.io\" \"src/java/io\")) (\"ObjectStreamException\" (\"java.io\" \"src/java/io\")) (\"ObjectStreamField\" (\"java.io\" \"src/java/io\")) (\"ObjectType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ObjectVector\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"ObjectView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"Observable\" (\"java.util\" \"src/java/util\")) (\"Observer\" (\"java.util\" \"src/java/util\")) (\"OneStepIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"OneStepIteratorForward\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"Op\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"OpCodes\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"OpMap\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"OpMapVector\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"OpenType\" (\"java.awt.font\" \"src/java/awt/font\")) (\"Operation\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Operation\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"OperationNotSupportedException\" (\"javax.naming\" \"src/javax/naming\")) (\"Operators\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"Option\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"OptionComboBoxModel\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"OptionListModel\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"OptionPaneUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"OptionalDataException\" (\"java.io\" \"src/java/io\")) (\"Or\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"OrientationRequested\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Otherwise\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"OutOfMemoryError\" (\"java.lang\" \"src/java/lang\")) (\"Output\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"OutputBuffer\" (\"org.apache.xalan.xsltc.runtime.output\" \"src/org/apache/xalan/xsltc/runtime/output\")) (\"OutputDeviceAssigned\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"OutputFormat\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"OutputKeys\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"OutputProperties\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"OutputPropertiesFactory\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"OutputPropertyUtils\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"OutputSettings\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"OutputStream\" (\"java.io\" \"src/java/io\")) (\"OutputStreamWriter\" (\"java.io\" \"src/java/io\")) (\"OverlappingFileLockException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"OverlayLayout\" (\"javax.swing\" \"src/javax/swing\")) (\"Owner\" (\"java.security.acl\" \"src/java/security/acl\")) (\"PDLOverrideSupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PKCS8EncodedKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"PSSParameterSpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"PSVIAttrNSImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"PSVIDOMImplementationImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"PSVIDocumentImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"PSVIElementNSImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"PSVIProvider\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"Package\" (\"java.lang\" \"src/java/lang\")) (\"PackedColorModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"PageAttributes\" (\"java.awt\" \"src/java/awt\")) (\"PageFormat\" (\"java.awt.print\" \"src/java/awt/print\")) (\"PageRanges\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Pageable\" (\"java.awt.print\" \"src/java/awt/print\")) (\"PagesPerMinute\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PagesPerMinuteColor\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Paint\" (\"java.awt\" \"src/java/awt\")) (\"PaintContext\" (\"java.awt\" \"src/java/awt\")) (\"PaintEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"Panel\" (\"java.awt\" \"src/java/awt\")) (\"PanelPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"PanelUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Paper\" (\"java.awt.print\" \"src/java/awt/print\")) (\"ParagraphView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"ParagraphView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Param\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Parameter\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"ParameterBlock\" (\"java.awt.image.renderable\" \"src/java/awt/image/renderable\")) (\"ParameterDescriptor\" (\"java.beans\" \"src/java/beans\")) (\"ParameterMetaData\" (\"java.sql\" \"src/java/sql\")) (\"ParameterRef\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ParentLocationPath\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ParentNode\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ParentPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ParseException\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"ParseException\" (\"java.text\" \"src/java/text\")) (\"ParsePosition\" (\"java.text\" \"src/java/text\")) (\"Parser\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"Parser\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Parser\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"ParserAdapter\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"ParserConfigurationException\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"ParserConfigurationSettings\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"ParserDelegator\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"ParserFactory\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"ParserForXMLSchema\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"PartialResultException\" (\"javax.naming\" \"src/javax/naming\")) (\"PartiallyOrderedSet\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"PasswordAuthentication\" (\"java.net\" \"src/java/net\")) (\"PasswordView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Patch\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"PathComponent\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"PathIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"Pattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Pattern\" (\"java.util.regex\" \"src/java/util/regex\")) (\"PatternEntry\" (\"java.text\" \"src/java/text\")) (\"PatternSyntaxException\" (\"java.util.regex\" \"src/java/util/regex\")) (\"Permission\" (\"java.security.acl\" \"src/java/security/acl\")) (\"Permission\" (\"java.security\" \"src/java/security\")) (\"PermissionCollection\" (\"java.security\" \"src/java/security\")) (\"Permissions\" (\"java.security\" \"src/java/security\")) (\"PersistenceDelegate\" (\"java.beans\" \"src/java/beans\")) (\"PhantomReference\" (\"java.lang.ref\" \"src/java/lang/ref\")) (\"Pipe\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"PipeDocument\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"PipedInputStream\" (\"java.io\" \"src/java/io\")) (\"PipedOutputStream\" (\"java.io\" \"src/java/io\")) (\"PipedReader\" (\"java.io\" \"src/java/io\")) (\"PipedWriter\" (\"java.io\" \"src/java/io\")) (\"PixelGrabber\" (\"java.awt.image\" \"src/java/awt/image\")) (\"PixelInterleavedSampleModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"PlainDatagramSocketImpl\" (\"java.net\" \"src/java/net\")) (\"PlainDocument\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"PlainSocketImpl\" (\"java.net\" \"src/java/net\")) (\"PlainView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Plus\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Point\" (\"java.awt\" \"src/java/awt\")) (\"Point2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"Policy\" (\"java.security\" \"src/java/security\")) (\"Polygon\" (\"java.awt\" \"src/java/awt\")) (\"PooledConnection\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"PooledConnection\" (\"javax.sql\" \"src/javax/sql\")) (\"Popup\" (\"javax.swing\" \"src/javax/swing\")) (\"PopupFactory\" (\"javax.swing\" \"src/javax/swing\")) (\"PopupMenu\" (\"java.awt\" \"src/java/awt\")) (\"PopupMenuEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"PopupMenuListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"PopupMenuPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"PopupMenuUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Port\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"PortUnreachableException\" (\"java.net\" \"src/java/net\")) (\"PortableRemoteObject\" (\"javax.rmi\" \"src/javax/rmi\")) (\"PortableRemoteObjectDelegate\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"Position\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"PositionCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Predicate\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"PredicatedNodeTest\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"PreferenceChangeEvent\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"PreferenceChangeListener\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"Preferences\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"PreferencesFactory\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"PrefixResolver\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"PrefixResolverDefault\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"PreparedStatement\" (\"java.sql\" \"src/java/sql\")) (\"PresentationDirection\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Principal\" (\"java.security\" \"src/java/security\")) (\"PrintEvent\" (\"javax.print.event\" \"src/javax/print/event\")) (\"PrintException\" (\"javax.print\" \"src/javax/print\")) (\"PrintGraphics\" (\"java.awt\" \"src/java/awt\")) (\"PrintJob\" (\"java.awt\" \"src/java/awt\")) (\"PrintJobAdapter\" (\"javax.print.event\" \"src/javax/print/event\")) (\"PrintJobAttribute\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"PrintJobAttributeEvent\" (\"javax.print.event\" \"src/javax/print/event\")) (\"PrintJobAttributeListener\" (\"javax.print.event\" \"src/javax/print/event\")) (\"PrintJobAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"PrintJobEvent\" (\"javax.print.event\" \"src/javax/print/event\")) (\"PrintJobListener\" (\"javax.print.event\" \"src/javax/print/event\")) (\"PrintQuality\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrintRequestAttribute\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"PrintRequestAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"PrintService\" (\"javax.print\" \"src/javax/print\")) (\"PrintServiceAttribute\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"PrintServiceAttributeEvent\" (\"javax.print.event\" \"src/javax/print/event\")) (\"PrintServiceAttributeListener\" (\"javax.print.event\" \"src/javax/print/event\")) (\"PrintServiceAttributeSet\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"PrintServiceLookup\" (\"javax.print\" \"src/javax/print\")) (\"PrintStream\" (\"java.io\" \"src/java/io\")) (\"PrintTraceListener\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"PrintWriter\" (\"java.io\" \"src/java/io\")) (\"Printable\" (\"java.awt.print\" \"src/java/awt/print\")) (\"Printer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"PrinterAbortException\" (\"java.awt.print\" \"src/java/awt/print\")) (\"PrinterException\" (\"java.awt.print\" \"src/java/awt/print\")) (\"PrinterGraphics\" (\"java.awt.print\" \"src/java/awt/print\")) (\"PrinterIOException\" (\"java.awt.print\" \"src/java/awt/print\")) (\"PrinterInfo\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterIsAcceptingJobs\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterJob\" (\"java.awt.print\" \"src/java/awt/print\")) (\"PrinterLocation\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterMakeAndModel\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterMessageFromOperator\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterMoreInfo\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterMoreInfoManufacturer\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterName\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterResolution\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterState\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterStateReason\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterStateReasons\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrinterURI\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"PrivateKey\" (\"java.security\" \"src/java/security\")) (\"PrivilegedAction\" (\"java.security\" \"src/java/security\")) (\"PrivilegedActionException\" (\"java.security\" \"src/java/security\")) (\"PrivilegedExceptionAction\" (\"java.security\" \"src/java/security\")) (\"Process\" (\"org.apache.xalan.xslt\" \"src/org/apache/xalan/xslt\")) (\"Process\" (\"java.lang\" \"src/java/lang\")) (\"ProcessingInstruction\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"ProcessingInstruction\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ProcessingInstructionImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"ProcessingInstructionPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ProcessorAttributeSet\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorCharacters\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorDecimalFormat\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorExsltFuncResult\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorExsltFunction\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorGlobalParamDecl\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorGlobalVariableDecl\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorImport\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorInclude\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorKey\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorLRE\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorNamespaceAlias\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorOutputElem\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorPreserveSpace\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorStripSpace\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorStylesheetDoc\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorStylesheetElement\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorTemplate\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorTemplateElem\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorText\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorUnknown\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"ProcessorVersion\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"ProfileDataException\" (\"java.awt.color\" \"src/java/awt/color\")) (\"ProgressBarUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ProgressMonitor\" (\"javax.swing\" \"src/javax/swing\")) (\"ProgressMonitorInputStream\" (\"javax.swing\" \"src/javax/swing\")) (\"Properties\" (\"java.util\" \"src/java/util\")) (\"PropertyChangeEvent\" (\"java.beans\" \"src/java/beans\")) (\"PropertyChangeListener\" (\"java.beans\" \"src/java/beans\")) (\"PropertyChangeListenerProxy\" (\"java.beans\" \"src/java/beans\")) (\"PropertyChangeSupport\" (\"java.beans\" \"src/java/beans\")) (\"PropertyDescriptor\" (\"java.beans\" \"src/java/beans\")) (\"PropertyEditor\" (\"java.beans\" \"src/java/beans\")) (\"PropertyEditorManager\" (\"java.beans\" \"src/java/beans\")) (\"PropertyEditorSupport\" (\"java.beans\" \"src/java/beans\")) (\"PropertyPermission\" (\"java.util\" \"src/java/util\")) (\"PropertyResourceBundle\" (\"java.util\" \"src/java/util\")) (\"PropertyVetoException\" (\"java.beans\" \"src/java/beans\")) (\"ProtectionDomain\" (\"java.security\" \"src/java/security\")) (\"ProtocolException\" (\"java.net\" \"src/java/net\")) (\"Provider\" (\"java.security\" \"src/java/security\")) (\"ProviderException\" (\"java.security\" \"src/java/security\")) (\"Proxy\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"PsuedoNames\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"PublicKey\" (\"java.security\" \"src/java/security\")) (\"PushbackInputStream\" (\"java.io\" \"src/java/io\")) (\"PushbackReader\" (\"java.io\" \"src/java/io\")) (\"QName\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"QName\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"QName\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"QNameDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"QuadCurve2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"QuadIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"QueryParameter\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"QueuedEvents\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"QueuedJobCount\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Quo\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"RBCollationTables\" (\"java.text\" \"src/java/text\")) (\"RBTableBuilder\" (\"java.text\" \"src/java/text\")) (\"REUtil\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"RGBColor\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"RGBImageFilter\" (\"java.awt.image\" \"src/java/awt/image\")) (\"RMIClassLoader\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RMIClassLoaderSpi\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RMIClientSocketFactory\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RMIFailureHandler\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RMISecurityException\" (\"java.rmi\" \"src/java/rmi\")) (\"RMISecurityManager\" (\"java.rmi\" \"src/java/rmi\")) (\"RMIServerSocketFactory\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RMISocketFactory\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RSAKey\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"RSAKeyGenParameterSpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"RSAMultiPrimePrivateCrtKey\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"RSAMultiPrimePrivateCrtKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"RSAOtherPrimeInfo\" (\"java.security.spec\" \"src/java/security/spec\")) (\"RSAPrivateCrtKey\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"RSAPrivateCrtKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"RSAPrivateKey\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"RSAPrivateKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"RSAPublicKey\" (\"java.security.interfaces\" \"src/java/security/interfaces\")) (\"RSAPublicKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"RTFAttribute\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"RTFAttributes\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"RTFEditorKit\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"RTFGenerator\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"RTFIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"RTFParser\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"RTFReader\" (\"javax.swing.text.rtf\" \"src/javax/swing/text/rtf\")) (\"Random\" (\"java.util\" \"src/java/util\")) (\"RandomAccess\" (\"java.util\" \"src/java/util\")) (\"RandomAccessFile\" (\"java.io\" \"src/java/io\")) (\"Range\" (\"org.w3c.dom.ranges\" \"src/org/w3c/dom/ranges\")) (\"RangeException\" (\"org.w3c.dom.ranges\" \"src/org/w3c/dom/ranges\")) (\"RangeExceptionImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"RangeImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"RangeToken\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"Raster\" (\"java.awt.image\" \"src/java/awt/image\")) (\"RasterFormatException\" (\"java.awt.image\" \"src/java/awt/image\")) (\"RasterOp\" (\"java.awt.image\" \"src/java/awt/image\")) (\"RawCharacterHandler\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"ReadOnlyBufferException\" (\"java.nio\" \"src/java/nio\")) (\"ReadableByteChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"Reader\" (\"java.io\" \"src/java/io\")) (\"RealExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"RealType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"Receiver\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"Rect\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"RectIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"Rectangle\" (\"java.awt\" \"src/java/awt\")) (\"Rectangle2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"RectangularShape\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"Redirect\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"RedundentExprEliminator\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"Ref\" (\"java.sql\" \"src/java/sql\")) (\"RefAddr\" (\"javax.naming\" \"src/javax/naming\")) (\"Reference\" (\"javax.naming\" \"src/javax/naming\")) (\"Reference\" (\"java.lang.ref\" \"src/java/lang/ref\")) (\"ReferenceQueue\" (\"java.lang.ref\" \"src/java/lang/ref\")) (\"ReferenceType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"ReferenceUriSchemesSupported\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Referenceable\" (\"javax.naming\" \"src/javax/naming\")) (\"ReferralException\" (\"javax.naming\" \"src/javax/naming\")) (\"ReflectAccess\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"ReflectPermission\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"RegexParser\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"RegisterableService\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"Registry\" (\"java.rmi.registry\" \"src/java/rmi/registry\")) (\"RegistryHandler\" (\"java.rmi.registry\" \"src/java/rmi/registry\")) (\"RegularExpression\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"RelationalExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"RelativeLocationPath\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"RelativePathPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Remote\" (\"java.rmi\" \"src/java/rmi\")) (\"RemoteCall\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RemoteException\" (\"java.rmi\" \"src/java/rmi\")) (\"RemoteObject\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RemoteRef\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RemoteServer\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RemoteStub\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"RenderContext\" (\"java.awt.image.renderable\" \"src/java/awt/image/renderable\")) (\"RenderableImage\" (\"java.awt.image.renderable\" \"src/java/awt/image/renderable\")) (\"RenderableImageOp\" (\"java.awt.image.renderable\" \"src/java/awt/image/renderable\")) (\"RenderableImageProducer\" (\"java.awt.image.renderable\" \"src/java/awt/image/renderable\")) (\"RenderedImage\" (\"java.awt.image\" \"src/java/awt/image\")) (\"RenderedImageFactory\" (\"java.awt.image.renderable\" \"src/java/awt/image/renderable\")) (\"Renderer\" (\"javax.swing\" \"src/javax/swing\")) (\"RenderingHints\" (\"java.awt\" \"src/java/awt\")) (\"RepaintManager\" (\"javax.swing\" \"src/javax/swing\")) (\"ReplicateScaleFilter\" (\"java.awt.image\" \"src/java/awt/image\")) (\"RequestingUserName\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"RescaleOp\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ResolutionSyntax\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"ResolveResult\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"Resolver\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"ResourceBundle\" (\"java.util\" \"src/java/util\")) (\"ResourceBundleEnumeration\" (\"java.util\" \"src/java/util\")) (\"ResourceLoader\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"ResourceLoader\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"Result\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"ResultNameSpace\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"ResultSet\" (\"java.sql\" \"src/java/sql\")) (\"ResultSetMetaData\" (\"java.sql\" \"src/java/sql\")) (\"ResultTreeType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"RevalidationHandler\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"ReverbType\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"ReverseAxesWalker\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"Robot\" (\"java.awt\" \"src/java/awt\")) (\"RobotPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"RootPaneContainer\" (\"javax.swing\" \"src/javax/swing\")) (\"RootPaneUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"RoundCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"RoundRectIterator\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"RoundRectangle2D\" (\"java.awt.geom\" \"src/java/awt/geom\")) (\"RowMapper\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"RowSet\" (\"javax.sql\" \"src/javax/sql\")) (\"RowSetEvent\" (\"javax.sql\" \"src/javax/sql\")) (\"RowSetInternal\" (\"javax.sql\" \"src/javax/sql\")) (\"RowSetListener\" (\"javax.sql\" \"src/javax/sql\")) (\"RowSetMetaData\" (\"javax.sql\" \"src/javax/sql\")) (\"RowSetReader\" (\"javax.sql\" \"src/javax/sql\")) (\"RowSetWriter\" (\"javax.sql\" \"src/javax/sql\")) (\"RtMethodGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"RuleBasedBreakIterator\" (\"java.text\" \"src/java/text\")) (\"RuleBasedCollator\" (\"java.text\" \"src/java/text\")) (\"Runnable\" (\"java.lang\" \"src/java/lang\")) (\"Runtime\" (\"java.lang\" \"src/java/lang\")) (\"RuntimeException\" (\"java.lang\" \"src/java/lang\")) (\"RuntimePermission\" (\"java.lang\" \"src/java/lang\")) (\"SAX2DOM\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"SAX2DTM\" (\"org.apache.xml.dtm.ref.sax2dtm\" \"src/org/apache/xml/dtm/ref/sax2dtm\")) (\"SAX2DTM2\" (\"org.apache.xml.dtm.ref.sax2dtm\" \"src/org/apache/xml/dtm/ref/sax2dtm\")) (\"SAX2RTFDTM\" (\"org.apache.xml.dtm.ref.sax2dtm\" \"src/org/apache/xml/dtm/ref/sax2dtm\")) (\"SAXException\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"SAXImpl\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"SAXMessageFormatter\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"SAXNotRecognizedException\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"SAXNotSupportedException\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"SAXParseException\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"SAXParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"SAXParser\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"SAXParserFactory\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"SAXParserFactoryImpl\" (\"org.apache.xerces.jaxp\" \"src/org/apache/xerces/jaxp\")) (\"SAXParserImpl\" (\"org.apache.xerces.jaxp\" \"src/org/apache/xerces/jaxp\")) (\"SAXResult\" (\"javax.xml.transform.sax\" \"src/javax/xml/transform/sax\")) (\"SAXSource\" (\"javax.xml.transform.sax\" \"src/javax/xml/transform/sax\")) (\"SAXSourceLocator\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"SAXTransformerFactory\" (\"javax.xml.transform.sax\" \"src/javax/xml/transform/sax\")) (\"SQLData\" (\"java.sql\" \"src/java/sql\")) (\"SQLDocument\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"SQLErrorDocument\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"SQLException\" (\"java.sql\" \"src/java/sql\")) (\"SQLInput\" (\"java.sql\" \"src/java/sql\")) (\"SQLOutput\" (\"java.sql\" \"src/java/sql\")) (\"SQLPermission\" (\"java.sql\" \"src/java/sql\")) (\"SQLQueryParser\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"SQLWarning\" (\"java.sql\" \"src/java/sql\")) (\"SampleModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Savepoint\" (\"java.sql\" \"src/java/sql\")) (\"ScatteringByteChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"SchemaDOM\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"SchemaDOMParser\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"SchemaDVFactory\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"SchemaDVFactoryImpl\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"SchemaDateTimeException\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"SchemaGrammar\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"SchemaNamespaceSupport\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"SchemaParsingConfig\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"SchemaSymbols\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"SchemaViolationException\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"ScrollBarUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ScrollPane\" (\"java.awt\" \"src/java/awt\")) (\"ScrollPaneAdjustable\" (\"java.awt\" \"src/java/awt\")) (\"ScrollPaneConstants\" (\"javax.swing\" \"src/javax/swing\")) (\"ScrollPaneLayout\" (\"javax.swing\" \"src/javax/swing\")) (\"ScrollPanePeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"ScrollPaneUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Scrollable\" (\"javax.swing\" \"src/javax/swing\")) (\"Scrollbar\" (\"java.awt\" \"src/java/awt\")) (\"ScrollbarPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"SearchControls\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"SearchResult\" (\"javax.naming.directory\" \"src/javax/naming/directory\")) (\"SecureClassLoader\" (\"java.security\" \"src/java/security\")) (\"SecureRandom\" (\"java.security\" \"src/java/security\")) (\"SecureRandomSpi\" (\"java.security\" \"src/java/security\")) (\"Security\" (\"java.security\" \"src/java/security\")) (\"SecurityConfiguration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"SecurityException\" (\"java.lang\" \"src/java/lang\")) (\"SecurityManager\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"SecurityManager\" (\"java.lang\" \"src/java/lang\")) (\"SecurityPermission\" (\"java.security\" \"src/java/security\")) (\"SecuritySupport\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"SecuritySupport\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"SecuritySupport\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"SecuritySupport\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"SecuritySupport\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"SecuritySupport\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"SecuritySupport\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"SecuritySupport\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"SecuritySupport\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"SecuritySupport\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"SecuritySupport\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"SecuritySupport\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"SecuritySupport\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"SecuritySupport\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"SecuritySupport\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"SecuritySupport\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"SecuritySupport\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"SecuritySupport\" (\"org.apache.xalan.xsltc.cmdline\" \"src/org/apache/xalan/xsltc/cmdline\")) (\"SecuritySupport\" (\"org.apache.xalan.xslt\" \"src/org/apache/xalan/xslt\")) (\"SecuritySupport\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"SecuritySupport\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"SecuritySupport\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"SecuritySupport\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"SecuritySupport\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"SecuritySupport\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"SecuritySupport12\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"SecuritySupport12\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"SecuritySupport12\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"SecuritySupport12\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"SecuritySupport12\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"SecuritySupport12\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"SecuritySupport12\" (\"org.apache.xml.dtm.ref\" \"src/org/apache/xml/dtm/ref\")) (\"SecuritySupport12\" (\"org.apache.xml.dtm\" \"src/org/apache/xml/dtm\")) (\"SecuritySupport12\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"SecuritySupport12\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"SecuritySupport12\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"SecuritySupport12\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"SecuritySupport12\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"SecuritySupport12\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"SecuritySupport12\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"SecuritySupport12\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"SecuritySupport12\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"SecuritySupport12\" (\"org.apache.xalan.xsltc.cmdline\" \"src/org/apache/xalan/xsltc/cmdline\")) (\"SecuritySupport12\" (\"org.apache.xalan.xslt\" \"src/org/apache/xalan/xslt\")) (\"SecuritySupport12\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"SecuritySupport12\" (\"org.apache.xalan.lib\" \"src/org/apache/xalan/lib\")) (\"SecuritySupport12\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"SecuritySupport12\" (\"org.apache.html.dom\" \"src/org/apache/html/dom\")) (\"SecuritySupport12\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"SecuritySupport12\" (\"javax.xml.parsers\" \"src/javax/xml/parsers\")) (\"Segment\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"SegmentCache\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"SelectableChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"SelectionEvent\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"SelectionKey\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"Selector\" (\"org.apache.xerces.impl.xs.identity\" \"src/org/apache/xerces/impl/xs/identity\")) (\"Selector\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"SelectorProvider\" (\"java.nio.channels.spi\" \"src/java/nio/channels/spi\")) (\"SelfIteratorNoPredicate\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"SentEvent\" (\"java.awt\" \"src/java/awt\")) (\"SentenceBreakData\" (\"java.text\" \"src/java/text\")) (\"SeparatorUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Sequence\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"SequenceInputStream\" (\"java.io\" \"src/java/io\")) (\"SequencedEvent\" (\"java.awt\" \"src/java/awt\")) (\"Sequencer\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"Serializable\" (\"java.io\" \"src/java/io\")) (\"SerializableLocatorImpl\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"SerializablePermission\" (\"java.io\" \"src/java/io\")) (\"SerializationHandler\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"SerializationTester\" (\"java.awt.dnd\" \"src/java/awt/dnd\")) (\"Serializer\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"Serializer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"Serializer\" (\"org.apache.xalan.serialize\" \"src/org/apache/xalan/serialize\")) (\"SerializerBase\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"SerializerConstants\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"SerializerFactory\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"SerializerFactory\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"SerializerFactory\" (\"org.apache.xalan.serialize\" \"src/org/apache/xalan/serialize\")) (\"SerializerFactoryImpl\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"SerializerSwitcher\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"SerializerTrace\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"SerializerTraceWriter\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"SerializerUtils\" (\"org.apache.xalan.serialize\" \"src/org/apache/xalan/serialize\")) (\"ServerCloneException\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"ServerError\" (\"java.rmi\" \"src/java/rmi\")) (\"ServerException\" (\"java.rmi\" \"src/java/rmi\")) (\"ServerNotActiveException\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"ServerRef\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"ServerRuntimeException\" (\"java.rmi\" \"src/java/rmi\")) (\"ServerSocket\" (\"java.net\" \"src/java/net\")) (\"ServerSocketChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"ServiceRegistry\" (\"javax.imageio.spi\" \"src/javax/imageio/spi\")) (\"ServiceUI\" (\"javax.print\" \"src/javax/print\")) (\"ServiceUIFactory\" (\"javax.print\" \"src/javax/print\")) (\"ServiceUnavailableException\" (\"javax.naming\" \"src/javax/naming\")) (\"Set\" (\"java.util\" \"src/java/util\")) (\"SetOfIntegerSyntax\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"Severity\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"ShadowedSymbolTable\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"Shape\" (\"java.awt\" \"src/java/awt\")) (\"ShapeGraphicAttribute\" (\"java.awt.font\" \"src/java/awt/font\")) (\"SheetCollate\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Short\" (\"java.lang\" \"src/java/lang\")) (\"ShortBuffer\" (\"java.nio\" \"src/java/nio\")) (\"ShortList\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"ShortListImpl\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"ShortLookupTable\" (\"java.awt.image\" \"src/java/awt/image\")) (\"ShortMessage\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"Shutdown\" (\"java.lang\" \"src/java/lang\")) (\"Sides\" (\"javax.print.attribute.standard\" \"src/javax/print/attribute/standard\")) (\"Signature\" (\"java.security\" \"src/java/security\")) (\"SignatureException\" (\"java.security\" \"src/java/security\")) (\"SignatureSpi\" (\"java.security\" \"src/java/security\")) (\"SignedMutableBigInteger\" (\"java.math\" \"src/java/math\")) (\"SignedObject\" (\"java.security\" \"src/java/security\")) (\"Signer\" (\"java.security\" \"src/java/security\")) (\"SimpleAttributeSet\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"SimpleAttributeValue\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"SimpleBeanInfo\" (\"java.beans\" \"src/java/beans\")) (\"SimpleContentModel\" (\"org.apache.xerces.impl.dtd.models\" \"src/org/apache/xerces/impl/dtd/models\")) (\"SimpleDateFormat\" (\"java.text\" \"src/java/text\")) (\"SimpleDoc\" (\"javax.print\" \"src/javax/print\")) (\"SimpleFormatter\" (\"java.util.logging\" \"src/java/util/logging\")) (\"SimpleLocator\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"SimpleResultTreeImpl\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"SimpleTextBoundary\" (\"java.text\" \"src/java/text\")) (\"SimpleTimeZone\" (\"java.util\" \"src/java/util\")) (\"SingleNodeCounter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"SinglePixelPackedSampleModel\" (\"java.awt.image\" \"src/java/awt/image\")) (\"SingleSelectionModel\" (\"javax.swing\" \"src/javax/swing\")) (\"SingletonIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"Size2DSyntax\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"SizeLimitExceededException\" (\"javax.naming\" \"src/javax/naming\")) (\"SizeRequirements\" (\"javax.swing\" \"src/javax/swing\")) (\"SizeSequence\" (\"javax.swing\" \"src/javax/swing\")) (\"Skeleton\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"SkeletonMismatchException\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"SkeletonNotFoundException\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"SliderUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"SlotAllocator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"SmartGridLayout\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"SmartTransformerFactoryImpl\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"Socket\" (\"java.net\" \"src/java/net\")) (\"SocketAddress\" (\"java.net\" \"src/java/net\")) (\"SocketChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"SocketException\" (\"java.net\" \"src/java/net\")) (\"SocketHandler\" (\"java.util.logging\" \"src/java/util/logging\")) (\"SocketImpl\" (\"java.net\" \"src/java/net\")) (\"SocketImplFactory\" (\"java.net\" \"src/java/net\")) (\"SocketInputStream\" (\"java.net\" \"src/java/net\")) (\"SocketOptions\" (\"java.net\" \"src/java/net\")) (\"SocketOutputStream\" (\"java.net\" \"src/java/net\")) (\"SocketPermission\" (\"java.net\" \"src/java/net\")) (\"SocketSecurityException\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"SocketTimeoutException\" (\"java.net\" \"src/java/net\")) (\"SocksConsts\" (\"java.net\" \"src/java/net\")) (\"SocksSocketImpl\" (\"java.net\" \"src/java/net\")) (\"SocksSocketImplFactory\" (\"java.net\" \"src/java/net\")) (\"SoftBevelBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"SoftReference\" (\"java.lang.ref\" \"src/java/lang/ref\")) (\"Sort\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"SortSettings\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"SortedMap\" (\"java.util\" \"src/java/util\")) (\"SortedSet\" (\"java.util\" \"src/java/util\")) (\"SortingFocusTraversalPolicy\" (\"javax.swing\" \"src/javax/swing\")) (\"SortingIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"Soundbank\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"SoundbankReader\" (\"javax.sound.midi.spi\" \"src/javax/sound/midi/spi\")) (\"SoundbankResource\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"Source\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"SourceDataLine\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"SourceLoader\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"SourceLocator\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"SourceTree\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"SourceTreeManager\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"SpecialMapping\" (\"java.text\" \"src/java/text\")) (\"SpinnerDateModel\" (\"javax.swing\" \"src/javax/swing\")) (\"SpinnerListModel\" (\"javax.swing\" \"src/javax/swing\")) (\"SpinnerModel\" (\"javax.swing\" \"src/javax/swing\")) (\"SpinnerNumberModel\" (\"javax.swing\" \"src/javax/swing\")) (\"SpinnerUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"SplitPaneUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Spring\" (\"javax.swing\" \"src/javax/swing\")) (\"SpringLayout\" (\"javax.swing\" \"src/javax/swing\")) (\"Stack\" (\"java.util\" \"src/java/util\")) (\"StackGuard\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"StackOverflowError\" (\"java.lang\" \"src/java/lang\")) (\"StackTraceElement\" (\"java.lang\" \"src/java/lang\")) (\"StandardParserConfiguration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"StartTlsRequest\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"StartTlsResponse\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"StartsWithCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"StateEdit\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"StateEditable\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"StateFactory\" (\"javax.naming.spi\" \"src/javax/naming/spi\")) (\"StateInvariantError\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Statement\" (\"java.sql\" \"src/java/sql\")) (\"Statement\" (\"java.beans\" \"src/java/beans\")) (\"Step\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"StepIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"StepPattern\" (\"org.apache.xpath.patterns\" \"src/org/apache/xpath/patterns\")) (\"StepPattern\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"StopParseException\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"StreamCorruptedException\" (\"java.io\" \"src/java/io\")) (\"StreamHandler\" (\"java.util.logging\" \"src/java/util/logging\")) (\"StreamPrintService\" (\"javax.print\" \"src/javax/print\")) (\"StreamPrintServiceFactory\" (\"javax.print\" \"src/javax/print\")) (\"StreamResult\" (\"javax.xml.transform.stream\" \"src/javax/xml/transform/stream\")) (\"StreamSource\" (\"javax.xml.transform.stream\" \"src/javax/xml/transform/stream\")) (\"StreamTokenizer\" (\"java.io\" \"src/java/io\")) (\"StrictMath\" (\"java.lang\" \"src/java/lang\")) (\"String\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"String\" (\"java.lang\" \"src/java/lang\")) (\"StringBuffer\" (\"java.lang\" \"src/java/lang\")) (\"StringBufferInputStream\" (\"java.io\" \"src/java/io\")) (\"StringBufferPool\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"StringCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"StringCharBuffer\" (\"java.nio\" \"src/java/nio\")) (\"StringCharacterIterator\" (\"java.text\" \"src/java/text\")) (\"StringCoding\" (\"java.lang\" \"src/java/lang\")) (\"StringComparable\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"StringContent\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"StringDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"StringDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"StringIndexOutOfBoundsException\" (\"java.lang\" \"src/java/lang\")) (\"StringLengthCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"StringList\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"StringListImpl\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"StringOutputBuffer\" (\"org.apache.xalan.xsltc.runtime.output\" \"src/org/apache/xalan/xsltc/runtime/output\")) (\"StringReader\" (\"java.io\" \"src/java/io\")) (\"StringRefAddr\" (\"javax.naming\" \"src/javax/naming\")) (\"StringSelection\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"StringStack\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"StringToIntTable\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"StringToStringTable\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"StringToStringTableVector\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"StringTokenizer\" (\"java.util\" \"src/java/util\")) (\"StringType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"StringValueHandler\" (\"org.apache.xalan.xsltc.runtime\" \"src/org/apache/xalan/xsltc/runtime\")) (\"StringVector\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"StringWriter\" (\"java.io\" \"src/java/io\")) (\"StripFilter\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"StripWhitespaceFilter\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"Stroke\" (\"java.awt\" \"src/java/awt\")) (\"Struct\" (\"java.sql\" \"src/java/sql\")) (\"Stub\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"StubDelegate\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"StubNotFoundException\" (\"java.rmi\" \"src/java/rmi\")) (\"Style\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"StyleConstants\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"StyleContext\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"StyleSheet\" (\"org.w3c.dom.stylesheets\" \"src/org/w3c/dom/stylesheets\")) (\"StyleSheet\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"StyleSheetList\" (\"org.w3c.dom.stylesheets\" \"src/org/w3c/dom/stylesheets\")) (\"StyledDocument\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"StyledEditorKit\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"StyledParagraph\" (\"java.awt.font\" \"src/java/awt/font\")) (\"Stylesheet\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Stylesheet\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"StylesheetComposed\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"StylesheetHandler\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"StylesheetPIHandler\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"StylesheetRoot\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"StylesheetRootProxy\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"SubContextList\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"SuballocatedByteVector\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"SuballocatedIntVector\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"SubstitutionGroupHandler\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"SupportedValuesAttribute\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"SwingConstants\" (\"javax.swing\" \"src/javax/swing\")) (\"SwingGraphics\" (\"javax.swing\" \"src/javax/swing\")) (\"SwingPropertyChangeSupport\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"SwingUtilities\" (\"javax.swing\" \"src/javax/swing\")) (\"SymbolHash\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"SymbolTable\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"SymbolTable\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"SyncFailedException\" (\"java.io\" \"src/java/io\")) (\"SynchronizedSymbolTable\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"SyntaxTreeNode\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"SynthesisException\" (\"org.apache.xml.utils.synthetic\" \"src/org/apache/xml/utils/synthetic\")) (\"Synthesizer\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"SyntheticImage\" (\"javax.swing.colorchooser\" \"src/javax/swing/colorchooser\")) (\"SysexMessage\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"System\" (\"java.lang\" \"src/java/lang\")) (\"SystemColor\" (\"java.awt\" \"src/java/awt\")) (\"SystemEventQueueUtilities\" (\"javax.swing\" \"src/javax/swing\")) (\"SystemFlavorMap\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"SystemIDResolver\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"TabExpander\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"TabSet\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"TabStop\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"TabableView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"TabbedPaneUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"TableCellEditor\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"TableCellRenderer\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"TableColumn\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"TableColumnModel\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"TableColumnModelEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TableColumnModelListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TableHeaderUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"TableModel\" (\"javax.swing.table\" \"src/javax/swing/table\")) (\"TableModelEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TableModelListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TableUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"TableView\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"TableView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"TagElement\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"TagStack\" (\"javax.swing.text.html.parser\" \"src/javax/swing/text/html/parser\")) (\"TargetDataLine\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"Template\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"TemplateList\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"TemplateSubPatternAssociation\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"Templates\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"TemplatesHandler\" (\"javax.xml.transform.sax\" \"src/javax/xml/transform/sax\")) (\"TemplatesHandlerImpl\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"TemplatesImpl\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"TemplatesImplProxy\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"TestDriver\" (\"org.apache.xml.utils.synthetic\" \"src/org/apache/xml/utils/synthetic\")) (\"TestGenerator\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"TestSeq\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"Text\" (\"org.w3c.dom\" \"src/org/w3c/dom\")) (\"Text\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"TextAction\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"TextArea\" (\"java.awt\" \"src/java/awt\")) (\"TextAreaDocument\" (\"javax.swing.text.html\" \"src/javax/swing/text/html\")) (\"TextAreaPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"TextAttribute\" (\"java.awt.font\" \"src/java/awt/font\")) (\"TextBoundaryData\" (\"java.text\" \"src/java/text\")) (\"TextComponent\" (\"java.awt\" \"src/java/awt\")) (\"TextComponentPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"TextEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"TextField\" (\"java.awt\" \"src/java/awt\")) (\"TextFieldPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"TextHitInfo\" (\"java.awt.font\" \"src/java/awt/font\")) (\"TextImpl\" (\"org.apache.xerces.impl.xs.opti\" \"src/org/apache/xerces/impl/xs/opti\")) (\"TextImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"TextJustifier\" (\"java.awt.font\" \"src/java/awt/font\")) (\"TextLayout\" (\"java.awt.font\" \"src/java/awt/font\")) (\"TextLayoutStrategy\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"TextLine\" (\"java.awt.font\" \"src/java/awt/font\")) (\"TextListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"TextMeasurer\" (\"java.awt.font\" \"src/java/awt/font\")) (\"TextSerializer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"TextSyntax\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"TextUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"TexturePaint\" (\"java.awt\" \"src/java/awt\")) (\"TexturePaintContext\" (\"java.awt\" \"src/java/awt\")) (\"Thread\" (\"java.lang\" \"src/java/lang\")) (\"ThreadControllerWrapper\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"ThreadDeath\" (\"java.lang\" \"src/java/lang\")) (\"ThreadGroup\" (\"java.lang\" \"src/java/lang\")) (\"ThreadLocal\" (\"java.lang\" \"src/java/lang\")) (\"Throwable\" (\"java.lang\" \"src/java/lang\")) (\"Tie\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"TileObserver\" (\"java.awt.image\" \"src/java/awt/image\")) (\"Time\" (\"java.sql\" \"src/java/sql\")) (\"TimeDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"TimeLimitExceededException\" (\"javax.naming\" \"src/javax/naming\")) (\"TimeZone\" (\"java.util\" \"src/java/util\")) (\"Timer\" (\"javax.swing\" \"src/javax/swing\")) (\"Timer\" (\"java.util\" \"src/java/util\")) (\"TimerQueue\" (\"javax.swing\" \"src/javax/swing\")) (\"TimerTask\" (\"java.util\" \"src/java/util\")) (\"Timestamp\" (\"java.sql\" \"src/java/sql\")) (\"TitledBorder\" (\"javax.swing.border\" \"src/javax/swing/border\")) (\"ToHTMLSAXHandler\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ToHTMLStream\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ToSAXHandler\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ToStream\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ToTextSAXHandler\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ToTextStream\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ToUnknownStream\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ToXMLSAXHandler\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"ToXMLStream\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"Token\" (\"org.apache.xerces.impl.xpath.regex\" \"src/org/apache/xerces/impl/xpath/regex\")) (\"TooManyListenersException\" (\"java.util\" \"src/java/util\")) (\"ToolBarUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"ToolTipManager\" (\"javax.swing\" \"src/javax/swing\")) (\"ToolTipUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"Toolkit\" (\"java.awt\" \"src/java/awt\")) (\"TopLevelElement\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"TrAXFilter\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"TrAXFilter\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"TraceListener\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"TraceListenerEx\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"TraceListenerEx2\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"TraceListenerEx3\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"TraceManager\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"TracerEvent\" (\"org.apache.xalan.trace\" \"src/org/apache/xalan/trace\")) (\"Track\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"TransactionRequiredException\" (\"javax.transaction\" \"src/javax/transaction\")) (\"TransactionRolledbackException\" (\"javax.transaction\" \"src/javax/transaction\")) (\"TransferHandler\" (\"javax.swing\" \"src/javax/swing\")) (\"Transferable\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"Transform\" (\"org.apache.xalan.xsltc.cmdline\" \"src/org/apache/xalan/xsltc/cmdline\")) (\"TransformAttribute\" (\"java.awt.font\" \"src/java/awt/font\")) (\"TransformSnapshot\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"TransformSnapshotImpl\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"TransformState\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"TransformStateSetter\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"Transformer\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"TransformerClient\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"TransformerConfigurationException\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"TransformerException\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"TransformerFactory\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"TransformerFactoryConfigurationError\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"TransformerFactoryImpl\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"TransformerFactoryImpl\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"TransformerHandler\" (\"javax.xml.transform.sax\" \"src/javax/xml/transform/sax\")) (\"TransformerHandlerImpl\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"TransformerHandlerImpl\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"TransformerIdentityImpl\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"TransformerImpl\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"TransformerImpl\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"Translet\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"TransletException\" (\"org.apache.xalan.xsltc\" \"src/org/apache/xalan/xsltc\")) (\"TransletOutput\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"TransletOutputHandlerFactory\" (\"org.apache.xalan.xsltc.runtime.output\" \"src/org/apache/xalan/xsltc/runtime/output\")) (\"Transmitter\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"Transparency\" (\"java.awt\" \"src/java/awt\")) (\"TreeCellEditor\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"TreeCellRenderer\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"TreeExpansionEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TreeExpansionListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TreeMap\" (\"java.util\" \"src/java/util\")) (\"TreeModel\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"TreeModelEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TreeModelListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TreeNode\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"TreePath\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"TreeSelectionEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TreeSelectionListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"TreeSelectionModel\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"TreeSet\" (\"java.util\" \"src/java/util\")) (\"TreeUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"TreeWalker\" (\"org.w3c.dom.traversal\" \"src/org/w3c/dom/traversal\")) (\"TreeWalker\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"TreeWalker2Result\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"TreeWalkerImpl\" (\"org.apache.xerces.dom\" \"src/org/apache/xerces/dom\")) (\"TreeWillExpandListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"Trie\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"Type\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"TypeCheckError\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"TypeInfo\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"TypeValidator\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"Types\" (\"java.sql\" \"src/java/sql\")) (\"UCSReader\" (\"org.apache.xerces.impl.io\" \"src/org/apache/xerces/impl/io\")) (\"UID\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"UIDefaults\" (\"javax.swing\" \"src/javax/swing\")) (\"UIEvent\" (\"org.w3c.dom.events\" \"src/org/w3c/dom/events\")) (\"UIManager\" (\"javax.swing\" \"src/javax/swing\")) (\"UIResource\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"URI\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"URI\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"URI\" (\"java.net\" \"src/java/net\")) (\"URIException\" (\"javax.print\" \"src/javax/print\")) (\"URIResolver\" (\"javax.xml.transform\" \"src/javax/xml/transform\")) (\"URISyntax\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"URISyntaxException\" (\"java.net\" \"src/java/net\")) (\"URL\" (\"java.net\" \"src/java/net\")) (\"URLClassLoader\" (\"java.net\" \"src/java/net\")) (\"URLConnection\" (\"java.net\" \"src/java/net\")) (\"URLDecoder\" (\"java.net\" \"src/java/net\")) (\"URLEncoder\" (\"java.net\" \"src/java/net\")) (\"URLStreamHandler\" (\"java.net\" \"src/java/net\")) (\"URLStreamHandlerFactory\" (\"java.net\" \"src/java/net\")) (\"UTF8Reader\" (\"org.apache.xerces.impl.io\" \"src/org/apache/xerces/impl/io\")) (\"UTFDataFormatException\" (\"java.io\" \"src/java/io\")) (\"UnImplNode\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"UnaryOpExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"UnaryOperation\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"UndeclaredThrowableException\" (\"java.lang.reflect\" \"src/java/lang/reflect\")) (\"UndoManager\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"UndoableEdit\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"UndoableEditEvent\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"UndoableEditListener\" (\"javax.swing.event\" \"src/javax/swing/event\")) (\"UndoableEditSupport\" (\"javax.swing.undo\" \"src/javax/swing/undo\")) (\"UnexpectedException\" (\"java.rmi\" \"src/java/rmi\")) (\"UnicastRemoteObject\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"UnicodeClassMapping\" (\"java.text\" \"src/java/text\")) (\"UnionChildIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"UnionDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"UnionIterator\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"UnionPathExpr\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"UnionPathIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"UnionPattern\" (\"org.apache.xpath.patterns\" \"src/org/apache/xpath/patterns\")) (\"UniqueOrKey\" (\"org.apache.xerces.impl.xs.identity\" \"src/org/apache/xerces/impl/xs/identity\")) (\"UnknownError\" (\"java.lang\" \"src/java/lang\")) (\"UnknownGroupException\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"UnknownHostException\" (\"java.rmi\" \"src/java/rmi\")) (\"UnknownHostException\" (\"java.net\" \"src/java/net\")) (\"UnknownObjectException\" (\"java.rmi.activation\" \"src/java/rmi/activation\")) (\"UnknownServiceException\" (\"java.net\" \"src/java/net\")) (\"UnmappableCharacterException\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"UnmarshalException\" (\"java.rmi\" \"src/java/rmi\")) (\"UnmodifiableSetException\" (\"javax.print.attribute\" \"src/javax/print/attribute\")) (\"UnparsedEntityUriCall\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"UnrecoverableKeyException\" (\"java.security\" \"src/java/security\")) (\"Unreferenced\" (\"java.rmi.server\" \"src/java/rmi/server\")) (\"UnresolvedAddressException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"UnresolvedPermission\" (\"java.security\" \"src/java/security\")) (\"UnresolvedPermissionCollection\" (\"java.security\" \"src/java/security\")) (\"UnresolvedRef\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"UnsatisfiedLinkError\" (\"java.lang\" \"src/java/lang\")) (\"UnsolicitedNotification\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"UnsolicitedNotificationEvent\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"UnsolicitedNotificationListener\" (\"javax.naming.ldap\" \"src/javax/naming/ldap\")) (\"UnsupportedAddressTypeException\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"UnsupportedAudioFileException\" (\"javax.sound.sampled\" \"src/javax/sound/sampled\")) (\"UnsupportedCharsetException\" (\"java.nio.charset\" \"src/java/nio/charset\")) (\"UnsupportedClassVersionError\" (\"java.lang\" \"src/java/lang\")) (\"UnsupportedElement\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"UnsupportedEncodingException\" (\"java.io\" \"src/java/io\")) (\"UnsupportedFlavorException\" (\"java.awt.datatransfer\" \"src/java/awt/datatransfer\")) (\"UnsupportedLookAndFeelException\" (\"javax.swing\" \"src/javax/swing\")) (\"UnsupportedOperationException\" (\"java.lang\" \"src/java/lang\")) (\"UseAttributeSets\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"UserDataHandler\" (\"org.apache.xerces.dom3\" \"src/org/apache/xerces/dom3\")) (\"Util\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"Util\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"Util\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"UtilDelegate\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"Utilities\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"Utils\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"VMID\" (\"java.rmi.dgc\" \"src/java/rmi/dgc\")) (\"ValidatedInfo\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"ValidationContext\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"ValidationManager\" (\"org.apache.xerces.impl.validation\" \"src/org/apache/xerces/impl/validation\")) (\"ValidationState\" (\"org.apache.xerces.impl.validation\" \"src/org/apache/xerces/impl/validation\")) (\"ValueHandler\" (\"javax.rmi.CORBA\" \"src/javax/rmi/CORBA\")) (\"ValueOf\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"ValueStore\" (\"org.apache.xerces.impl.xs.identity\" \"src/org/apache/xerces/impl/xs/identity\")) (\"VarNameCollector\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"Variable\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"Variable\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"VariableBase\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"VariableHeightLayoutCache\" (\"javax.swing.tree\" \"src/javax/swing/tree\")) (\"VariableRef\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"VariableRefBase\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"VariableSafeAbsRef\" (\"org.apache.xpath.operations\" \"src/org/apache/xpath/operations\")) (\"VariableStack\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"Vector\" (\"java.util\" \"src/java/util\")) (\"VerifyError\" (\"java.lang\" \"src/java/lang\")) (\"Version\" (\"org.apache.xmlcommons\" \"src/org/apache/xmlcommons\")) (\"Version\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"Version\" (\"org.apache.xalan\" \"src/org/apache/xalan\")) (\"VetoableChangeListener\" (\"java.beans\" \"src/java/beans\")) (\"VetoableChangeListenerProxy\" (\"java.beans\" \"src/java/beans\")) (\"VetoableChangeSupport\" (\"java.beans\" \"src/java/beans\")) (\"View\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"ViewCSS\" (\"org.w3c.dom.css\" \"src/org/w3c/dom/css\")) (\"ViewFactory\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"ViewportLayout\" (\"javax.swing\" \"src/javax/swing\")) (\"ViewportUI\" (\"javax.swing.plaf\" \"src/javax/swing/plaf\")) (\"VirtualMachineError\" (\"java.lang\" \"src/java/lang\")) (\"Visibility\" (\"java.beans\" \"src/java/beans\")) (\"VoiceStatus\" (\"javax.sound.midi\" \"src/javax/sound/midi\")) (\"Void\" (\"java.lang\" \"src/java/lang\")) (\"VoidType\" (\"org.apache.xalan.xsltc.compiler.util\" \"src/org/apache/xalan/xsltc/compiler/util\")) (\"VolatileImage\" (\"java.awt.image\" \"src/java/awt/image\")) (\"WMLAElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLAElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLAccessElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLAccessElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLAnchorElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLAnchorElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLBElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLBElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLBigElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLBigElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLBrElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLBrElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLCardElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLCardElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLDOMImplementation\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLDOMImplementationImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLDoElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLDoElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLDocument\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLDocumentImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLEmElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLEmElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLFieldsetElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLFieldsetElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLGoElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLGoElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLHeadElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLHeadElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLIElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLIElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLImgElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLImgElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLInputElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLInputElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLMetaElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLMetaElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLNoopElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLNoopElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLOneventElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLOneventElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLOptgroupElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLOptgroupElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLOptionElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLOptionElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLPElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLPElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLPostfieldElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLPostfieldElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLPrevElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLPrevElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLRefreshElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLRefreshElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLSelectElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLSelectElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLSetvarElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLSetvarElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLSmallElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLSmallElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLStrongElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLStrongElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLTableElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLTableElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLTdElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLTdElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLTemplateElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLTemplateElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLTimerElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLTimerElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLTrElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLTrElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLUElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLUElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WMLWmlElement\" (\"org.apache.wml\" \"src/org/apache/wml\")) (\"WMLWmlElementImpl\" (\"org.apache.wml.dom\" \"src/org/apache/wml/dom\")) (\"WalkerFactory\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"WalkingIterator\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"WalkingIteratorSorted\" (\"org.apache.xpath.axes\" \"src/org/apache/xpath/axes\")) (\"WeakHashMap\" (\"java.util\" \"src/java/util\")) (\"WeakReference\" (\"java.lang.ref\" \"src/java/lang/ref\")) (\"When\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"WhiteSpaceInfo\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"Whitespace\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"WhitespaceInfoPaths\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"WhitespaceStrippingElementMatcher\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"Window\" (\"java.awt\" \"src/java/awt\")) (\"WindowAdapter\" (\"java.awt.event\" \"src/java/awt/event\")) (\"WindowConstants\" (\"javax.swing\" \"src/javax/swing\")) (\"WindowEvent\" (\"java.awt.event\" \"src/java/awt/event\")) (\"WindowFocusListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"WindowListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"WindowPeer\" (\"java.awt.peer\" \"src/java/awt/peer\")) (\"WindowStateListener\" (\"java.awt.event\" \"src/java/awt/event\")) (\"WithParam\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"WordBreakData\" (\"java.text\" \"src/java/text\")) (\"WordBreakTable\" (\"java.text\" \"src/java/text\")) (\"WrappedPlainView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"WrappedRuntimeException\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"WritableByteChannel\" (\"java.nio.channels\" \"src/java/nio/channels\")) (\"WritableRaster\" (\"java.awt.image\" \"src/java/awt/image\")) (\"WritableRenderedImage\" (\"java.awt.image\" \"src/java/awt/image\")) (\"WriteAbortedException\" (\"java.io\" \"src/java/io\")) (\"Writer\" (\"java.io\" \"src/java/io\")) (\"WriterOutputBuffer\" (\"org.apache.xalan.xsltc.runtime.output\" \"src/org/apache/xalan/xsltc/runtime/output\")) (\"WriterToASCI\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"WriterToUTF8Buffered\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"WrongNumberArgsException\" (\"org.apache.xpath.functions\" \"src/org/apache/xpath/functions\")) (\"WrongParserException\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"X509CRL\" (\"java.security.cert\" \"src/java/security/cert\")) (\"X509CRLEntry\" (\"java.security.cert\" \"src/java/security/cert\")) (\"X509EncodedKeySpec\" (\"java.security.spec\" \"src/java/security/spec\")) (\"X509Extension\" (\"java.security.cert\" \"src/java/security/cert\")) (\"XAConnection\" (\"javax.sql\" \"src/javax/sql\")) (\"XADataSource\" (\"javax.sql\" \"src/javax/sql\")) (\"XAException\" (\"javax.transaction.xa\" \"src/javax/transaction/xa\")) (\"XAResource\" (\"javax.transaction.xa\" \"src/javax/transaction/xa\")) (\"XBoolean\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XBooleanStatic\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XConnection\" (\"org.apache.xalan.lib.sql\" \"src/org/apache/xalan/lib/sql\")) (\"XHTMLSerializer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"XInclude11TextReader\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"XIncludeHandler\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"XIncludeMessageFormatter\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"XIncludeNamespaceSupport\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"XIncludeParserConfiguration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"XIncludeTextReader\" (\"org.apache.xerces.xinclude\" \"src/org/apache/xerces/xinclude\")) (\"XInt\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"XIntPool\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"XML11Char\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"XML11Configuration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"XML11DTDDVFactoryImpl\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"XML11DTDProcessor\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XML11DTDScannerImpl\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XML11DTDValidator\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XML11DocumentScannerImpl\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XML11EntityScanner\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XML11IDDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"XML11IDREFDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"XML11NMTOKENDatatypeValidator\" (\"org.apache.xerces.impl.dv.dtd\" \"src/org/apache/xerces/impl/dv/dtd\")) (\"XML11NSDTDValidator\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XML11NSDocumentScannerImpl\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XML11NamespaceBinder\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XML11Serializer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"XMLAttributeDecl\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLAttributes\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XMLAttributesImpl\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"XMLCatalogResolver\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"XMLChar\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"XMLChar\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"XMLCharacterRecognizer\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"XMLComponent\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLComponentManager\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLConfigurationException\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLContentSpec\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLDTDContentModelFilter\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLDTDContentModelHandler\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XMLDTDContentModelSource\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLDTDDescription\" (\"org.apache.xerces.xni.grammars\" \"src/org/apache/xerces/xni/grammars\")) (\"XMLDTDDescription\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLDTDFilter\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLDTDHandler\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XMLDTDLoader\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLDTDProcessor\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLDTDScanner\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLDTDScannerImpl\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLDTDSource\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLDTDValidator\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLDTDValidatorFilter\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLDecoder\" (\"java.beans\" \"src/java/beans\")) (\"XMLDocumentFilter\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLDocumentFragmentHandler\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XMLDocumentFragmentScannerImpl\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLDocumentHandler\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XMLDocumentParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"XMLDocumentScanner\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLDocumentScannerImpl\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLDocumentSource\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLElementDecl\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLEncoder\" (\"java.beans\" \"src/java/beans\")) (\"XMLEntityDecl\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLEntityHandler\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLEntityManager\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLEntityResolver\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLEntityScanner\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLErrorHandler\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLErrorReporter\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLErrorResources\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_ca\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_cs\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_de\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_es\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_fr\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_hu\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_it\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_ja\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_ko\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_pl\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_pt_BR\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_ru\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_sk\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_sl\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_tr\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_zh\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLErrorResources_zh_TW\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLFilter\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"XMLFilterImpl\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"XMLFormatter\" (\"java.util.logging\" \"src/java/util/logging\")) (\"XMLGrammarCachingConfiguration\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"XMLGrammarDescription\" (\"org.apache.xerces.xni.grammars\" \"src/org/apache/xerces/xni/grammars\")) (\"XMLGrammarLoader\" (\"org.apache.xerces.xni.grammars\" \"src/org/apache/xerces/xni/grammars\")) (\"XMLGrammarParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"XMLGrammarPool\" (\"org.apache.xerces.xni.grammars\" \"src/org/apache/xerces/xni/grammars\")) (\"XMLGrammarPoolImpl\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"XMLGrammarPreparser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"XMLInputSource\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLLocator\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XMLMessageFormatter\" (\"org.apache.xerces.impl.msg\" \"src/org/apache/xerces/impl/msg\")) (\"XMLMessages\" (\"org.apache.xml.res\" \"src/org/apache/xml/res\")) (\"XMLNSDTDValidator\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLNSDecl\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"XMLNSDocumentScannerImpl\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLNamespaceBinder\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLNotationDecl\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLParseException\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLParser\" (\"org.apache.xerces.parsers\" \"src/org/apache/xerces/parsers\")) (\"XMLParserConfiguration\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLPullParserConfiguration\" (\"org.apache.xerces.xni.parser\" \"src/org/apache/xerces/xni/parser\")) (\"XMLReader\" (\"org.xml.sax\" \"src/org/xml/sax\")) (\"XMLReaderAdapter\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"XMLReaderFactory\" (\"org.xml.sax.helpers\" \"src/org/xml/sax/helpers\")) (\"XMLReaderManager\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"XMLResourceIdentifier\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XMLResourceIdentifierImpl\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"XMLScanner\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XMLSchemaDescription\" (\"org.apache.xerces.xni.grammars\" \"src/org/apache/xerces/xni/grammars\")) (\"XMLSchemaException\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XMLSchemaLoader\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XMLSchemaValidator\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XMLSerializer\" (\"org.apache.xml.serialize\" \"src/org/apache/xml/serialize\")) (\"XMLSimpleType\" (\"org.apache.xerces.impl.dtd\" \"src/org/apache/xerces/impl/dtd\")) (\"XMLString\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"XMLString\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XMLStringBuffer\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"XMLStringDefault\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"XMLStringFactory\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"XMLStringFactoryDefault\" (\"org.apache.xml.utils\" \"src/org/apache/xml/utils\")) (\"XMLStringFactoryImpl\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XMLSymbols\" (\"org.apache.xerces.util\" \"src/org/apache/xerces/util\")) (\"XMLVersionDetector\" (\"org.apache.xerces.impl\" \"src/org/apache/xerces/impl\")) (\"XNIException\" (\"org.apache.xerces.xni\" \"src/org/apache/xerces/xni\")) (\"XNodeSet\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XNodeSetForDOM\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XNull\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XNumber\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XObject\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XObjectFactory\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XPATHErrorResources\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_ca\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_ca_ES\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_cs\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_cs_CZ\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_de\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_de_DE\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_es\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_es_ES\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_fr\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_fr_FR\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_hu\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_hu_HU\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_it\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_it_IT\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_ja\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_ja_JP\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_ko\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_ko_KR\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_pl\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_pl_PL\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_pt_BR\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_ru\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_ru_RU\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_sk\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_sk_SK\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_sl\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_sl_SI\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_tr\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_tr_TR\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_zh\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_zh_CN\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHErrorResources_zh_TW\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPATHMessages\" (\"org.apache.xpath.res\" \"src/org/apache/xpath/res\")) (\"XPath\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"XPath\" (\"org.apache.xerces.impl.xpath\" \"src/org/apache/xerces/impl/xpath\")) (\"XPathAPI\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"XPathContext\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"XPathDumper\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"XPathEvaluatorImpl\" (\"org.apache.xpath.domapi\" \"src/org/apache/xpath/domapi\")) (\"XPathException\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"XPathException\" (\"org.apache.xerces.impl.xpath\" \"src/org/apache/xerces/impl/xpath\")) (\"XPathExpressionImpl\" (\"org.apache.xpath.domapi\" \"src/org/apache/xpath/domapi\")) (\"XPathFactory\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"XPathLexer\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"XPathMatcher\" (\"org.apache.xerces.impl.xs.identity\" \"src/org/apache/xerces/impl/xs/identity\")) (\"XPathNSResolverImpl\" (\"org.apache.xpath.domapi\" \"src/org/apache/xpath/domapi\")) (\"XPathNamespaceImpl\" (\"org.apache.xpath.domapi\" \"src/org/apache/xpath/domapi\")) (\"XPathParser\" (\"org.apache.xpath.compiler\" \"src/org/apache/xpath/compiler\")) (\"XPathParser\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"XPathProcessorException\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"XPathResultImpl\" (\"org.apache.xpath.domapi\" \"src/org/apache/xpath/domapi\")) (\"XPathVisitable\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"XPathVisitor\" (\"org.apache.xpath\" \"src/org/apache/xpath\")) (\"XRTreeFrag\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XRTreeFragSelectWrapper\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XResourceBundle\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResourceBundleBase\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_cy\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_de\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_el\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_en\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_es\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_fr\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_he\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_hy\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_it\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_ja_JP_A\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_ja_JP_HA\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_ja_JP_HI\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_ja_JP_I\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_ka\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_ko\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_sl\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_sv\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_zh_CN\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XResources_zh_TW\" (\"org.apache.xml.utils.res\" \"src/org/apache/xml/utils/res\")) (\"XSAllCM\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"XSAnnotation\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSAnnotationImpl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSAttributeChecker\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSAttributeDecl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSAttributeDeclaration\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSAttributeGroupDecl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSAttributeGroupDefinition\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSAttributeUse\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSAttributeUseImpl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSCMBinOp\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"XSCMLeaf\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"XSCMUniOp\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"XSCMValidator\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"XSComplexTypeDecl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSComplexTypeDefinition\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSConstants\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSConstraints\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSDAbstractIDConstraintTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDAbstractParticleTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDAbstractTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDAttributeGroupTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDAttributeTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDComplexTypeTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDDescription\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSDElementTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDFACM\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"XSDGroupTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDHandler\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDKeyrefTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDNotationTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDSimpleTypeTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDUniqueOrKeyTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDWildcardTraverser\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSDeclarationPool\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSDocumentInfo\" (\"org.apache.xerces.impl.xs.traversers\" \"src/org/apache/xerces/impl/xs/traversers\")) (\"XSElementDecl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSElementDeclaration\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSEmptyCM\" (\"org.apache.xerces.impl.xs.models\" \"src/org/apache/xerces/impl/xs/models\")) (\"XSException\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSFacet\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSFacets\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"XSGrammar\" (\"org.apache.xerces.xni.grammars\" \"src/org/apache/xerces/xni/grammars\")) (\"XSGrammarBucket\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSGrammarPool\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"XSGroupDecl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSIDCDefinition\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSImplementation\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSImplementationImpl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSLInfiniteLoopException\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"XSLMessages\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLOutputAttributes\" (\"org.apache.xml.serializer\" \"src/org/apache/xml/serializer\")) (\"XSLProcessorContext\" (\"org.apache.xalan.extensions\" \"src/org/apache/xalan/extensions\")) (\"XSLProcessorVersion\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"XSLTAttributeDef\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"XSLTC\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"XSLTCDTMManager\" (\"org.apache.xalan.xsltc.dom\" \"src/org/apache/xalan/xsltc/dom\")) (\"XSLTCSource\" (\"org.apache.xalan.xsltc.trax\" \"src/org/apache/xalan/xsltc/trax\")) (\"XSLTElementDef\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"XSLTElementProcessor\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"XSLTErrorResources\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_ca\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_ca_ES\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_cs\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_cs_CZ\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_de\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_de_DE\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_es\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_es_ES\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_fr\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_fr_FR\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_hu\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_hu_HU\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_it\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_it_IT\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_ja\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_ja_JP\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_ko\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_ko_KR\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_pl\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_pl_PL\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_pt_BR\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_ru\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_ru_RU\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_sk\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_sk_SK\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_sl\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_sl_SI\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_tr\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_tr_TR\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_zh\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_zh_CN\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTErrorResources_zh_TW\" (\"org.apache.xalan.res\" \"src/org/apache/xalan/res\")) (\"XSLTProcessorApplet\" (\"org.apache.xalan.client\" \"src/org/apache/xalan/client\")) (\"XSLTSchema\" (\"org.apache.xalan.processor\" \"src/org/apache/xalan/processor\")) (\"XSLTVisitable\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"XSLTVisitor\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"XSLoader\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSMessageFormatter\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSModel\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSModelGroup\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSModelGroupDefinition\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSModelGroupImpl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSModelImpl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSMultiValueFacet\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSNamedMap\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSNamedMap4Types\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"XSNamedMapImpl\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"XSNamespaceItem\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSNamespaceItemList\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSNotationDecl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSNotationDeclaration\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSObject\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSObjectList\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSObjectListImpl\" (\"org.apache.xerces.impl.xs.util\" \"src/org/apache/xerces/impl/xs/util\")) (\"XSParticle\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSParticleDecl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XSSimpleType\" (\"org.apache.xerces.impl.dv\" \"src/org/apache/xerces/impl/dv\")) (\"XSSimpleTypeDecl\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"XSSimpleTypeDefinition\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSTerm\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSTypeDefinition\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSWildcard\" (\"org.apache.xerces.xs\" \"src/org/apache/xerces/xs\")) (\"XSWildcardDecl\" (\"org.apache.xerces.impl.xs\" \"src/org/apache/xerces/impl/xs\")) (\"XString\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XStringForChars\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XStringForFSB\" (\"org.apache.xpath.objects\" \"src/org/apache/xpath/objects\")) (\"XUnresolvedVariable\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"XUnresolvedVariableSimple\" (\"org.apache.xalan.templates\" \"src/org/apache/xalan/templates\")) (\"XalanProperties\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"XalanTransformState\" (\"org.apache.xalan.transformer\" \"src/org/apache/xalan/transformer\")) (\"Xid\" (\"javax.transaction.xa\" \"src/javax/transaction/xa\")) (\"XmlSupport\" (\"java.util.prefs\" \"src/java/util/prefs\")) (\"XslAttribute\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"XslElement\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")) (\"YearDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"YearMonthDV\" (\"org.apache.xerces.impl.dv.xs\" \"src/org/apache/xerces/impl/dv/xs\")) (\"ZipConstants\" (\"java.util.zip\" \"src/java/util/zip\")) (\"ZipEntry\" (\"java.util.zip\" \"src/java/util/zip\")) (\"ZipException\" (\"java.util.zip\" \"src/java/util/zip\")) (\"ZipFile\" (\"java.util.zip\" \"src/java/util/zip\")) (\"ZipInputStream\" (\"java.util.zip\" \"src/java/util/zip\")) (\"ZipOutputStream\" (\"java.util.zip\" \"src/java/util/zip\")) (\"ZoneView\" (\"javax.swing.text\" \"src/javax/swing/text\")) (\"sym\" (\"org.apache.xalan.xsltc.compiler\" \"src/org/apache/xalan/xsltc/compiler\")))"

)
  "An alist that contains all of the class names, package names and file
names contained in the current JDK.  This list is created once by the
function shu-parse-base-jar and then byte compiled into this file.")



;;; shu-java-base.el ends here
