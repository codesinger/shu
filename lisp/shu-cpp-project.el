;; shu-cpp-project.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-cpp-project
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

;; A collection of useful functions for dealing with project files and treating
;; a set of source files in multiple directories as a single project
;;
;; ### Toggle back and forth between files ###
;;
;; If you are editing a C or C++ file and wish to switch to its associated
;; header file, SHU-HOTHER will switch to the header file.  SHU-COTHER will
;; switch back to the original C or C++ file.  SHU-TOTHER will switch to the
;; associated unit test file that ends in \"t.cpp.""
;; This set of functions allows you to treat a set of files as a single project.
;;
;; You define a project by creating a project file.  A project file is simply a
;; text file with one or more directory names in it.  You select the text in the
;; file and invoke the command shu-make-c-project.  This searches the given
;; directories for all of the header files and C and C++ files and remembers
;; where these files are located.
;;
;; When you want to visit a file you start typing the first few characters of the
;; file name.  You can use auto completion to complete the name.  Since
;; shu-project knows where all of the files are located, you do not have to
;; remember that.  Once you have typed in a complete file name, the file will be
;; visited wherever it happens to reside.
;;
;; If you have two or more files with the same name in different directories, you
;; will be presented with a menu and asked to select the file you have in mind.
;;
;; ### Creating a project file ###
;;
;; If you do not have a project file, shu-project gives you a convenient way to
;; create one.  Go to the root directory of your project, open a new empty file
;; (I usually call it project.txt), and invoke the command shu-make-c-project.
;; It will prompt you for the root directory with a default being the current
;; directory.  It will then find all of the directories at or below the root that
;; contain code and it will place the directory names in the file.  Now you have
;; your project file.
;;
;; ### File names within the project ###
;;
;; shu-project now knows where all of the file names reside so you do not have to
;; remember that.  But it does more to simplify your life.  Many files in large
;; projects start with a common prefix.  If you have a class called ThingLoader,
;; it might be defined in a file called thingloader.h.  But in a large project,
;; the full file name might be something like myproject_thingloader.h.
;;
;; As shu-project is creating the list of file names, it is also creating an
;; index of short names with common prefixes stripped.  So if you want to visit
;; myproject_thingloader.h, just type \"thing\" and hit tab to autocomplete.  If
;; no other file starts with \"thing\", shu-project will autocomplete to
;; \"thingloader\" and then look in its index to find myproject_thingloader.h.
;;
;; ### Visiting related files ###
;;
;; If you are in a .cpp file and you want to visit its associated .h file, issue
;; the command shu-hother and you will be taken to the .h file even if it is in a
;; different directory.  Similarly, you can visit the associated unit test file
;; (t.cpp) with the command shu-tother to visit the unit test file.
;;
;; ### Visiting files based on error messages ###
;;
;; You compile a file and the compiler complains ...
;;
;;      ..\myproject_thingloader.cpp:190:6: error: Invalid type for ...
;;
;; Simply place the cursor under any part of the file name and type Ctl-x h.
;; shu-project will take you to line 190, column 6 of myproject_thingloader.cpp.
;;

;;; Code:

(require 'shu-base)


(defconst shu-cpp-c-extensions (list "c" "cc" "cpp" "Cc" "C")
  "A list of file extensions for all of the C file types we want to find.  This is defined
as defconst in shu-cpp-base.el but may be modified by shu-add-cpp-c-extensions.")

(defconst shu-cpp-h-extensions (list "h" "H" "hh")
  "A list of file extensions for all of the H file types we want to find.  This is defined
as defconst in shu-cpp-base.el but may be modified by shu-add-cpp-h-extensions")

(defconst shu-py-extensions (list "py")
  "A list of file extensions for Python projects")

(defconst shu-project-extensions (append shu-cpp-c-extensions shu-cpp-h-extensions)
  "A list of file extensions for all of the file types we want to find.  This is defined
as defconst in shu-cpp-base.el but may be modified by shu-add-cpp-c-extensions or
shu-add-cpp-h-extensions.")

(defconst shu-project-exclude-list (list "cmake-build" "cmake-distro-dev" "dists")
  "A list of top level directory names to exclude while creating a project via
SHU-MAKE-C-PROJECT.  This list is ignored by SHU-MAKE-FULL-C-PROJECT")

(defvar shu-project-exclude-hash nil
  "The hash table that holds the directory names from SHU-PROJECT-EXCLUDE-LIST.")

(defvar shu-cpp-project-file nil
  "The name of the file from which the current project was read.")

(defvar shu-cpp-project-name nil
  "If the current project was established by either SHU-SETUP-PROJECT-AND-TAGS
of SHU-VISIT-PROJECT-AND-TAGS, this is the name of the interactive function that
was invoked by the user to set it up.  This is useful when you are in a project
and you forgot the name of the interactive function that got you there.")

(defvar shu-cpp-project-list nil
  "List that holds all of the subdirectories in the current project.")

(defvar shu-cpp-class-list nil
  "This is an alist whose keys are unqualified file names and whose values
contain a list of the fully qualified files with the same unqualified name.  if
SHU-CPP-PROJECT-SHORT-NAMES is nil, this list is identical to the one stored in
SHU-CPP-COMPLETING-LIST.")

(defvar shu-cpp-inverted-class-list nil
  "This is the inversion of SHU-CPP-CLASS-LIST.  It is a list of all of the
fully qualified file names found in SHU-CPP-CLASS-LIST.")

(defvar shu-cpp-prefix-list nil
  "This is the list of prefixes removed from the short names if
SHU-CPP-PROJECT-SHORT-NAMES is non-nil.")

(defvar shu-cpp-short-list nil
  "This is the list of short names, if there are any.  The car of each item is
the short name.  The cdr of each item is the full path to the associated
file name.")

(defvar shu-cpp-completing-list nil
  "This is an alist whose keys are unqualified file names and whose values
contain a list of the fully qualified files with the same unqualified name.  If
SHU-CPP-PROJECT-SHORT-NAMES is non-nil, then this alist includes the short file
names as well.")

(defvar shu-cpp-found-extensions (list)
  "This is a list of all of the file extensions found in the current project.  While
shu-project-extensions contains all of the extensions that we look for.  This variable
contains those that we actually found in building the current project.")

(defvar shu-cpp-project-time nil
  "This is the time at which the current project was created.")

(defvar shu-cpp-c-file-count 0
  "This is the count of the number of C files found in the project.")

(defvar shu-cpp-h-file-count 0
  "This is the count of the number of H files found in the project.")

(defconst shu-project-cpp-buffer-name "**Shu Make Project**"
  "The name of the buffer into which messages are placed as c and h files
are being scanned.")

(defvar shu-cpp-completion-target nil
  "Global variable used to hold the function to be invoked at the end of the
current completion.")

(defvar shu-cpp-target-file-line nil
  "If non-nil, this represents the line number that is to be located after a
file is visited by vh() and has gone through buffer completion selection.")

(defvar shu-cpp-target-file-column nil
  "If non-nil, this represents the column number that is to be located after a
file is visited by vh() and has gone through buffer completion selection.")

(defvar shu-default-file-to-seek nil
  "The default file to seek that is proposed as a possible file when vh() finds a
file name under the cursor, possibly with a line number.  If the user chooses a
file other than this one, we need to forget the associated line number.")

(defvar shu-cpp-completion-scratch nil
  "Scratch buffer used by C file name completions.")

(defvar shu-cpp-completion-current-buffer nil
  "Active buffer just before we have to do a completion.")

(defvar shu-cpp-completion-prefix nil
  "The default file name prefix when we are looking for a file and point is not
sitting on something that appears to be a file name.")

(defvar shu-completion-is-directory nil
  "True if we are to use the current directory name as the file name prefix.")

(defvar shu-cpp-final-list nil
  "The name of the shared variable that contains the list of directories assembled
by shu-make-c-project")

(defvar shu-project-user-class-count 0)

(defvar shu-project-file-list
  "This is a list of the full path and name of every file in the project.
It is used when a global change needs to visit every file in the project.")

(defvar shu-cpp-fetch-list nil
  "The name of the shared variable that contains the list of directories assembled
by shu-make-c-project")



;;
;;  shu-project-get-file-info
;;
(defmacro shu-project-get-file-info (plist file-name full-name-list)
  "Extract the file information from one entry in shu-cpp-class-list."
  (let ((titem (make-symbol "item"))
        (tflist (make-symbol "flist")))
    `(let ((,titem)
           (,tflist))
       (setq ,titem (car ,plist))
       (setq ,file-name (car ,titem))
       (setq ,tflist (cdr ,titem))
       (setq ,full-name-list (car ,tflist)))
    ))


;;
;;  Functions for customzing
;;

;; Initial prefix support.  This has to be enhanced with an option that
;; says use the last part of the current directory as the prefix.  The
;; current directory is held in the variable default-directory.  Consider
;; using something like the f.el package to extract the last part of the
;; path to use as the prefix


(defcustom shu-cpp-project-short-names nil
  "Set non-nil if shu-cpp-project creates short names for files in a project.  A
short name is an approximation of the file name that may be easier to type.  For
example, if all of the files in a project begin with a common prefix (e.g.,
\"x_server_mumble.cpp\" and \"x_server_stumble.cpp\", then the short names for
these two files would be \"mumble.cpp\" and \"stumble.cpp\".  This means that
the user does not have to type the prefix in order to find the file.  If the
user types \"mumble.cpp\" as the file name, emacs will open the file
\"x_server_mumble.cpp\"."
  :type '(number)
  :group 'shu-cpp-project)


(defcustom shu-cpp-project-very-short-names t
  "Set non-nil if shu-cpp-project creates very short names for files in a
project.  A short name is an approximation of the file name that may be easier
to type.  For example, if all of the files in a project begin with a common
prefix (e.g., \"my_own_server_mumble.cpp\" and \"my_own_server_stumble.cpp\",
then the short names for these two files would be \"mumble.cpp\" and
\"stumble.cpp\".  This means that the user does not have to type the prefix in
order to find the file.  If the user types \"mumble.cpp\" as the file name,
emacs will open the file \"my_own_server_mumble.cpp\"."
  :type '(number)
  :group 'shu-cpp-project)


;;
;;  shu-set-prefix
;;
(defun shu-set-prefix (prefix)
  "Set the default file name prefix for those times when we are trying to visit
a project file and point is not sitting on something that resembles a file name."
  (interactive "sPrefix? ")
  (setq shu-cpp-completion-prefix prefix)
  )

;;
;;  shu-set-dir-prefix
;;
(defun shu-set-dir-prefix (prefix)
  "Set the default file name prefix to be the current directory name end for those
times when we are trying to visit a project file and point is not sitting on
something that resembles a file name."
  (interactive)
  (setq shu-cpp-completion-prefix nil)
  (setq shu-completion-is-directory t)
  )

;;
;;  shu-clear-prefix
;;
(defun shu-clear-prefix ()
  "Clear the default file name prefix for those times when we are trying to visit
a project file and point is not sitting on something that resembles a file name."
  (setq shu-cpp-completion-prefix nil)
  (setq shu-completion-is-directory nil)
  )

;;
;;  shu-add-cpp-c-extensions
;;
(defun shu-add-cpp-c-extensions (xtns)
  "Add one or more file extensions to the list of C and C++ extensions recognized by the
C package functions.  Argument may be a single extension in a string or a list of strings.
This modifies both shu-cpp-c-extensions and shu-project-extensions."
  (let ((nx xtns))
    (when (not (listp nx))
      (setq nx (list nx)))
    (setq shu-cpp-c-extensions (append shu-cpp-c-extensions nx))
    (setq shu-project-extensions (append shu-cpp-c-extensions shu-cpp-h-extensions))
    ))

;;
;;  shu-add-cpp-h-extensions
;;
(defun shu-add-cpp-h-extensions (xtns)
  "Add one or more file extensions to the list of C and C++ extensions recognized by the
C package functions.  Argument may be a single extension in a string or a list of strings.
This modifies both shu-cpp-h-extensions and shu-project-extensions."
  (let ((nx xtns))
    (when (not (listp nx))
      (setq nx (list nx)))
    (setq shu-cpp-h-extensions (append shu-cpp-h-extensions nx))
    (setq shu-project-extensions (append shu-cpp-c-extensions shu-cpp-h-extensions))
    ))



;;
;;  shu-project-directory-is-excluded
;;
(defun shu-project-directory-is-excluded (directory-name)
  "Return t if the directory name is among those that should be ignored when
looking for files within a project.  Typically, these are the names of
CMake directories that hold generated code."
  (when (not shu-project-exclude-hash)
    (shu-project-make-exclude-hash))
  (gethash directory-name shu-project-exclude-hash)
  )



;;
;;  shu-cpp-project-is-type-wanted
;;
(defun shu-cpp-project-is-type-wanted (sname cpp-type)
  "Determine if a file should be included based on its file type and on the
argument CPP-TYPE.  If CPP-TYPE is t then we only want files of type C++.

If CPP-TYPE is nil, we want any file of any type..

If CPP-TYPE is nil, this function returns true.  If CPP-TYPE is t, this function
returns true iff the file extension of SNAME is one that holds C++ code."
  (let ((is-wanted t)
        (extension))
    (when cpp-type
      (setq is-wanted nil)
      (setq extension (file-name-extension sname))
      (when (member extension shu-project-extensions)
        (setq is-wanted t)))
    is-wanted
    ))




;;
;;  shu-project-file-pattern-match
;;
(defun shu-project-file-pattern-match (name pattern)
  "If PATTERN is nil, then all names qualify and this function returns t.
If PATTERN is non-nil, it is a regular expression that must match NAME.  If
PATTERN matches NAME, this function returns true.

This function returns true if PATTERN is nil (all names wanted) or if
PATTERN is not nil and is a regular expression that is matched by NAME."
  (let ((wanted t))
    (when pattern
      (setq wanted (string-match pattern name)))
    wanted
    ))


;;
;;  shu-make-p-project
;;
(defun shu-make-p-project (proj-root)
  "Create a Python project that is analogous to a c project."
  (interactive "DRoot?: ")
  (setq shu-project-extensions shu-py-extensions)
  (shu-sub-make-c-project proj-root)
  )


;;
;;  shu-make-c-project
;;
(defun shu-make-c-project (proj-root)
  "Create a project file of all directories containing c or h files.  Starts at
the specified root directory and searches all subdirectories for any that
contain c or h files.  Top level directories whose names are found in
SHU-PROJECT-EXCLUDE-LIST are excluded from the search.  Typically
SHU-PROJECT-EXCLUDE-LIST is used to exclude CMake directories that include c or
h files that have been created as part of the build process and are not members
of the repository itself.  It then inserts all of the directory names into the
current file at point."
  (interactive "DRoot?: ")
  (shu-project-make-exclude-hash)
  (shu-sub-make-c-project proj-root)
  )


;;
;;  shu-make-full-c-project
;;
(defun shu-make-full-c-project (proj-root)
  "Create a project file of all directories containing c or h files.
Starts at the specified root directory and searches all subdirectories for
any that contain c or h files.  It then inserts all of the directory names
into the current file at point."
  (interactive "DRoot?: ")
  (setq shu-project-exclude-hash nil)
  (shu-sub-make-c-project proj-root)
  )


;;
;;  shu-sub-make-c-project
;;
(defun shu-sub-make-c-project (proj-root)
  "Create a project file of all directories containing c or h files.
Starts at the specified root directory and searches all subdirectories for
any that contain c or h files.  It then inserts all of the directory names
into the current file at point."
  (let ((level     1)
        (dtop)
        (tlist)
        (local-dir))
    (setq shu-cpp-final-list nil)
    (shu-cpp-project-subdirs (expand-file-name proj-root) level)
    (setq shu-cpp-final-list (sort shu-cpp-final-list 'string<))
    (while shu-cpp-final-list
      (setq local-dir (file-relative-name (car shu-cpp-final-list)))
      (insert (concat local-dir "\n"))
      (setq shu-cpp-final-list (cdr shu-cpp-final-list)))
    ))

;;
;;  shu-cpp-project-subdirs
;;
(defun shu-cpp-project-subdirs (dir-name level)
  "Starting with the directory name DIR-NAME. create a list of subdirectories
whose head is in SHU-CPP-FINAL-LIST, that contains the name of every directory and
subdirectory that contains C, C++, or H files.  This is used by shu-make-c-project
and other functions that wish to discover all directories that might contain
source code."
  (let ((gbuf      (get-buffer-create "*Project List*")) ;
        (dlist )
        (tlist )
        (sname )
        (dir-list )
        (sub-list )
        (cname )
        (dname )
        (got-interest )
        (extension))
    (setq dlist (directory-files dir-name t nil t))
    (setq tlist dlist)
    (while tlist
      (setq cname (car tlist))
      (setq sname (file-name-nondirectory cname))
      (if (file-directory-p cname)
          (progn
            (unless (or (string= sname  ".")
                        (string= sname ".."))
              (unless (and (= level 1)
                           shu-project-exclude-hash
                           (gethash sname shu-project-exclude-hash))
                (setq dir-list (cons cname dir-list)))))
        (when (not got-interest)
          (setq extension (file-name-extension sname))
          (when (member extension shu-project-extensions)
            (setq got-interest t))))
      (setq tlist (cdr tlist)))
    (when got-interest
      (setq shu-cpp-final-list (cons dir-name shu-cpp-final-list)))
    (while dir-list
      (setq dname (car dir-list))
      (shu-cpp-project-subdirs dname (1+ level))
      (setq dir-list (cdr dir-list)))
    ))



;;
;;  shu-set-p-project
;;
(defun shu-set-p-project (start end)
  "Mark a region in a file that contains one subdirectory name per line.  Then
invoke set-c-project and it will find and remember all of the c and h files in
those subdirectories.  You may then subsequently visit any of those files by
invoking M-x vh which will allow you to type in the file name only (with auto
completion) and will then visit the file in the appropriate subdirectory.  If
this function is called interactively, it clears the project name that was
established by either SHU-SETUP-PROJECT-AND-TAGS of SHU-VISIT-PROJECT-AND-TAGS."
  (interactive "r")
  (setq shu-project-extensions shu-py-extensions)
  (setq shu-cpp-project-name nil)
  (shu-internal-set-c-project start end)
  )



;;
;;  shu-set-c-project
;;
(defun shu-set-c-project (start end)
  "Mark a region in a file that contains one subdirectory name per line.  Then
invoke set-c-project and it will find and remember all of the c and h files in
those subdirectories.  You may then subsequently visit any of those files by
invoking M-x vh which will allow you to type in the file name only (with auto
completion) and will then visit the file in the appropriate subdirectory.  If
this function is called interactively, it clears the project name that was
established by either SHU-SETUP-PROJECT-AND-TAGS of SHU-VISIT-PROJECT-AND-TAGS."
  (interactive "r")
  (setq shu-cpp-project-name nil)
  (shu-internal-set-c-project start end)
  )




;;
;;  shu-internal-set-c-project
;;
(defun shu-internal-set-c-project (start end)
  "Mark a region in a file that contains one subdirectory name per line.
Then invoke set-c-project and it will find and remember all of the c and h
files in those subdirectories.  You may then subsequently visit any of
those files by invoking M-x vh which will allow you to type in the file
name only (with auto completion) and will then visit the file in the
appropriate subdirectory."
  (save-excursion
    (let ((sline (shu-the-line-at start))
          (eline (shu-the-line-at end))
          (line-diff 0)
          (eol       nil)
          (dir-name  nil)
          (local-dir (file-name-directory buffer-file-name))
          (shu-cpp-buffer (get-buffer-create shu-project-cpp-buffer-name)))
      (setq shu-cpp-project-list nil)
      (setq shu-cpp-project-file (buffer-file-name))
      (setq shu-cpp-c-file-count 0)
      (setq shu-cpp-h-file-count 0)
      (while (and (<= (shu-current-line) eline) (= line-diff 0)) ; there are more lines
        (setq eol (save-excursion (end-of-line) (point)))
        (when (> eol (point))
          (setq dir-name (concat local-dir (buffer-substring (point) eol)))
          (setq shu-cpp-project-list (cons dir-name shu-cpp-project-list)))
        (setq line-diff (forward-line 1)))
      (shu-renew-c-project))
    ))


;;
;;  shu-renew-c-project
;;
(defun shu-renew-c-project ()
  "Renew a previously established project to pick up any new files."
  (interactive)
  (let ((dir-name)
        (ilist)
        (key-list)
        (c1)
        (file-name)
        (full-name)
        (extension)
        (shu-cpp-buffer (get-buffer-create shu-project-cpp-buffer-name))
        (tlist)
        (ps)
        (short-keys)
        (all-keys)
        (plist))
    (setq shu-project-user-class-count 0)
    (setq shu-project-file-list nil)
    (setq tlist shu-cpp-project-list)
    (if (not tlist)
        (progn
          (message "There is no project to renew.")
          (ding))
      (while tlist
        (setq dir-name (car tlist))
        (setq key-list (append key-list (shu-add-cpp-package-line dir-name)))
        (setq tlist (cdr tlist)))
      ;;
      ;;  key-list is now an a-list. In each entry, the car is the short
      ;;  Unqualified name and the cdr is the fully qualified name.
      ;;  See the doc-string for shu-cpp-subdir-for-package.
      ;;
      ;;  Take all of the files we found and put the list of file names
      ;;  in shu-project-file-list to be used for project global changes.
      ;;
      (setq ilist key-list)
      (while ilist
        (setq c1 (car ilist))
        (setq file-name (car c1))
        (setq full-name (cdr c1))
        (setq extension (file-name-extension file-name))
        (setq shu-project-file-list (cons full-name shu-project-file-list))
        (setq shu-project-user-class-count (1+ shu-project-user-class-count))
        (setq ilist (cdr ilist)))
      (setq shu-cpp-class-list (shu-cpp-project-collapse-list key-list))
      (setq shu-cpp-inverted-class-list nil)
      (setq shu-cpp-prefix-list nil)
      (setq shu-cpp-short-list nil)
      (if (not shu-cpp-project-short-names)
          (setq shu-cpp-completing-list shu-cpp-class-list)
        (setq ps (shu-project-make-short-key-list key-list))
        (setq shu-cpp-prefix-list (car ps))
        (setq short-keys (cdr ps))
        (setq shu-cpp-short-list (shu-cpp-project-collapse-list short-keys))
        (setq all-keys (append key-list short-keys))
        (setq shu-cpp-completing-list (shu-cpp-project-collapse-list all-keys))
        (shu-cpp-finish-project)))
    ))


;;
;;  shu-cpp-finish-project
;;
(defun shu-cpp-finish-project ()
  "Finish constructing a C project from a user file list.  The input is
KEY-LIST, which is an a-list.  The cdr of each entry is the short (unqualified)
file name.  The cdr of each entry is the fully qualified name.  This alist may
have duplicate short names.  This function produces a new list.  The car of each
item is still the short (unqualified) file name.  The cdr is a list of all of
the fully qualified file names to which the short name maps.  If a user selects
a file that has only one fully qualified file name, we open the file.  But if it
has more than one fully qualified file name, we have to ask the user which one
is wanted."
  (let ((counts)
        (c-count         0)
        (h-count         0)
        (dup-count       0)
        (name-name       "name")
        (occur-name      "occurs")
        (shu-cpp-buffer (get-buffer-create shu-project-cpp-buffer-name))
        (plist)
        (file-name)
        (full-name-list))
    (setq counts (shu-cpp-project-get-list-counts shu-cpp-short-list))
    (setq c-count (car counts))
    (setq counts (cdr counts))
    (setq h-count (car counts))
    (setq dup-count (cadr counts))
    (setq shu-cpp-project-time (current-time))
    (setq shu-cpp-c-file-count c-count)
    (setq shu-cpp-h-file-count h-count)
    (setq plist shu-cpp-completing-list)
    (while plist
      (shu-project-get-file-info plist file-name full-name-list)
      (princ (concat file-name ":\n      ") shu-cpp-buffer)
      (princ full-name-list shu-cpp-buffer)
      (princ "\n" shu-cpp-buffer)
      (setq plist (cdr plist)))
    (when (> dup-count 1)
      (setq name-name "names")
      (setq occur-name "occur"))
    (if (= dup-count 0)
        (message "%d C files, %d H files." c-count h-count)
      (message "%d C files, %d H files.  %d %s %s multiple times."
               c-count h-count dup-count name-name occur-name))
    ))



;;
;;  shu-add-cpp-package-line
;;
(defun shu-add-cpp-package-line (dir-name)
  "Called with point at the beginning of the line.  Take the whole line as the
name of a directory, look into the directory, and create an alist of all of the
files in the directory as described in shu-cpp-subdir-for-package."
  (let ((key-list))
    (setq key-list (shu-cpp-subdir-for-package dir-name))
    key-list
    ))

;;
;;  shu-cpp-subdir-for-package
;;
(defun shu-cpp-subdir-for-package (directory-name)
  "Given a subdirectory name return an alist that contains as keys the names
of all of the c and h files in the subdirectory, and as values the the
fully qualified name and path of the c or h file.  So if the directory
\"/u/foo/bar\" contains thing.c and what.h the returned alist would be

      ( (\"thing.c\" \"/u/foo/bar/thing.c\")
        (\"what.h\"  \"/u/foo/bar/what.h\" ) )

This allows us to associate the key \"thing.c\" with the fully qualified
name \"/u/foo/bar/thing.c\"."
  (let*
      ((file-name "")
       (full-name "")
       (xtn-name )
       (item nil)
       (key-list nil)
       (target-extensions (regexp-opt shu-project-extensions t))
       (target-name (concat "[^.]\\." target-extensions "$"))
       (directory-list (directory-files directory-name t target-name)))
    (while directory-list
      (setq full-name (car directory-list))
      (setq file-name (file-name-nondirectory full-name))
      ;; Exclude emacs file reservations
      (when (not (and (file-symlink-p full-name) (string= (substring file-name 0 2) ".#")))
        (setq item (cons file-name full-name))
        (setq key-list (cons item key-list))
        (setq xtn-name (file-name-extension file-name))
        (when (not (member xtn-name shu-cpp-found-extensions))
          (setq shu-cpp-found-extensions (cons xtn-name shu-cpp-found-extensions))))
      (setq directory-list (cdr directory-list)))
    key-list
    ))


;;
;;  shu-clear-c-project
;;
(defun shu-clear-c-project ()
  "Clear an existing project, if any."
  (interactive)
  (setq shu-cpp-project-file nil)
  (setq shu-cpp-project-list nil)
  (setq shu-cpp-class-list nil)
  (setq shu-cpp-inverted-class-list nil)
  (setq shu-cpp-prefix-list nil)
  (setq shu-cpp-short-list nil)
  (setq shu-cpp-completing-list nil)
  )


;;
;;  shu-project-make-exclude-hash
;;
(defun shu-project-make-exclude-hash ()
  "Turn SHU-PROJECT-EXCLUDE-LIST into the hash table SHU-PROJECT-EXCLUDE-HASH.
If SHU-PROJECT-EXCLUDE-LIST is nil or not a list or an empty list, then
SHU-PROJECT-EXCLUDE-HASH is also set to nil."
  (interactive)
  (let ((ht)
        (xl shu-project-exclude-list)
        (xd))
    (setq shu-project-exclude-hash nil)
    (when shu-project-exclude-list
      (when (listp shu-project-exclude-list)
        (when (> (length shu-project-exclude-list) 0)
          (setq ht (make-hash-table :test 'equal :size (length shu-project-exclude-list)))
          (while xl
            (setq xd (car xl))
            (puthash xd xd ht)
            (setq xl (cdr xl)))
          (setq shu-project-exclude-hash ht))))
    ))



;;
;;  shu-vh - Visit a c or h file in a project
;;
;;
(defun shu-vh ()
  "Visit a c or h file in a project.  If point is on something that resembles a file
name, then visit that file.  If the file name is followed by a colon and a number
then go to that line in the file.  If the line number is followed by a colon and
a number then use the second number as the column number within the line."
  (interactive)
  (shu-internal-visit-project-file t)
  )



;;
;;  shu-vj - Visit a c or h file in a project
;;
;;
(defun shu-vj ()
  "Visit a c or h file in a project.  Ignore any text that point is on and visit the
file typed in the completion buffer."
  (interactive)
  (shu-internal-visit-project-file nil)
  )



;;
;;  shu-vf
;;
(defun shu-vf ()
  "If point is on something that looks like a file name, visit the file.  If the
file name is followed by a colon and a number, the number is interpreted as a
line number within the file and point is moved to the beginning of that line.
If the line number is followed by a colon and another number, then the second
number is interpreted as a column number and point is moved to that column
number."
  (interactive)
  (let ((gb (get-buffer-create "**boo**"))
        (ret)
        (x)
        (file)
        (line)
        (column))
    (setq debug-on-error t)
    (setq ret (shu-possible-cpp-file-name t t))
    (princ ret gb)(princ "\n" gb)
    (if (not ret)
        (progn
          (ding)
          (message "%s" "No file name found."))
      (setq file (car ret))
      (princ "file: '" gb)(princ file gb)(princ "'\n" gb)
      (setq ret (cdr ret))
      (when ret
        (setq line (car ret))
        (setq ret (cdr ret))
        (when ret
          (setq column (car ret))))
      (find-file file)
      (when line
        (goto-char (point-min))
        (forward-line (1- line))
        (when column
          (move-to-column (1- column)))))
    ))




;;
;;  shu-internal-visit-project-file
;;
;;    Note: At some point there seemed to be a bug in
;;          completing-read.  Even when require-match is t
;;          <return> does not complete-and-exit, it exits
;;          with the text in the minibuffer, which may or
;;          may not match an entry in the table.
;;          This is the reason for the while loop that
;;          keeps reading until there is a match.
;;
(defun shu-internal-visit-project-file (look-for-target)
  "Visit a c or h file in a project."
  (let
      ((key-list)
       (file-to-seek)
       (real-file-to-seek)
       (tfile)
       (invitation "Visit c or h file: ")
       (completion-prefix)
       (default-file-string))
    (when shu-completion-is-directory
      (setq completion-prefix (shu-get-directory-prefix)))
    (when shu-cpp-completion-prefix
      (setq completion-prefix shu-cpp-completion-prefix)
      (when shu-cpp-project-short-names
        (setq completion-prefix nil)
        ))
    (if (not shu-cpp-completing-list)
        (progn
          (message "No project files have been defined.")
          (ding))
      (if look-for-target                                        ;
          (setq shu-default-file-to-seek (shu-find-default-cpp-name))
        (setq shu-default-file-to-seek nil)
        (setq shu-cpp-target-file-line nil)
        (setq shu-cpp-target-file-column nil))
      (if shu-cpp-target-file-column
          (setq default-file-string (format "%s:%d:%d" shu-default-file-to-seek
                                            shu-cpp-target-file-line
                                            shu-cpp-target-file-column))
        (if shu-cpp-target-file-line
            (setq default-file-string (format "%s:%d" shu-default-file-to-seek
                                              shu-cpp-target-file-line))
          (setq default-file-string shu-default-file-to-seek)))
      (when shu-default-file-to-seek
        (setq completion-prefix nil))
      (while (not tfile)
        (setq file-to-seek
              (completing-read
               (if default-file-string
                   (format "%s(default %s) " invitation default-file-string)
                 invitation)
               shu-cpp-completing-list
               nil
               nil
               completion-prefix
               nil
               shu-default-file-to-seek))
        (if (equal file-to-seek "")
            (or shu-default-file-to-seek (error "There is no default file")))

        (setq tfile (assoc file-to-seek shu-cpp-completing-list))
        (if (not tfile)
            (ding)))
      (shu-cpp-choose-file tfile))
    ))



;;
;;  shu-find-default-cpp-name - If point is sitting on something that looks like
;;    a file name then return it as a default candidate for the file name
;;    we wish to visit.
;;
(defun shu-find-default-cpp-name ()
  "Find a default file name to visit.  Calls shu-find-line-and-file to find a possible file
name and possible line number within the file.  Return the file name if one is found and
sets shu-cpp-target-file-line to the line number if one is found"
  (let
      ((ret-name)
       (ret-list)
       (tlist))
    (setq shu-cpp-target-file-line nil)
    (setq shu-cpp-target-file-column nil)
    (setq ret-list (shu-find-line-and-file))
    (when ret-list
      (setq tlist ret-list)
      (setq ret-name (car tlist))
      (setq tlist (cdr tlist))
      (when tlist
        (setq shu-cpp-target-file-line (car tlist))
        (setq tlist (cdr tlist))
        (when tlist
          (setq shu-cpp-target-file-column (car tlist)))))
    ret-name
    ))


;;
;;
;;  shu-find-line-and-file
;;
(defun shu-find-line-and-file()
  "If point is sitting on the word \"line\", then look for a string of the form
\"line 678 of frobnitz.cpp\" and return a list whose first item is the file name
and whose second item is the line number.  If point is not sitting on the word \"line\",
then check to see if point is sitting on a string that has the syntax of a valid
file name.  If that is the case, remember the file name.  If the file name is
followed by a colon, look for a line number following the colon.  If found, look
for another colon followed by a possible column number.  This function will return
nil if none of the above are found.  If only a file name is found, return a list
with one entry.  If file name and line number, a list with two entries.  If file
name, line number, and column number, a list with three entries."
  (let*
      ((case-fold-search t)           ;; Searches ignore case
       (target-extensions (regexp-opt shu-project-extensions t))
       (target-name (concat shu-cpp-file-name "*\\." target-extensions))
       (target-char shu-cpp-file-name)
       (target-line-file (concat "line\\s-+\\([0-9]+\\)\\s-+\\(?:in\\|of\\)*\\s-+" "\\(" target-name "\\)"))
       (eol (save-excursion (end-of-line) (point)))
       (eoln (save-excursion (forward-line 1) (end-of-line) (point)))
       (x )
       (ret-line )
       (ret-name)
       (file-line-list )
       (ret-val ))

    (setq x (shu-on-the-word-line))
    (if (not x) ;;  We are NOT sitting on the word "line".  Check for file name
        (setq file-line-list (shu-possible-cpp-file-name))
      (save-excursion
        (goto-char x) ;; Look for "line 763 of frobnitz.cpp"
        (when (re-search-forward target-line-file eoln t)
          (setq ret-line (string-to-number (match-string 1))) ;; Line number
          (setq ret-name (match-string 2)) ;; File name
          (setq file-line-list (list ret-name ret-line)))))
    file-line-list
    ))

;;
;;  shu-on-the-word-line
;;
(defun shu-on-the-word-line()
  "Return the character position of the start of the current word if point is sitting
anywhere on the word \"line\".  This is used pick up file positions of the form:
\"line 628 of frobnitz.cpp\""
  (let
      ((case-fold-search t)           ;; Searches ignore case
       (bol (save-excursion (beginning-of-line) (point)))
       (eol (save-excursion (forward-line 1) (end-of-line) (point)))
       (x )                           ;; Set to character position if sitting on
       ;;  "l", "i", "n", or "e"
       (y )
       (z )
       (got-line))                    ;; Point at beginning of word "line" or nil
    (save-excursion
      ;; First see if we are positioned on "l", "i", "n", or "e"
      (setq x (cond
               ((when (looking-at "[LL]") 0))
               ((when (looking-at "[II]") 1))
               ((when (looking-at "[Nn]") 2))
               ((when (looking-at "[Ee]") 3))
               (t nil)))
      (when x   ;; Positioned on one of the letters of "line"
        (setq y (- (point) x))    ;; Get point of possible word start
        (setq z (1- y))           ;; Get point of possible char in front of word
        (if (>= z bol)            ;; We can look at char before word start
            (progn
              (goto-char z)         ;; Match any non-letter, followed by "line", then whitespace
              (when (looking-at "[^a-zA-Z]+line\\s-+")
                (setq got-line (1+ z))))
          ;;                        ;; No room to look at char before word start
          (goto-char y)           ;; Match (at beginning of line) "line" followed by whitespace
          (when (looking-at "line\\s-+")
            (setq got-line y)))))
    got-line                     ;; Return nil or point of beginning of "line"
    ))



;;
;;  shu-possible-cpp-file-name
;;
(defun shu-possible-cpp-file-name (&optional include-directory any-extension)
  "Return a list containing a possible file name with a possible line number
and a possible column number.  If the thing on point does not resemble a file
name, return nil.  If it looks like a file name, save it and call
shu-get-line-column-of-file to perhaps harvest a line number and column number
within the file.  The return result is a list of length one if there is only
a file name, a list of length two if there is a file name and line number, a
list of length three if there is a file name, line number, and column number.
If the optional argument INCLUDE-DIRECTORY is true, the file name may include
the forward slash character, which means that the returned file name may also
include directory names.  Normally, this function only looks for code files,
but if the optional argument ANY-EXTENSION is true, then a file name with any
extension will be returned."
  (let* ((local-file-name (if include-directory
                              shu-cpp-file-directory-name
                            shu-cpp-file-name))
         (target-extensions (regexp-opt shu-project-extensions t))
         (target-name (if any-extension
                          (concat local-file-name "+")
                        (concat local-file-name "*\\." target-extensions)))
         (target-char local-file-name)
         (numbers "[0-9]+")
         (bol (line-beginning-position))
         (eol (line-end-position))
         (file-name )     ;; This will contain the name or nil
         (line-number )   ;; Line number or nil
         (column-number ) ;; Column number or nil
         (ret-list )      ;; Returned list
         (line-col))
    (save-excursion
      (when (looking-at target-char) ;; Looking at a legal file name character
        (while (and (looking-at target-char) ;; Still on a file name char
                    (> (point) bol)) ;; And still on same line
          (backward-char 1))            ;; Keep moving back until we aren't on a file name char
        ;;  or we hit the beginning of the line
        (when (not (looking-at target-char)) ;; Moved backward past beginning of name
          (forward-char 1))             ;; Move forward to what might be the beginning
        (when (re-search-forward target-name eol t)
          (setq file-name (match-string 0)) ;; Have something that matches file name syntax
          (when (not (= (point) eol))
            (when (looking-at target-char)
              (setq file-name nil)))))
      (when file-name
        (setq line-col (shu-get-line-column-of-file))
        (setq ret-list (cons file-name line-col))))
    ret-list
    ))



;;
;;  shu-get-line-column-of-file
;;
(defun shu-get-line-column-of-file()
  "Fetch the potential line number and column number within a file.  On entry,
point is positioned at the character following a file name.  This file name
may be followed by a line number and the line number may be followed by a
column number.  This function recognizes four forms of line and column
specifications.

  thing.cpp:1234:42

indicates the file thing.cpp line number 1234, column 42

  [file=thing.cpp] [line=1234]

indicates the file thing.cpp line number 1234.

  \"thing.cpp\", line 55.16:

indicates the file thing.cpp line number 55, column 16.

  \"thing.cpp\", line 55:

indicates the file thing.cpp line number 55.

The purpose of this function is only to gather the line and column
specification following the file name.  The return value is a list, which is
empty if no line or column number was found.  It has only one element, which
is the line number if only a line number was found.  It has two elements,
which are the line number and column number if both line number and column
number were found.

This should probably be turned into a hook at some point so that other line
and column number indications may be used."
  (let ((numbers "[0-9]+")
        (line1-ss "\\[line\\s-*=\\s-*\\([0-9]+\\)\\]")
        (line2-ss ",\\s-*line\\s-*\\([0-9]+\\)")
        (eol (line-end-position))
        (line-number )  ;; Line number or nil
        (column-number ) ;; Column number or nil
        (ret-list ))     ;; Returned list
    (cond
     ((looking-at ":")
      (when (re-search-forward numbers eol t)
        (setq line-number (string-to-number (match-string 0))) ;; Line number within file
        (when (looking-at ":")  ;; Colon following line number
          (when (re-search-forward numbers eol t)
            (setq column-number (string-to-number (match-string 0)))))))
     ((looking-at "]")
      (when (re-search-forward line1-ss eol t)
        (setq line-number (string-to-number (match-string 1))))) ;; Line number within file
     ((or (looking-at "\"") (looking-at ","))
      (when (re-search-forward line2-ss eol t)
        (setq line-number (string-to-number (match-string 1))) ;; Line number within file
        (when (looking-at "\\.")  ;; Dot following line number
          (when (re-search-forward numbers eol t)
            (setq column-number (string-to-number (match-string 0))))))))
    (when line-number
      (setq ret-list (list line-number))
      (when column-number
        (when (< column-number 1)
          (setq column-number 1))
        (setq ret-list (append ret-list (list column-number)))))
    ret-list
    ))


;;
;;  shu-cpp-choose-file
;;
(defun shu-cpp-choose-file (assoc-result)
  "Choose the file to visit for a given unqualified name.  If there is
only one file associated with the name then visit it.  If there are
multiple files put all of the fully qualified file names in the completion
buffer and give the user the opportunity to select the desired file.  Then
visit that file."
  (let*
      ((c1 (cdr assoc-result))
       (full-name-list (car c1))
       (file-name (car full-name-list)))
    (if (= (length full-name-list) 1)
        (shu-cpp-visit-target file-name) ; visit the single file
                                        ; Ask user which one to visit
      (shu-cpp-resolve-choice full-name-list 'shu-cpp-visit-target))
    ))


;;
;;  shu-cpp-resolve-choice
;;
(defun shu-cpp-resolve-choice (full-name-list target)
  "Choose from a number of possible file names.
We have found an unqualified file name of interest but it resolves to multiple
fully qualified file names.  Display all of the possibilities in a completion
buffer and ask the user to choose the desired one.  The string containing the
chosen fully qualified file name will then be passed to the function pointed
to by target."
  (let ((shu-cpp-buffer (get-buffer-create shu-project-cpp-buffer-name)))
    (princ "\nshu-cpp-resolve-choice\n" shu-cpp-buffer)
    (princ full-name-list shu-cpp-buffer) (princ "\n\n" shu-cpp-buffer)
    (princ target shu-cpp-buffer) (princ "\n\n" shu-cpp-buffer)
    (ad-enable-advice 'choose-completion
                      'after 'shu-cpp-choose-completion)
    (ad-activate 'choose-completion)
    (ad-enable-advice 'mouse-choose-completion
                      'after 'shu-cpp-mouse-choose-completion)
    (ad-activate 'mouse-choose-completion)
    (setq shu-cpp-completion-target target)
    (setq shu-cpp-completion-current-buffer (current-buffer))
    (setq shu-cpp-completion-scratch (generate-new-buffer "*C Scratch*"))
    (set-buffer shu-cpp-completion-scratch)
    (with-output-to-temp-buffer "*C Completions*"
      (display-completion-list full-name-list))
    ))

;; At this point this function exits and control resumes at
;; the function shu-cpp-common-completion, which is called
;; by either shu-cpp-choose-completion or shu-cpp-mouse-choose-completion
;; when the user hits enter or clicks mouse button 2.



;;
;;  shu-cpp-choose-completion
;;
(defadvice choose-completion (after shu-cpp-choose-completion () disable)
  "Advice that runs after choose-completion to grab the users selected
choice out of the buffer in which choose-completion inserts it."
  (shu-cpp-common-completion)
  )

;;
;;  shu-cpp-mouse-choose-completion
;;
(defadvice mouse-choose-completion (after shu-cpp-mouse-choose-completion () disable)
  "Advice that runs after mouse-choose-completion to grab the users selected
choice out of the buffer in which mouse-choose-completion inserts it."
  (shu-cpp-common-completion)
  )

;;
;;  shu-cpp-common-completion
;;
(defun shu-cpp-common-completion ()
  "Called when the user hits enter or clicks mouse button 2 on completion window.
At this point the users selected choice is in the current buffer.  We get the
answer from the current buffer and call the function that is currently
pointed to by shu-cpp-completion-target."
  (let
      ((eol (save-excursion (end-of-line) (point)))
       (bol (save-excursion (beginning-of-line) (point)))
       (answer))
    (beginning-of-line)
    (setq answer (buffer-substring-no-properties bol eol))

    (ad-disable-advice 'choose-completion
                       'after 'shu-cpp-choose-completion)
    (ad-activate 'choose-completion)
    (ad-disable-advice 'mouse-choose-completion
                       'after 'shu-cpp-mouse-choose-completion)
    (ad-activate 'mouse-choose-completion)

    (delete-other-windows)  ; Get rid of completion window
    (kill-buffer shu-cpp-completion-scratch)
    (switch-to-buffer shu-cpp-completion-current-buffer)

    (funcall shu-cpp-completion-target answer)
    (setq shu-cpp-completion-target nil)
    ))



;;
;;  shu-cpp-visit-target
;;
(defun shu-cpp-visit-target (file-name)
  "This is the function that visits the file name chosen by vh() and perhaps
by a completing read from a completion buffer."
  (let
      ((num )
       (short-file-name (file-name-nondirectory file-name)))
    ;; If the file we are to visit is not the default proposed by vh(), then
    ;; forget any associated line number and column number
    (when (not (string= short-file-name shu-default-file-to-seek))
      (setq shu-cpp-target-file-line nil)
      (setq shu-cpp-target-file-column nil))
    (find-file file-name)
    (when shu-cpp-target-file-line
      (setq num shu-cpp-target-file-line)
      (goto-char (point-min))
      (forward-line (1- num))
      (shu-put-line-near-top)
      (when shu-cpp-target-file-column
        (move-to-column (1- shu-cpp-target-file-column))))
    (setq shu-cpp-target-file-line nil)
    ))


;;
;;  shu-count-c-project
;;
(defun shu-count-c-project ()
  "Count the number of lines of code in a project.  The final count is shown in
the minibuffer.  The counts of individual subdirectories are stored in the
temporary buffer *shu-project-count*"
  (interactive)
  (let
      ((pbuf    (get-buffer-create "*shu-project-count*"))
       (tlist    shu-cpp-project-list)
       (tdirs    0)
       (t-c-files   0)
       (t-h-files   0)
       (d-c-files   0)
       (d-h-files   0)
       (d-c-count   0)
       (d-h-count   0)
       (t-c-count   0)
       (t-h-count   0)
       (rlist)
       (dir-name))
    (if (not tlist)
        (progn
          (message "There is no project to count.")
          (ding))
      (princ "\n" pbuf)
      (princ " H Files     H Lines  C Files     C Lines   Directory\n" pbuf)
      (princ " -------     -------  -------     -------   ---------\n" pbuf)
      ;;     "12345678 12345678901 12345678 12345678901
      (while tlist
        (setq dir-name (car tlist))
        (setq rlist (shu-count-in-cpp-directory dir-name pbuf tdirs
                                                t-h-files t-c-files t-h-count t-c-count))
        (setq d-h-files (car rlist))
        (setq rlist  (cdr rlist))
        (setq d-c-files (car rlist))
        (setq rlist  (cdr rlist))
        (setq d-h-count (car rlist))
        (setq rlist  (cdr rlist))
        (setq d-c-count (car rlist))
        (setq rlist  (cdr rlist))
        (setq tdirs  (car rlist))
        (setq rlist  (cdr rlist))
        (setq t-h-files (car rlist))
        (setq rlist  (cdr rlist))
        (setq t-c-files (car rlist))
        (setq rlist  (cdr rlist))
        (setq t-h-count (car rlist))
        (setq rlist  (cdr rlist))
        (setq t-c-count (car rlist))

        (princ (format "%s %s %s %s:  %s\n"
                       (shu-fixed-format-num d-h-files 8)
                       (shu-fixed-format-num d-h-count 11)
                       (shu-fixed-format-num d-c-files 8)
                       (shu-fixed-format-num d-c-count 11)
                       dir-name) pbuf)
        (setq tlist (cdr tlist))))
    ))

;;
;;  shu-count-in-cpp-directory
;;
(defun shu-count-in-cpp-directory (directory-name pbuf tdirs
                                                  t-h-files t-c-files t-h-count t-c-count)
  "Count the lines of code in each of the code files in the given directory, updating
the message in the minibuffer and passing the totals back to the caller."
  (let*
      ((full-name "")
       (target-extensions (regexp-opt shu-project-extensions t))
       (target-name (concat "[^.]\\." target-extensions "$"))
       (directory-list (directory-files directory-name t target-name))
       (xf)
       (nbytes)
       (lcount)
       (pcount)
       (d-c-files    0)
       (d-h-files    0)
       (d-c-count    0)
       (d-h-count    0)
       (extension))
    (setq tdirs (1+ tdirs))
    (while directory-list
      (setq full-name (car directory-list))
      (setq extension (file-name-extension full-name))
      (if (member extension shu-cpp-c-extensions)
          (progn
            (setq d-c-files (1+ d-c-files))
            (setq t-c-files (1+ t-c-files)))
        (setq d-h-files (1+ d-h-files))
        (setq t-h-files (1+ t-h-files)))
      (with-temp-buffer
        (setq xf (insert-file-contents full-name))
        (setq nbytes (cadr xf))
        (setq lcount 0)
        (when (> nbytes 0)
          (setq lcount (count-lines (point-min) (point-max))))
        (if (member extension shu-cpp-c-extensions)
            (progn
              (setq d-c-count (+ d-c-count lcount))
              (setq t-c-count (+ t-c-count lcount)))
          (setq d-h-count (+ d-h-count lcount))
          (setq t-h-count (+ t-h-count lcount)))
        (message "Directories: %s, H files: %s, H lines: %s, C files: %s, C lines: %s, total lines: %s."
                 (shu-group-number tdirs 3)
                 (shu-group-number t-h-files 3)
                 (shu-group-number t-h-count 3)
                 (shu-group-number t-c-files 3)
                 (shu-group-number t-c-count 3)
                 (shu-group-number (+ t-h-count t-c-count) 3)))
      (setq directory-list (cdr directory-list)))
    (list d-h-files d-c-files d-h-count d-c-count tdirs
          t-h-files t-c-files t-h-count t-c-count)
    ))



;;
;;  shu-list-c-file-names
;;
(defun shu-list-c-file-names ()
  "Insert into the buffer a list of all of the unique file names in the project.
This does not include the file path as it will be different for duplicate
names."
  (interactive)
  (if (not shu-cpp-class-list)
      (progn
        (message "There is no project to list.")
        (ding))
    (shu-internal-list-c-file-names shu-cpp-class-list))
  )




;;
;;  shu-internal-list-c-file-names
;;
(defun shu-internal-list-c-file-names (proj-list)
  "Internal implementation function of SHU-LIST-C-FILE-NAMES."
  (let ((plist proj-list)
        (file-name)
        (full-name-list)
        (file-names))
    (while plist
      (shu-project-get-file-info plist file-name full-name-list)
      (push file-name file-names)
      (setq plist (cdr plist)))
    (setq file-names (sort file-names `string<))
    (insert (concat (shu-group-number (length file-names)) " unique file names:\n\n"))
    (while file-names
      (setq file-name (car file-names))
      (insert (concat file-name "\n"))
      (setq file-names (cdr file-names)))
    ))



;;
;;  shu-list-c-project
;;
(defun shu-list-c-project ()
  "Insert into the current buffer the names of all of the code files in the
current project."
  (interactive)
  (if (not shu-cpp-class-list)
      (progn
        (message "There is no project to list.")
        (ding))
    (shu-internal-list-c-project shu-cpp-class-list))
  )



;;
;;  shu-internal-list-c-project
;;
(defun shu-internal-list-c-project (proj-list)
  "Insert into the current buffer the names of all of the code files in the
project whose files are in PROJ-LIST."
  (let ((plist (shu-cpp-project-invert-list proj-list))
        (full-name))
    (insert (concat (shu-group-number (length plist)) " files:\n\n"))
    (while plist
      (setq full-name (car plist))
      (insert (concat full-name "\n"))
      (setq plist (cdr plist)))
    ))



;;
;;  shu-list-c-duplicates
;;
(defun shu-list-c-duplicates ()
  "Insert into the current buffer a list of all of the duplicate files names.
Under each duplicate file name, insert a list of the full paths to all of the
duplicates."
  (interactive)
  (if (not shu-cpp-class-list)
      (progn
        (message "There is no project to list.")
        (ding))
    (shu-internal-list-c-duplicates shu-cpp-short-list))
  )



;;
;;  shu-internal-list-c-duplicates
;;
(defun shu-internal-list-c-duplicates (proj-list)
  "Internal implementation of shu-list-c-duplicates."
  (let ((plist proj-list)
        (file-name)
        (dup-name)
        (flist)
        (full-name-list)
        (dlist)
        (proj-dups)
        (dups)
        (entry)
        (nl "")
        (name1)
        (name2))
    (while plist
      (shu-project-get-file-info plist file-name full-name-list)
      (when (> (length full-name-list) 1)
        (setq dlist (cons file-name full-name-list))
        (push dlist proj-dups))
      (setq plist (cdr plist)))
    (if (not proj-dups)
        (message "%s" "Project has no duplicates")
      (setq dlist (sort proj-dups
                        (lambda(lhs rhs)
                          (string< (car lhs) (car rhs)))))
      (setq proj-dups dlist)
      (insert (concat (shu-group-number (length dlist)) " duplicates:\n\n"))
      (while dlist
        (setq entry (car dlist))
        (setq file-name (car entry))
        (setq full-name-list (cdr entry))
        (setq flist (sort full-name-list 'string<))
        (insert (concat nl file-name "\n\n"))
        (setq nl "\n")
        (while flist
          (setq dup-name (car flist))
          (insert (concat "    " dup-name "\n"))
          (setq flist (cdr flist)))
        (setq dlist (cdr dlist)))
      (setq dlist proj-dups)
      (insert "\n")
      (while dlist
        (setq entry (car dlist))
        (setq full-name-list (cdr entry))
        (setq flist (sort full-name-list 'string<))
        (setq name1 (car flist))
        (setq flist (cdr flist))
        (while flist
          (setq name2 (car flist))
          (insert (concat "diff -b " name1 "  " name2 "\n"))
          (setq flist (cdr flist)))
        (setq dlist (cdr dlist))))
    ))



;;
;;  shu-list-c-prefixes
;;
(defun shu-list-c-prefixes ()
  "List all of the file prefixes found in the current project, if any.
See the doc-string for SHU-PROJECT-SPLIT-FILE-NAME for further information
about extracted file prefixes."
  (interactive)
  (let ((pl shu-cpp-prefix-list)
        (max-prefix 20)
        (np 0)
        (item)
        (prefix)
        (prefix-name)
        (count)
        (pad-length)
        (pad)
        (ess "es"))
    (if (not shu-cpp-class-list)
        (progn
          (message "There is no project in use.")
          (ding))
      (while pl
        (setq item (car pl))
        (setq prefix (car item))
        (setq np (1+ np))
        (when (> (length prefix) max-prefix)
          (setq max-prefix (length prefix)))
        (setq pl (cdr pl)))
      (if (not (> np 0))
          (progn
            (message "Current project has no prefixes.")
            (ding))
        (setq pl shu-cpp-prefix-list)
        (when (= 1 (length pl))
          (setq ess ""))
        (insert (concat (shu-group-number (length pl)) " prefix" ess "\n\n"))
        (insert
         (concat
          "prefix name               count\n"
          "-----------               -----\n"))
        (while pl
          (setq item (car pl))
          (setq prefix (car item))
          (if (= (length prefix) 0)
              (setq prefix-name "**none**")
            (setq prefix-name prefix))
          (setq count (cdr item))
          (setq pad "")
          (when (< (length prefix-name) max-prefix)
            (setq pad-length (- max-prefix (length prefix-name)))
            (setq pad (make-string pad-length ? )))
          (insert
           (concat
            prefix-name pad (shu-fixed-format-num count 11) "\n"))
          (setq pl (cdr pl)))))
    ))



;;
;;  shu-cpp-list-short-names
;;
(defun shu-cpp-list-short-names ()
  "List all of the short names in a project with the names of the files to
which they map."
  (interactive)
  (shu-cpp-internal-list-names shu-cpp-short-list "short names")
  )


;;
;;  shu-cpp-list-project-names
;;
(defun shu-cpp-list-project-names ()
  "List all of the names in a project with the names of the files to
which they map."
  (interactive)
  (shu-cpp-internal-list-names shu-cpp-class-list "project names")
  )



;;
;;  shu-cpp-list-completing-names
;;
(defun shu-cpp-list-completing-names ()
  "List all of the names that are used to do a completing read of a file name
along with the names of the actual files to which they map."
  (interactive)
  (shu-cpp-internal-list-names shu-cpp-completing-list "completing names")
  )



;;
;;  shu-cpp-internal-list-names
;;
(defun shu-cpp-internal-list-names (name-list type-name)
  "Implementation function for SHU-CPP-LIST-SHORT-NAMES,
SHU-CPP-LIST-PROJECT-NAMES, and SHU-CPP-LIST-COMPLETING-NAMES."
  (let ((sl (when name-list (copy-tree name-list)))
        (sll)
        (pos (point))
        (lpos)
        (pad)
        (pad1)
        (item)
        (name)
        (full-name)
        (full-names)
        (full-name-list)
        (name-count 0)
        (file-count 0)
        (max-len 0)
        (ncount)
        (pad-count)
        (header))
    (if (not shu-cpp-class-list)
        (progn
          (message "There is no project in use.")
          (ding))
      (if (not sl)
          (progn
            (message "Current project has no %s" type-name)
            (ding))
        (setq sll sl)
        (while sll
          (setq item (car sll))
          (setq name (car item))
          (when (> (length name) max-len)
            (setq max-len (length name)))
          (setq sll (cdr sll)))
        (setq max-len (+ max-len 2))
        (setq pad1 (make-string max-len ? ))
        (while sl
          (setq name-count (1+ name-count))
          (setq item (car sl))
          (setq name (car item))
          (setq full-name-list (cdr item))
          (insert (concat name ":"))
          (while full-name-list
            (setq full-names (car full-name-list))
            (setq ncount 1)
            (while full-names
              (setq full-name (car full-names))
              (if (/= ncount 1)
                  (setq pad pad1)
                (setq pad "")
                (when (< (length name) max-len)
                  (setq pad-count (1- (- max-len (length name))))
                  (setq pad (make-string pad-count ? ))))
              (insert (concat pad full-name "\n"))
              (setq file-count (1+ file-count))
              (setq ncount (1+ ncount))
              (setq full-names (cdr full-names)))
            (setq full-name (car full-names))
            (setq full-name-list (cdr full-name-list)))
          (setq sl (cdr sl)))
        (setq header (concat
                      (shu-group-number name-count)
                      " " type-name " map to "
                      (shu-group-number file-count)
                      " files:\n\n"))
        (setq lpos (+ (point) (length header)))
        (goto-char pos)
        (insert header))
      (goto-char lpos))
    ))


;;
;;  shu-list-c-directories
;;
(defun shu-list-c-directories ()
  "Insert into the current buffer the names of all of the directories in a project."
  (interactive)
  (let (
        (tlist    shu-cpp-project-list)
        (dir-name))
    (if (not tlist)
        (progn
          (message "There is no project to list.")
          (ding))
      (insert (concat (shu-group-number (length tlist)) " directories:\n\n"))
      (while tlist
        (setq dir-name (car tlist))
        (insert (concat dir-name "\n"))
        (setq tlist (cdr tlist))))
    ))

;;
;;  shu-list-in-cpp-directory
;;
(defun shu-list-in-cpp-directory (directory-name)
  "Insert into the current buffer the names of all of the code files in a directory."
  (let*
      ((full-name "")
       (target-extensions (regexp-opt shu-project-extensions t))
       (target-name (concat "[^.]\\." target-extensions "$"))
       (directory-list (directory-files directory-name t target-name))
       (extension))
    (while directory-list
      (setq full-name (car directory-list))
      (setq extension (file-name-extension full-name))
      (when (member extension shu-project-extensions)
        (insert (concat full-name "\n")))
      (setq directory-list (cdr directory-list)))
    ))




;;
;;  shu-list-c-all-project
;;
(defun shu-list-c-all-project ()
  "Insert into the current buffer everything that is known about the files,
prefixes, short names, long names, and duplicate names within the current
project."
  (interactive)
  (if (not shu-cpp-class-list)
      (progn
        (message "There is no project to list.")
        (ding))
    (shu-internal-list-c-all-project)))




;;
;;  shu-internal-list-c-all-project
;;
(defun shu-internal-list-c-all-project ()
  "The internal implementation function for SHU-LIST-C-ALL-PROJECT."
  (let ((pos (point))
        (prefix "*** ")
        (suffix " ***")
        (pad    "    "))
    (insert
     (concat
      "\n"
      "\n"
      prefix "list-c-prefixes" suffix "\n"
      pad    "(All prefixes removed to form short names)\n"
      "\n"))
    (shu-list-c-prefixes)
    (insert
     (concat
      "\n"
      "\n"
      prefix "list-c-directories" suffix "\n"
      pad    "(All directories in the project)\n"
      "\n"))
    (shu-list-c-directories)
    (insert
     (concat
      "\n"
      "\n"
      prefix "list-c-file-names" suffix "\n"
      pad    "(All unique file names within the project)\n"
      "\n"))
    (shu-list-c-file-names)
    (insert
     (concat
      "\n"
      "\n"
      prefix "list-c-project" suffix "\n"
      pad    "(Full path to all files in the project)\n"
      "\n"))
    (shu-list-c-project)
    (insert
     (concat
      "\n"
      "\n"
      prefix "list-short-names" suffix "\n"
      pad    "(All the short names and their corresponding full names)\n"
      "\n"))
    (shu-cpp-list-short-names)
    (insert
     (concat
      "\n"
      "\n"
      prefix "list-c-duplicates" suffix "\n"
      pad    "(Full path to all files whose short names are duplicates)\n"
      "\n"))
    (shu-list-c-duplicates)
    (insert
     (concat
      "\n"
      "\n"
      prefix "list-completing-names" suffix "\n"
      pad    "(All recognized file names of all forms)\n"
      "\n"))
    (shu-cpp-list-completing-names)
    (goto-char pos)
    ))



;;
;;  shu-which-c-project
;;
(defun shu-which-c-project ()
  "Identify the current project by putting into a project buffer the name of the file
from which the project was derived as well as the name of all of the directories in the
project.  Then switch to that buffer.  The idea is to invoke this function, look at the
results in that buffer, and then quit out of the buffer."
  (interactive)
  (let
      ((pbuf    (get-buffer-create "*current c project*"))
       (tlist    shu-cpp-project-list)
       (dir-name))
    (shu-internal-which-c-project pbuf)
    ))

;;
;;  shu-internal-which-c-project
;;
(defun shu-internal-which-c-project (pbuf)
  (let
      ((tlist    shu-cpp-project-list)
       (dir-name))
    (if (not tlist)
        (progn
          (message "There is no current project.")
          (ding))
      (princ (concat shu-cpp-project-file ":\n------------\n") pbuf)
      (when shu-cpp-project-name
        (princ (concat "Project name: " shu-cpp-project-name "\n") pbuf))
      (princ (format-time-string "Set on %a, %e %b %Y at %k:%M:%S." shu-cpp-project-time) pbuf)
      (princ "\n\n" pbuf)
      (while tlist
        (setq dir-name (car tlist))
        (princ (concat dir-name "\n") (get-buffer pbuf))
        (setq tlist (cdr tlist)))
      (switch-to-buffer pbuf))
    ))




;;
;;  shu-record-visited-project
;;
(defun shu-record-visited-project (name proj-dir)
  "Record a project visit in the file \"~/visited-projects.log\"."
  (let ((fnow (format-time-string "%Y-%m-%d-%T-(%a)"))
        (line))
    (setq line (concat fnow ": " name "  " proj-dir "\n"))
    (write-region line nil "~/visited-projects.log" 'append)
    ))



;;
;;  shu-setup-project-and-tags
;;
(defun shu-setup-project-and-tags (proj-dir)
  "Visit a project file, make a C project from the contents of the whole file,
create a file called \"files.txt\" with the name of every file found, invoke
ctags on that file to build a new tags file, and then visit the tags file.
PROJ-DIR is the name of the directory in which the project file exists and in
which the tags file is to be built.
Record the visit in the file \"~/visited-projects.log\"."
  (let
      ((tags-add-tables nil)
       (gbuf (get-buffer-create "*setup project*"))
       (debug-on-error t)
       (large-file-warning-threshold 64000000)
       (proj-file (concat proj-dir "/project.txt"))
       (files-file (concat proj-dir "/files.txt"))
       (tags-file (concat proj-dir "/TAGS"))
       (stime (current-time))
       (elapsed)
       (estring)
       (sstring)
       (etime)
       (c-count)
       (h-count)
       (pname (shu-get-real-this-command-name)))
    (setq shu-cpp-project-name pname)
    (setq sstring (format-time-string "on %a, %e %b %Y at %k:%M:%S" stime))
    (princ (format "\nStart project (%s) setup in %s %s.\n\n" shu-cpp-project-name proj-dir sstring) gbuf)
    (find-file proj-file)
    (shu-internal-set-c-project (point-min) (point-max))
    (setq elapsed (time-since stime))
    (kill-buffer (current-buffer))
    (setq sstring (format-time-string "%M:%S.%3N" elapsed))
    (setq c-count (shu-group-number shu-cpp-c-file-count 3))
    (setq h-count (shu-group-number shu-cpp-h-file-count 3))
    (princ (format "%s C files and %s H files found in %s\n" c-count
                   h-count
                   sstring) gbuf)
    (shell-command (concat "rm " files-file))
    (find-file files-file)
    (delete-region (point-min) (point-max))
    (list-c-project)
    (basic-save-buffer)
    (kill-buffer (current-buffer))
    (shell-command (concat "rm " tags-file))
    (setq stime (current-time))
    (shell-command (concat "ctags -e -L " files-file " -f " tags-file))
    (visit-tags-table-buffer tags-file)
    (setq elapsed (time-since stime))
    (setq sstring (format-time-string "%M:%S.%3N" elapsed))
    (princ (format "TAGS file built in %s.\n" sstring) gbuf)
    (setq etime (current-time))
    (setq sstring (format-time-string "on %a, %e %b %Y at %k:%M:%S" etime))
    (princ (format "\n  End project setup in %s %s.\n" proj-dir sstring) gbuf)
    (princ "\nThe current project is:\n\n" gbuf)
    (shu-internal-which-c-project gbuf)
    (shu-record-visited-project pname proj-dir)
    (switch-to-buffer gbuf)
    ))



;;
;;  shu-get-real-this-command-name
;;
(defun shu-get-real-this-command-name ()
  "Return the symbol name of the variable \"real-this-command\" if it is defined.
If not defined, return the string \"**unknown**\".  Some older versions of emacs
do not support real-this-command."
  (if (version< emacs-version "24.3.1") "**unknown**" (symbol-name real-this-command))
  )



;;
;;  shu-visit-project-and-tags
;;
(defun shu-visit-project-and-tags (proj-dir)
  "Visit a project file, make a C project from the contents of the whole file,
and load that tags table from the tags file in the specified directory.  This
function uses the existing tags table, whereas SHU-SETUP-PROJECT-AND-TAGS
creates a new tags table."
  (let
      ((tags-add-tables nil)
       (gbuf (get-buffer-create "*setup project*"))
       (debug-on-error t)
       (large-file-warning-threshold 64000000)
       (proj-file (concat proj-dir "/project.txt"))
       (files-file (concat proj-dir "/files.txt"))
       (tags-file (concat proj-dir "/TAGS"))
       (stime (current-time))
       (elapsed)
       (estring)
       (sstring)
       (etime)
       (c-count)
       (h-count)
       (pname))
    (setq pname (shu-get-real-this-command-name))
    (setq shu-cpp-project-name pname)
    (setq sstring (format-time-string "on %a, %e %b %Y at %k:%M:%S" stime))
    (princ (format "\nStart project (%s) setup in %s %s.\n\n" shu-cpp-project-name proj-dir sstring) gbuf)
    (find-file proj-file)
    (shu-internal-set-c-project (point-min) (point-max))
    (setq elapsed (time-since stime))
    (kill-buffer (current-buffer))
    (setq sstring (format-time-string "%M:%S.%3N" elapsed))
    (setq c-count (shu-group-number shu-cpp-c-file-count 3))
    (setq h-count (shu-group-number shu-cpp-h-file-count 3))
    (princ (format "%s C files and %s H files found in %s\n" c-count
                   h-count
                   sstring) gbuf)
    (setq stime (current-time))
    (visit-tags-table-buffer tags-file)
    (setq elapsed (time-since stime))
    (setq sstring (format-time-string "%M:%S.%3N" elapsed))
    (princ (format "TAGS file loaded in %s.\n" sstring) gbuf)
    (setq etime (current-time))
    (setq sstring (format-time-string "on %a, %e %b %Y at %k:%M:%S" etime))
    (princ (format "\n  End project visit in %s %s.\n" proj-dir sstring) gbuf)
    (princ "\nThe current project is:\n\n" gbuf)
    (shu-internal-which-c-project gbuf)
    (switch-to-buffer gbuf)
    ))


;;
;;  shu-global-operation
;;
(defun shu-global-operation (documentation function-to-call
                                           &optional search-target replace)
  "Invoke a function on every file in the project.
documentation is the string to put in the buffer to describe the operation."
  (let ((gbuf      (get-buffer-create shu-global-buffer-name))
        (spoint)
        (tlist     shu-project-file-list)
        (file)
        (fbuf)
        (file-buf)
        (stime (current-time))
        (etime)
        (sstring)
        (estring)
        (fcount 0)
        (ccount 0))
    (save-excursion
      (setq spoint (with-current-buffer gbuf (point)))
      (setq sstring (format-time-string "on %a, %e %b %Y at %k:%M:%S" stime))
      (print (concat "***start " documentation " at " sstring) (get-buffer gbuf))
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
        (when (buffer-modified-p)
          (setq ccount (1+ ccount))
          (basic-save-buffer))
        (when (not fbuf)  ; We created the file buffer
          (kill-buffer file-buf))
        (setq fcount (1+ fcount))
        (setq tlist (cdr tlist)))
      (setq etime (current-time))
      (setq estring (format-time-string "on %a, %e %b %Y at %k:%M:%S" etime))
      (print (concat "***end " documentation " at " estring "\n") gbuf)
      (princ (format "%d files changed of %d files scanned.\n" ccount fcount) gbuf))
    (switch-to-buffer gbuf)
    (goto-char spoint)
    ))




;;
;;  shu-global-search-replace
;;
(defun shu-global-search-replace (file argument1 argument2)
  "This function is called once for each file in the project.  The first
argument is the file name.  The second argument is a list holding lists of
search and replace operations.  Each search and replace operation is defined by
a list of arguments as follows:

     1. A boolean value, t means case sensitive search, nil means ignore case
     2. The function to call to do the search.  This must be
        'search-forward, 're-search-forward, or any function with the same
        signature and behavior.
     3. The string that is the target of the search
     4. The string the is to replace the target whenever found
     5. An optional second argument to be passed to replace-match
        The default value is t
     6. An optional third argument to be passed to replace-match
        The default value is t

For example

     (list
       (list t 'search-forward \"Mumble\" \"Bumble\")
       (list nil 'search-forward \"howdy\" \"doody\"))

is a list that defines two search and replace operations.  Both operations use
the search-forward function.  The first is a case sensitive search and replace to
replace all instances of \"Mumble\" with \"Bumble\".  The second is a case
insensitive search and replace to replace all instances of \"howdy\" with \"doody\".

These operations may be performed on every file in the project as follows:

     (setq ops
       (list
         (list t 'search-forward \"Mumble\" \"Bumble\")
         (list nil 'search-forward \"howdy\" \"doody\")))
     (setq doc \"Description of change\")
     (shu-global-operation doc 'shu-global-search-replace ops)"
  (let ((gb (get-buffer-create shu-global-buffer-name))
        (rlist)
        (tlist)
        (case-sensitive)
        (search-function)
        (target)
        (replacment)
        (rm1 t)
        (rm2 t)
        (debug-on-error t))
    (princ (concat "\n    IN: " file "\n") gb)
    (setq tlist argument1)
    (save-excursion
      (save-restriction
        (widen)
        (while tlist
          (setq rlist (car tlist))
          (setq case-sensitive (pop rlist))
          (setq search-function (pop rlist))
          (setq target (pop rlist))
          (setq replacment (pop rlist))
          (when (cdr rlist)
            (setq rm1 (pop rlist)))
          (when (cdr rlist)
            (setq rm2 (pop rlist)))
          (goto-char (point-min))
          (setq case-fold-search (not case-sensitive))
          (while (funcall search-function target nil t)
            (princ (concat file ":" (number-to-string (line-number-at-pos (match-beginning 0) t))
                           ":\nFound \"" (match-string 0) "\"\n") gb)
            (princ (concat "Replacing with: \"" replacment "\"\n") gb)
            (replace-match replacment rm1 rm2))
          (setq tlist (cdr tlist)))))
    ))


;;
;;  shu-cpp-project-get-list-counts
;;
(defun shu-cpp-project-get-list-counts (proj-list)
  "PROJ-LIST is an alist whose structure is identical to that of SHU-CPP-CLASS-LIST.
This function returns a list with three items on it: the number of c / cpp files, the
number of h files, and the number of duplicate names found in the list."
  (let ((plist proj-list)
        (c-count   0)
        (h-count   0)
        (dup-count 0)
        (item)
        (file-name)
        (extension)
        (flist)
        (full-name-list))
    (while plist
      (shu-project-get-file-info plist file-name full-name-list)
      (setq extension (file-name-extension file-name))
      (if (member extension shu-cpp-c-extensions)
          (setq c-count (+ c-count (length full-name-list)))
        (setq h-count (+ h-count (length full-name-list))))
      (when (> (length full-name-list) 1)
        (setq dup-count (1+ dup-count)))
      (setq plist (cdr plist)))
    (list c-count h-count dup-count)
    ))



;;
;;  shu-cpp-project-collapse-list
;;
(defun shu-cpp-project-collapse-list (key-list)
  "KEY-LIST is an alist in which the cdr of each item is the unqualified file name
and the car of each item is the fully qualified file name, including the path to
the file.  The output is a different alist in which the car of each item is the
unqualified file name and the cdr of each item is the list of fully qualified
file names to which the unqualified file name refers.

For example, if KEY-LIST contains:

    (\"xxx_mumble.h\" . \"/foo/bar/xxx_mumble.h\")
    (\"xxx_stumble.h . \"/foo/bar/xxx_stumble..h\")
    (\"xxx_stumble.h . \"/boo/baz/xxx_stumble..h\")

then the returned list will contain

    (\"xxx_mumble.h\" . \"/foo/bar/xxx_mumble.h\")
    (\"xxx_stumble.h . \"/foo/bar/xxx_stumble..h\" \"/boo/baz/xxx_stumble..h\")"
  (let ((klist (copy-tree key-list))
        (ilist)
        (c1)
        (file-name)
        (full-name)
        (full-name-list)
        (limit)
        (nname)
        (item)
        (rlist))
    (setq ilist (sort klist
                      (lambda(obj1 obj2)
                        (string< (car obj1) (car obj2)))))

    ;;
    ;; Go through the sorted list, merging the information
    ;; for duplicate names.
    ;; The car of each item on the list is the unqualified file name.
    ;; The cdr of each item on the list is a list of all of the fully
    ;; qualified file names that share the same unqualified name.
    ;;
    (while ilist
      (setq c1 (car ilist))
      (setq file-name (car c1))
      (setq full-name (cdr c1))
      (setq full-name-list (list full-name))
      (setq limit ilist)
      (while limit
        (setq ilist (cdr ilist))
        (setq limit ilist)            ; terminate loop if list is done
        (when ilist                   ; have at least one more entry
          (setq c1 (car ilist))
          (setq nname (car c1))
          (setq full-name (cdr c1))
          (if (string= file-name nname)     ; Name remains the same
              (progn
                (setq full-name-list (cons full-name full-name-list))
                )
            (setq limit nil))
          )
        )
      ;; Now have all properties for current name
      (when (> (length full-name-list) 1)
        (setq full-name-list (delete-dups full-name-list))
        (when (> (length full-name-list) 1)
          (setq full-name-list (sort full-name-list 'string<))))
      (setq item (cons file-name (list full-name-list)))
      (setq rlist (cons item rlist)))
    (nreverse rlist)
    ))


;;
;;  shu-cpp-project-invert-list
;;
(defun shu-cpp-project-invert-list (proj-list)
  "PROJ-LIST is an alist in which the cdr of each item is the unqualified file name
and the car of each item is the list of fully qualified file names to which
the unqualified name refers.  The returned output is a single list of fully
qualified file names."
  (let ((plist proj-list)
        (file-list)
        (file-name)
        (full-name)
        (full-name-list))
    (while plist
      (shu-project-get-file-info plist file-name full-name-list)
      (while full-name-list
        (setq full-name (car full-name-list))
        (setq file-list (cons full-name file-list))
        (setq full-name-list (cdr full-name-list)))
      (setq plist (cdr plist)))
    (sort file-list 'string<)
    ))




;;
;;  shu-project-split-file-name
;;
(defun shu-project-split-file-name (file-name)
  "Split FILE-NAME into two parts.  The first part is the prefix and the second
part is the short name.  The rules for splitting are as follows:

If the name has no underscores, then the prefix is empty and the short name is
the whole name.

If the name has one underscore in it (e.g., \"abcdef_mumble.cpp\"), then the
prefix is the part before the underscore (\"abcdef\") and the short name is all
of the rest (\"mumble.cpp\").

If the name has more than one underscore in it (e.g., \"x_abcdef_mumble.cpp\"),
then we look as the length of the first part (\"x\").  If its length is one,
then the prefix is the concatenation of the first two parts (\"x_abcdef\"), and
the short name is the rest (\"mumble.cpp\").  If the length of the first part is
not one (e.g., file name is \"lovely_looking_mumble.cpp\"), then the prefix is
the first part (\"lovely\") and the short name is the rest
(\"looking_mumble.cpp\").

After the split, the short name is converted to all lower case.

Return a cons cell of the form (prefix . short-name)"
  (let ((underscore (regexp-quote "_"))
        (pos -1)
        (nlist)
        (pfx "")
        (np)
        (short))
    (setq nlist (split-string file-name underscore t))
    (setq np (length nlist))
    (cond
     ((= 1 np) ;; No underscores in name
      (setq pfx "")
      (setq short file-name))
     ((= 2 np) ;; One underscore in name
      (setq pfx (car nlist))
      (setq short (cadr nlist)))
     (t        ;; At least two underscores in name
      (if shu-cpp-project-very-short-names
          (progn
            (setq short (car (last nlist)))
            (setq nlist (nbutlast nlist))
            (setq pfx (string-join nlist "_")))
        (setq pfx (car nlist))
        (setq nlist (cdr nlist))
        (when (= 1 (length pfx))
          (setq pfx (concat pfx "_" (car nlist)))
          (setq nlist (cdr nlist)))
        (setq short (car nlist))
        (setq nlist (cdr nlist))
        (while nlist
          (setq short (concat short "_" (car nlist)))
          (setq nlist (cdr nlist))))))
    (setq short (downcase short))
    (cons pfx short)
    ))



;;
;;  shu-project-make-short-key-list
;;
(defun shu-project-make-short-key-list (key-list)
  "KEY-LIST is an alist in which the car of each item is the unqualified file
name and the cdr of each item is the fully qualified file name, including the
path to the file.  This function creates two lists.  One is an alist of all of
the file prefixes.  That car of each item is the prefix.  The cdr of each item
is the number of times that prefix was found.  The second is a list similar to
KEY-LIST with all of the file names changed to their equivalent short names.  If
the long and short names are the same, then that item is omitted from the new
list of short names.

Return is a cons cell whose car is the prefix list and whose cdr is the short
name list."
  (let ((kl (copy-tree key-list))
        (file-name)
        (full-name-list)
        (ps)
        (prefix)
        (short-name)
        (short-keys)
        (item)
        (short-list)
        (prefix-list)
        (x)
        (count))
    (while kl
      (setq item (car kl))
      (setq file-name (car item))
      (setq full-name-list (cdr item))
      (setq ps (shu-project-split-file-name file-name))
      (setq prefix (car ps))
      (setq short-name (cdr ps))
      (when (not (string= short-name file-name))
        (setq item (cons short-name full-name-list))
        (setq short-list (cons item short-list))
        (setq item (cons prefix 1))
        (if (not prefix-list)
            (setq prefix-list (cons item prefix-list))
          (setq x (assoc prefix prefix-list))
          (if (not x)
              (setq prefix-list (cons item prefix-list))
            (setq count (cdr x))
            (setq count (1+ count))
            (setcdr x count))))
      (setq kl (cdr kl)))
    (setq short-list (sort short-list (lambda(obj1 obj2)
                                        (string< (car obj1) (car obj2)))))
    (setq prefix-list (sort prefix-list (lambda(obj1 obj2)
                                          (string< (car obj1) (car obj2)))))
    (cons prefix-list short-list)
    ))


;;
;;  shu-other
;;
(defun shu-other ()
  "Visit an h file from a c file or a c file from an h file If visiting a .h
file, invoke this function and you will be taken to the .c or .cpp file.  If
visiting a .c or .cpp file, invoke this function and you will be taken to the
corresponding .h file.  This function will use a project if one is active.
Otherwise, it will assume that all files reside in the same directory."
  (interactive)
  (let ((ext       (file-name-extension (buffer-file-name))))
    (if (string= ext "h")
        (shu-cother)
      (shu-hother))
    ))


;;
;;  shu-cother
;;
(defun shu-cother ()
  "Visit a .cpp file from the corresponding .t.cpp or .h file.  If visiting a
t.cpp or .h file, invoke this function and you will be taken to the
corresponding .cpp or .c file.  This function will use a project if one is
active.  Otherwise, it will assume that all files reside in the same directory."
  (interactive)
  (shu-project-some-other ".cpp")
  )


;;
;;  shu-hother
;;
(defun shu-hother ()
  "Visit a .h file from the corresponding .cpp or t.cpp file.  If visiting a
.cpp or t.cpp file, invoke this function and you will be taken to the
corresponding .h file.  This function will use a project if one is active.
Otherwise, it will assume that all files reside in the same directory."
  (interactive)
  (shu-project-some-other ".h")
  )


;;
;;  shu-iother
;;
(defun shu-iother ()
  "Visit a i.cpp file from the corresponding .cpp or .h file.  If visiting a .c
or .cpp file, invoke this function and you will be taken to the corresponding
.i.cpp file.  This function will use a project if one is active.  Otherwise, it
will assume that all files reside in the same directory."
  (interactive)
  (shu-project-some-other ".i.cpp")
  )


;;
;;  shu-tother
;;
(defun shu-tother ()
  "Visit a t.cpp file from the corresponding .cpp or .h file.  If visiting a .c
or .cpp file, invoke this function and you will be taken to the corresponding
.t.cpp file.  This function will use a project if one is active.  Otherwise, it
will assume that all files reside in the same directory."
  (interactive)
  (shu-project-some-other ".t.cpp")
  )



;;
;;  shu-project-some-other
;;
(defun shu-project-some-other (extension)
  "Visit a related file within a project.  If you are in a \".h\" file and you
wish to go to the corresponding \".t.cpp\" file, this function will form the new
file name and then look it up within the project to find the location of the
related file and then visit that file, if possible."
  (let ((base-name (shu-cpp-project-get-base-name))
        (newfile))
    (when (shu-validate-file-in-project)
      (setq newfile (concat base-name extension))
      (when (not (shu-cpp-choose-other-file newfile))
        (message "Cannot '%s' file for %s" (file-name-nondirectory newfile) base-name)))
    ))



;;
;;  shu-validate-file-in-project
;;
(defun shu-validate-file-in-project ()
  "Validate that the current file is a member of the current project.

If you are in a project and then visit some file in another directory tree whose
FILE-NAME-NONDIRECTORY matches a file name in the current project, an attempt to
visit a related file by using the project primitives will take you back into the
project tree, when you might have thought that you were going to some other part
of the directory tree you were in.

For example, suppose you are in a project file \"a/b/src/foo/foo.h\" and you
then visit another file in \"x/y/src/foo/foo.h.\" If you then try to go to the
unit test file for \"x/y/src/foo/foo.h,\" instead of visiting
\"x/y/test/unit/foo.t.cpp,\" you will, instead, silently be taken to
\"a/b/test/unit/foo.t.cpp,\" which is not the unit test file for
\"x/y/src/foo/foo.h.\"

When visiting related files within a project, this function verifies that the
current file is actually a member of the current project.  If there exists a
project and the current file is not a member of that project, you are probably
about to be taken silently to the wrong file."
  (let ((local-name (buffer-file-name))
        (plist)
        (file-in-project))
    (if (not shu-cpp-class-list)
        (setq file-in-project t)
      (setq plist (shu-cpp-get-inverted-class-list))
      (if (member local-name plist)
          (setq file-in-project t)
        (ding)
        (message "%s" "current file is not in current project")))
    file-in-project
    ))



;;
;;  shu-cpp-project-get-base-name
;;
(defun shu-cpp-project-get-base-name ()
  "When visiting a file within a project, the name might consist of two or three
parts - the file name, the normal extension, such as .h or .cpp, and the
intermediate extension, such as .i or .t when visiting .i.cpp or .t.cpp.  This
function gets the buffer file name and removes the one or two extensions in
order to get the name with no extensions at all."
  (interactive)
  (let ((base-name (buffer-file-name)))
    (shu-cpp-project-extract-base-name base-name)
    ))



;;
;;  shu-cpp-project-extract-base-name
;;
(defun shu-cpp-project-extract-base-name (name)
  "This is the implementation function of SHU-CPP-PROJECT-GET-BASE-NAME so that
the logic of the function can be unit tested."
  (let ((base-name (file-name-sans-extension name)))
    (if (string= (file-name-extension base-name) "t")
        (setq base-name (file-name-sans-extension base-name))
      (when (string= (file-name-extension base-name) "i")
        (setq base-name (file-name-sans-extension base-name))))
    base-name
    ))



;;
;;  shu-cpp-choose-other-file
;;
(defun shu-cpp-choose-other-file (newfile)
  "NEWFILE is a fully qualified file name that has been formed by changing the
file suffix, perhaps from .cpp to .h or .h to .t.cpp.  We want to try to open
the file either in the current directory or in the project.

Up until 5 February 2022, we first looked for the file in the project, then in
the local directory.  But if two .h files were duplicate names and two .cpp
files were duplicate names, a request to visit the \"other\" file would bring up
a choice of the duplicate names.  It seems logical that if you are in a .h file
and you want to visit the associated .cpp file, the one that you want to visit
is the one in the current directory.

As of 5 February 2022, we first look for the file in the current directory and
then in the project.  The local variable LOCAL-DIRECTORY-FIRST can be used to
invert that choice and return to the original behavior of first looking in the
project for the file.

If a file was found and visited, return true."
  (let ((found)
        (local-directory-first t))
    (if local-directory-first
        (setq found (shu-cpp-project-visit-prefer-local newfile))
      (setq found (shu-cpp-project-visit-prefer-project newfile)))
    found
    ))



;;
;;  shu-cpp-project-visit-prefer-local
;;
(defun shu-cpp-project-visit-prefer-local (newfile)
  "NEWFILE is a fully qualified file name that has been formed by changing the
file suffix, perhaps from .cpp to .h or .h to .t.cpp.  We want to try to open
the file either in the current directory or in the project.

First try to visit the file in the local directory.  If not found in the local
directory, try to find and visit it within the project.

If a file was found and visited, return true."
  (let ((nfile (file-name-nondirectory newfile))
        (found))
    (if (not (file-readable-p newfile))
        (setq found (shu-cpp-choose-project-file nfile))
      (setq found t)
      (find-file newfile))
    found
    ))



;;
;;  shu-cpp-project-visit-prefer-project
;;
(defun shu-cpp-project-visit-prefer-project (newfile)
  "NEWFILE is a fully qualified file name that has been formed by changing the
file suffix, perhaps from .cpp to .h or .h to .t.cpp.  We want to try to open
the file either in the current directory or in the project.

First try to visit the file in the project.  If not found in the project, try to
visit the file in the local directory.

If a file was found and visited, return true."
  (let ((nfile (file-name-nondirectory newfile))
        (found))
    (setq found (shu-cpp-choose-project-file nfile))
    (when (and (not found)
               (file-readable-p newfile))
      (setq found t)
      (find-file newfile))
    found
    ))



;;
;;  shu-cpp-choose-project-file
;;
(defun shu-cpp-choose-project-file (newfile)
  "Try to visit a file within a project.  If a project is in use, try to visit
the given file in the list of files that belong to the project.  This goes
through the standard project selection process, including prompting the user to
choose the desired file if more than one file with the same name exists.  If a
file was found and visited, return true."
  (let ((tfile)
        (found))
    (when shu-cpp-class-list
      (setq tfile (assoc newfile shu-cpp-class-list))
      (when tfile
        (setq found t)
        (shu-cpp-choose-file tfile)))
    found
    ))


;;
;;  shu-cpp-get-inverted-class-list
;;
(defun shu-cpp-get-inverted-class-list ()
  "Return SHU-CPP-INVERTED-CLASS-LIST, creating it if it is currently nil."
  (when (not shu-cpp-inverted-class-list)
    (setq shu-cpp-inverted-class-list (shu-cpp-project-invert-list shu-cpp-class-list)))
  shu-cpp-inverted-class-list
  )



;;
;;  shu-project-get-specific-files
;;
(defun shu-project-get-specific-files (root pattern cpp-type)
  "Starting at ROOT, search for all files that meet the following criteria:

      - If PATTERN is non-nil, it is a regular expression that the file name
        must match.  If PATTERN is nil, then the file names are ignored and all
        files are potentially returned.

      - If CPP-TYPE is nil, then file type is ignored and all files are
        potentially returned subject to constraints of PATTERN described above.

If PATTERN and CPP-TYPE are both nil, then all files are returned.

If you wish to find all files that are of type C++, then set PATTERN to nil and
CPP-TYPE to t.

If you wish to find all files with similar names that are of type C++, then set
CPP-TYPE to t and PATTERN to some regular expression that will filter the files
appropriately, such as \"mumble_bar*\".

The directories excluded by project search are also excluded here.  There are
typically CMake directories that are populated with temporary files.  See
SHU-PROJECT-EXCLUDE-LIST for a list of excluded directories.

The files are returned as a list of files with paths relative to ROOT."
  (let ((level 0)
        (flist)
        (dir-name))
    (shu-project-make-exclude-hash)
    (setq shu-cpp-fetch-list nil)
    (shu-project-sub-specific-files root level pattern cpp-type)
    (setq shu-cpp-fetch-list (sort shu-cpp-fetch-list 'string<))
    shu-cpp-fetch-list
    ))




;;
;;  shu-project-sub-specific-files
;;
(defun shu-project-sub-specific-files (dir-name level pattern cpp-type)
  "Recursively search for all files as described in
SHU-PROJECT-GET-SPECIFIC-FILES.

Two lists are accumulated.  DIR-LIST is a list of all directories encountered.
This function calls itself recursively to search all of them for the desired
files.

The global variable SHU-CPP-FETCH-LIST is used to accumulate the list of
qualified files found."
  (let ((dlist)
        (tlist)
        (cname)
        (sname)
        (dname)
        (dir-list)
        (got-interest)
        (extension))
    (setq dlist (directory-files dir-name t nil t))
    (setq tlist dlist)
    (while tlist
      (setq cname (car tlist))
      (setq sname (file-name-nondirectory cname))
      (if (file-directory-p cname)
          (progn
            (unless (or (string= sname  ".")
                        (string= sname ".."))
              (unless (shu-project-directory-is-excluded sname)
                (push cname dir-list))))
        (when (and (shu-cpp-project-is-type-wanted sname cpp-type)
                   (shu-project-file-pattern-match sname pattern))
          (push (file-relative-name cname) shu-cpp-fetch-list)))
      (setq tlist (cdr tlist)))
    (while dir-list
      (setq dname (car dir-list))
      (shu-project-sub-specific-files dname (1+ level) pattern cpp-type)
      (setq dir-list (cdr dir-list)))
    ))


;;
;;  shu-cpp-project-set-alias
;;
(defun shu-cpp-project-set-alias ()
  "Set the common alias names for the functions in shu-cpp-project.
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'set-prefix 'shu-set-prefix)
  (defalias 'set-dir-prefix 'shu-set-dir-prefix)
  (defalias 'clear-prefix 'shu-clear-prefix)
  (defalias 'make-p-project 'shu-make-p-project)
  (defalias 'make-c-project 'shu-make-c-project)
  (defalias 'make-full-c-project 'shu-make-full-c-project)
  (defalias 'set-p-project 'shu-set-p-project)
  (defalias 'set-c-project 'shu-set-c-project)
  (defalias 'renew-c-project 'shu-renew-c-project)
  (defalias 'clear-c-project 'shu-clear-c-project)
  (defalias 'count-c-project 'shu-count-c-project)
  (defalias 'list-c-duplicates 'shu-list-c-duplicates)
  (defalias 'list-c-file-names 'shu-list-c-file-names)
  (defalias 'list-c-project 'shu-list-c-project)
  (defalias 'list-c-prefixes 'shu-list-c-prefixes)
  (defalias 'list-short-names 'shu-cpp-list-short-names)
  (defalias 'list-project-names 'shu-cpp-list-project-names)
  (defalias 'list-completing-names 'shu-cpp-list-completing-names)
  (defalias 'list-c-directories 'shu-list-c-directories)
  (defalias 'list-c-all-project 'shu-list-c-all-project)
  (defalias 'which-c-project 'shu-which-c-project)
  (defalias 'vf 'shu-vf)
  (defalias 'other 'shu-other)
  (defalias 'cother 'shu-cother)
  (defalias 'hother 'shu-hother)
  (defalias 'iother 'shu-iother)
  (defalias 'tother 'shu-tother)
  )

(provide 'shu-cpp-project)

;;; shu-cpp-project.el ends here
