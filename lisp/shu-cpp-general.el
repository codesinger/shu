;;; shu-cpp-general.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-cpp-general
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

;; A collection of useful functions for dealing with C++ code.
;;
;; ## Selected highlights ##
;;
;; Here are some useful features of this package.
;;
;; ### Dealing with long string constants ###
;;
;; If you copy strings of text into string constants in your program, you may end up
;; with some very long lines.  SHU-CSPLIT can automatically split such a line
;; for you.  SHU-CUNSPLIT can undo the split.  SHU-CREPLACE can in one
;; operation, replace a split line with a different string constant.
;;

;;; Code:


(provide 'shu-cpp-general)
(require 'shu-base)
(require 'shu-cpp-token)


(defconst shu-cpp-base-types
  (list
   "bool"
   "char"
   "double"
   "enum"
   "float"
   "int"
   "int16_t"
   "int32_t"
   "int64_t"
   "long"
   "short"
   "signed"
   "std::size_t"
   "uint16_t"
   "uint32_t"
   "uint64_t"
   "unsigned")
  "A list of all of the base types in C and C++.  This may be modified by shu-add-cpp-base-types")

(defvar shu-cpp-member-prefix "_"
  "The character string that is used as the prefix to member variables of a C++ class.
This is used by shu-internal-get-set when generating getters and setters for a class.")

(defvar shu-is-const nil
  "Set true if the C++ data member we are working is declared to be const.")

(defvar shu-nc-vtype nil
  "Set true if the C++ data member we are working is declared to be non-const.")

(defvar shu-var-name nil
  "The variable name that corresponds to an attribute name.")

(defvar shu-attr-name nil
  "The name of an attribute.")

(defvar shu-lc-comment nil
  "Comment string with the first letter downcased.")

(defvar shu-rmv-classes nil
  "An alist of \"using namespace\" directives and their line numbers where first declared.
Used to filter duplicates.")

;;
;;  Functions for customzing
;;


;;
;;  shu-add-cpp-base-types
;;
(defun shu-add-cpp-base-types (ntypes)
  "Add one or more data types to the list of C++ native data types defined in shu-cpp-base-types
in shu-cpp-general.el.  Argument may be a single type in a string or a list of strings.
This modifies shu-cpp-base-types."
  (let ((nt ntypes))
    (when (not (listp nt))
      (setq nt (list nt)))
    (setq shu-cpp-base-types (append shu-cpp-base-types nt))
    ))



;;
;;  shu-cpp1-class
;;
(defun shu-cpp1-class (class-name)
  "Place a skeleton class definition in the current buffer at point."
  (interactive "*sClass name?: ")
  (let ((debug-on-error t))
    (insert (concat
             "class " class-name "\n"
             "{\n"
             "public:\n\n"
             "  " class-name "()\n"
             "   { }\n\n"
             "private:\n\n"
             "  " class-name "(\n"
             "    const " class-name "   &rhs);\n\n"
             "  " class-name " &operator=(\n"
             "    const " class-name "   &rhs);\n\n"
             "};"))
    (search-backward "{" nil t)
    (forward-char 2)
    ))


;;
;;  shu-cpp2-class
;;
(defun shu-cpp2-class (class-name)
  "Place a skeleton class definition in the current buffer at point."
  (interactive "*sClass name?: ")
  (shu-internal-cpp2-class class-name)
  )


;;
;;  shu-new-c-class
;;
(defun shu-new-c-class ()
  "Place a skeleton class definition in the current buffer at point."
  (interactive)
  (let ((debug-on-error t)
        (hfile (file-name-nondirectory (buffer-file-name)))
        (hfile-base (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (shu-internal-cpp2-class hfile-base)
    ))

;;
;;  shu-internal-cpp2-class
;;
(defun shu-internal-cpp2-class (class-name)
  "Place a skeleton class definition in the current buffer at point."
  (let ((debug-on-error t)
        (comment-start  36)
        (comment-end    78)
        (comment-length  0)
        (ostream        "std::ostream")
        (otry           nil)
        (opt-length     22)
        (class-arg      nil)
        (arg-length      0)
        (pad            "")
        (pad-after      "  ")
        (pad-count      0)
        (comment1       "!< The stream into which we stream")
        (comment2       "!< The object to be streamed"))

    (when (boundp 's-comment-start)
      (setq comment-start s-comment-start))
    (when (boundp 's-comment-end)
      (setq comment-end s-comment-end))
    (setq comment-length (- comment-end comment-start 3))

                                        ; std::ostream                &os
                                        ; const required_item_thing   &cn
                                        ; 123456789012345678901234567890

    (setq class-arg (concat "const " class-name))
    (setq arg-length (max (length class-arg) (length ostream)))
    (when (< arg-length opt-length)
      (setq pad-after (concat pad-after " ")))
    (setq class-arg (concat class-arg pad-after))
    (setq ostream (concat ostream pad-after))
    (setq arg-length (max (length class-arg) (length ostream)))
    (setq class-arg (shu-make-padded-line class-arg arg-length))
    (setq ostream (shu-make-padded-line ostream arg-length))

    (setq pad-count (- comment-start (length ostream) 9))
    (if (> pad-count 0)
        (setq pad (make-string pad-count ? ))
                                        ;
      (setq comment-length (+ comment-length pad-count)))
    (setq comment1 (shu-make-padded-line comment1 comment-length))
    (setq comment2 (shu-make-padded-line comment2 comment-length))

                                        ;  (when (< (length ostream) (length class-arg))
                                        ;    (setq class-arg (shu-make-padded-line class-arg (length ostream))))

    (insert (concat
             "/*!\n"
             " * \\brief Description of " class-name "\n"
             " */\n"
             "class " class-name "\n"
             "{\n"
             "public:\n\n"
             "  " "/*!\n"
             "  " " * \\brief Standard constructor\n"
             "  " " */\n"
             "  explicit " class-name "()\n"
             "  { }\n\n\n"
             "  " "/*!\n"
             "  " " *  \\brief Stream object out to a stream\n"
             "  " " *\n"
             "  " " * \\return The same stream as the input to allow for chained operators.\n"
             "  " " */\n"
             "  friend std::ostream &operator<<(\n"
             "    " ostream "&os," pad "/*" comment1 "*/\n"
             "    " class-arg "&cn)" pad "/*" comment2 "*/\n"
             "  {\n"
             "    return cn.print_self(os);\n"
             "  }\n\n\n"
             "private:\n\n"
             "  " "/*!\n"
             "  " " * \\brief The copy constructor is deliberately private and unimplemented.\n"
             "  " " *\n"
             "  " " * \\param rhs the object from which we are to be constructed\n"
             "  " " */\n"
             "  " class-name "(\n"
             "    const " class-name "   &rhs);\n\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator=() is deliberately private and unimplemented.\n"
             "  " " *\n"
             "  " " * \\param rhs the object from which we are to be assigned\n"
             "  " " *\n"
             "  " " * \\return reference to self to allow for chained operators\n"
             "  " " */\n"
             "  " class-name " &operator=(\n"
             "    const " class-name "   &rhs);\n\n\n"
             "  " "/*!\n"
             "  " " * \\brief This is the implementation function for operator<<()\n"
             "  " " *\n"
             "  " " * \\return The same stream as the input to allow for chained operators.\n"
             "  " " */\n"
             "  std::ostream &print_self(\n"
             "    std::ostream    &os)           /*!< The stream into which we stream     */\n"
             "  const\n"
             "  {\n"
             "    os << \"" class-name "\";\n\n"
             "    return os;\n"
             "  }\n\n\n"
             "};"))
    (search-backward "class " nil t)
    (search-backward "brief" nil t)
    (end-of-line)
    ))

;;
;;  shu-operators
;;
;;  Put skeletons for all of the operator functions into a header file
;;
(defun shu-operators (class-name)
  "Place skeletons of all of the standard c++ operator functions at point."
  (interactive "*sClass name?: ")
  (let ((debug-on-error t)
        (comment-start  36)
        (comment-end    78)
        (comment-length  0)
        (ostream        "std::ostream")
        (otry           nil)
        (opt-length     22)
        (class-arg      nil)
        (arg-length      0)
        (pad            "")
        (pad-after      "  ")
        (pad-count      0)
        (str   )
        (comment1       "!< The stream into which we stream")
        (comment2       "!< The object to be streamed"))

    (when (boundp 's-comment-start)
      (setq comment-start s-comment-start))
    (when (boundp 's-comment-end)
      (setq comment-end s-comment-end))
    (setq comment-length (- comment-end comment-start 3))

                                        ; std::ostream                &os
                                        ; const required_item_thing   &cn
                                        ; 123456789012345678901234567890

    (setq class-arg (concat "const " class-name))
    (setq arg-length (max (length class-arg) (length ostream)))
    (when (< arg-length opt-length)
      (setq pad-after (concat pad-after " ")))
    (setq class-arg (concat class-arg pad-after))
    (setq ostream (concat ostream pad-after))
    (setq arg-length (max (length class-arg) (length ostream)))
    (setq class-arg (shu-make-padded-line class-arg arg-length))
    (setq ostream (shu-make-padded-line ostream arg-length))

    (setq pad-count (- comment-start (length ostream) 9))
    (if (> pad-count 0)
        (setq pad (make-string pad-count ? ))
                                        ;
      (setq comment-length (+ comment-length pad-count)))
    (setq comment1 (shu-make-padded-line comment1 comment-length))
    (setq comment2 (shu-make-padded-line comment2 comment-length))

                                        ;  (when (< (length ostream) (length class-arg))
                                        ;    (setq class-arg (shu-make-padded-line class-arg (length ostream))))
    (setq str (concat "  " "  const " class-name "   &rhs"))
    (setq pad-count (- comment-start (length str) 2 ))
    (when (> pad-count 0)
      (setq pad (make-string pad-count ? )))
    (insert (concat
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator+()\n"
             "  " " *\n"
             "  " " * \\return a reference to ourself to allow chained operators\n"
             "  " " */\n"
             "  " class-name " &operator+(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to add           */\n"
             "  {\n"
             "\n"
             "    return *this;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator+=()\n"
             "  " " *\n"
             "  " " * \\return a reference to ourself to allow chained operators\n"
             "  " " */\n"
             "  " class-name " &operator+=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to add           */\n"
             "  {\n"
             "\n"
             "    return *this;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator-()\n"
             "  " " *\n"
             "  " " * \\return a reference to ourself to allow chained operators\n"
             "  " " */\n"
             "  " class-name " &operator-(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to subtract      */\n"
             "  {\n"
             "\n"
             "    return *this;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator-=()\n"
             "  " " *\n"
             "  " " * \\return a reference to ourself to allow chained operators\n"
             "  " " */\n"
             "  " class-name " &operator-=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to subtract      */\n"
             "  {\n"
             "\n"
             "    return *this;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator<()\n"
             "  " " *\n"
             "  " " * \\return true if this object is less than the right hand one\n"
             "  " " */\n"
             "  " " bool  operator<(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator<=()\n"
             "  " " *\n"
             "  " " * \\return true if this object is less than or equal to the right hand one\n"
             "  " " */\n"
             "  " " bool  operator<=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator>()\n"
             "  " " *\n"
             "  " " * \\return true if this object is greater than the right hand one\n"
             "  " " */\n"
             "  " " bool  operator>(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator>=()\n"
             "  " " *\n"
             "  " " * \\return true if this object is greater than or equal to the right hand one\n"
             "  " " */\n"
             "  " " bool  operator>=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator==()\n"
             "  " " *\n"
             "  " " * \\return true if this object is equal to the right hand one\n"
             "  " " */\n"
             "  " " bool  operator==(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = true;\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             "\n\n"
             "  " "/*!\n"
             "  " " * \\brief operator!=()\n"
             "  " " *\n"
             "  " " * \\return true if this object is not equal to the right hand one\n"
             "  " " */\n"
             "  " " bool  operator!=(\n"
             "  " "  const " class-name "   &rhs)" pad "/*!< Right hand operand to compare       */\n"
             "  {\n"
             "    const bool is_it = !operator==(rhs);\n"
             "\n"
             "    return is_it;\n"
             "  }\n"
             ))

    (search-backward "class " nil t)
    (search-backward "brief" nil t)
    (end-of-line)
    ))


;;
;;  shu-set-author
;;
(defun shu-set-author (name)
  "Set the author name to be placed in generated C++ classes."
  (interactive)
  (setq shu-cpp-author name))


;;
;;  shu-set-default-global-namespace
;;
(defun shu-set-default-global-namespace (name)
  "Set the global namespace for C++ classes."
  (interactive "sName?: ")
  (setq shu-cpp-default-global-namespace name))


;;
;;  shu-set-default-namespace
;;
(defun shu-set-default-namespace (name)
  "Set the local namespace for C++ classes."
  (interactive "sName?: ")
  (setq shu-cpp-default-namespace name))


;;
;;  shu-dox-hdr
;;
(defun shu-dox-hdr ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \n"
             pad " */"
             ))
    (search-backward "*/")
    (search-backward "*")
    (forward-char 2)
    ))

;;
;;  shu-dox-brief
;;
(defun shu-dox-brief ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \\brief \n"
             pad " */"
             ))
    (search-backward "*/")
    (search-backward "*")
    (search-forward "brief")
    (forward-char 1)
    ))

;;
;;  shu-dox2-hdr
;;
(defun shu-dox2-hdr ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \\brief \n"
             pad " * \n"
             pad " */"
             ))
    (search-backward "brief")
    (search-forward " ")
    ))

;;
;;  shu-dcc
;;
(defun shu-dcc ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \\brief The copy constructor is deliberately private and unimplemented.\n"
             pad " */"
             ))
    ))

;;
;;  shu-dce
;;
(defun shu-dce ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * \\brief operator=() is deliberately private and unimplemented.\n"
             pad " */"
             ))
    ))

;;
;;  shu-clc
;;
(defun shu-clc ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * The constructor acquires the latch.\n"
             pad " */"
             ))
    ))

;;
;;  shu-drc
;;
(defun shu-drc ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let ((debug-on-error t)
        (ccol           (current-column))
        (pad-count      (current-column))
        (pad            nil))
                                        ;  (when (> ccol 1)
                                        ;    (setq pad-count (- ccol 1)))
    (setq pad (make-string pad-count ? ))
    (beginning-of-line)
    (insert (concat
             pad "\n"
             pad "/*!\n"
             pad " * The destructor releases the latch.\n"
             pad " */"
             ))
    ))



;;
;;  shu-binclude
;;
(defun shu-binclude ()
  "If point is sitting on something that resembles a fully qualified class name,
use the standard BDE algorithm to turn the class name into the name of an
include file.  The standard BDE algorithm replaces the :: between namespace and
class name with an underscore, makes all letters lower case, and appends \".h\"
to the end of the name.

Thus \"abcdef::MumbleFrotz\" becomes \"abcdef_mumblefrotz.h\".

An include directive for the file is then created and put into the kill ring for
a subsequent yank.

The file name is delimited by double quotes unless SHU-CPP-INCLUDE-USER-BRACKETS
variable is true, in which case the file name is delimited by left and right
angle brackets.

Return true if a class name was found an an include generated.  This is for the
benefit of unit tests."
  (interactive)
  (let* ((bol (line-beginning-position))
        (eol (line-end-position))
        (target-list (append shu-cpp-name-list (list ":")))
        (target-char (regexp-opt target-list nil))
        (target-name
         (concat
          "\\(" shu-cpp-name "+\\)"
          "::"
          "\\(" shu-cpp-name "+\\)"))
        (namespace)
        (class-name)
        (file-name)
        (include-name)
        (left-delim (if shu-cpp-include-user-brackets "<" "\""))
        (right-delim (if shu-cpp-include-user-brackets ">" "\""))
        (got-it))
    (save-excursion
      (if (not (looking-at target-char)) ;; Looking at a legal class name character
          (message "%s" "Not a properly formed class name")
        (while (and (looking-at target-char) ;; Still on a file name char
                    (> (point) bol)) ;; And still on same line
          (backward-char 1))            ;; Keep moving back until we aren't on a file name char
        ;;  or we hit the beginning of the line
        (when (not (looking-at target-char)) ;; Moved backward past beginning of name
          (forward-char 1))             ;; Move forward to what might be the beginning
        (if (not (re-search-forward target-name eol t))
            (message "%s" "Not a properly formed class name")
          (setq namespace (match-string 1)) ;; Have something that matches file name syntax
          (setq class-name (match-string 2))
          (setq file-name (concat
                           (downcase namespace) "_"
                           (downcase class-name) ".h"))
          (setq include-name (concat
                              "#include "
                              left-delim file-name right-delim))
          (message "%s" include-name)
          (shu-kill-new include-name)
          (setq got-it t))))
    got-it
    ))



;;
;;  shu-ginclude
;;
(defun shu-ginclude()
  "While in a file buffer, wrap the file name in a C++ include directive and
put it in the kill ring.  The file name is delimited by double quotes unless
SHU-CPP-INCLUDE-USER-BRACKETS variable is true, in which case the file name
is delimited by left and right angle brackets."
  (interactive)
  (let ((name  (file-name-nondirectory (buffer-file-name)))
        (ext)
        (left-delim (if shu-cpp-include-user-brackets
                   "<"
                 "\""))
        (right-delim (if shu-cpp-include-user-brackets
                   ">"
                   "\""))
        (incl))
    (if (not name)
        (ding)
      (setq ext (file-name-extension name))
      (if (not (string= "H" (upcase ext)))
          (progn
            (ding)
            (message "%s" "THIS IS NOT A HEADER FILE"))
      (setq incl (concat "#include "
                         left-delim name right-delim "\n"))
      (shu-kill-new incl)
      (message "%s" incl)))
    ))


;;
;; shu-dox-cvt
;;
(defun shu-dox-cvt ()
  "Convert a section of comments delimited by // into Doxygen format."
  (interactive)
  (let ((debug-on-error t)
        (pad            nil)
        (eol            nil)
        (spos           nil)
        (slash-pos      nil)
        (not-done       t))
    (beginning-of-line)
    (setq eol (save-excursion (forward-line 1) (end-of-line) (point)))
    (setq spos (search-forward "//" eol t))
    (if (not spos)
        (error "Unable to find any // near here")
      (progn
        (setq slash-pos (- (current-column) 2))
        (when (< slash-pos 0)
          (setq slash-pos 0))
        (setq pad (make-string slash-pos ? ))
        (forward-line -1)
        (insert (concat "\n" pad "/*!"))
        (while not-done
          (forward-line 1)
          (setq eol (save-excursion (end-of-line) (point)))
          (if (search-forward "//" eol t)
              (replace-match " *")
            (progn
              (setq not-done nil)
              (insert (concat pad " */\n")))))))
    ))


;;
;; shu-dox-cbt
;;
(defun shu-dox-cbt ()
  "Convert a section of comments delimited by //! into Doxygen brief format."
  (interactive)
  (let ((bol (save-excursion (beginning-of-line) (point)))
        (pad  )
        (eol  )
        (spos )
        (slash-pos )
        (not-done       t)
        (j              0))
    (beginning-of-line)
    (setq eol (save-excursion (forward-line 1) (end-of-line) (point)))
    (setq spos (search-forward "//!" eol t))
    (if (not spos)
        (error "Unable to find any //! near here")
      (progn
        (setq slash-pos (- (current-column) 2))
        (when (< slash-pos 1)
          (setq slash-pos 1))
        (setq pad (make-string (1- slash-pos) ? ))
        (replace-match (concat "/*!\n" pad " * \\brief") t t)
        (beginning-of-line)
        (end-of-line)
        (insert (concat "\n" pad " */"))
        (forward-line -1)
        (end-of-line)))
    ))


;;
;;  shu-new-x-file
;;
(defun shu-new-x-file ()
  "Generate a skeleton Doxygen \\file directive."
  (interactive)
  (let ((xfile (file-name-nondirectory (buffer-file-name))))
    (insert (concat
             "\n"
             "/*!\n"
             "  \\file " xfile "\n"
             "\n"
             "    \\brief Contains the implementation fuctions of BloombergLP::m_moalrt::\n"
             "*/\n"))
    ))


;;
;;  shu-new-h-file
;;
(defun shu-new-h-file ()
  "Generate a skeleton header file for C or C++ file."
  (interactive)
  (let ((debug-on-error t)
        (comment-start  36)
        (comment-end    78)
        (comment-length nil)
        (comment-length nil)
        (pad-count      nil)
        (pad            nil)
        (ecomment       nil)
        (tcomment       nil)
        (hfile (file-name-nondirectory (buffer-file-name)))
        (hfile-base (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
        (file-line  "/*  FILE_NAME: ")
        (slength        nil)
        (incl-name      nil))
    (when (boundp 's-comment-start)
      (setq comment-start s-comment-start))
    (when (boundp 's-comment-end)
      (setq comment-end s-comment-end))
    (setq comment-length (- comment-end comment-start 4))


    (setq pad-count (- comment-end 4))
    (setq slength (- comment-end 2))
    (setq pad (make-string pad-count ? ))
    (setq ecomment (concat "/*" pad "*/\n"))
    (setq pad (make-string pad-count ?*))
    (setq tcomment (concat "/*" pad "*/\n"))
    (setq file-line (concat file-line hfile))
    (setq pad-count (- comment-end 2 (length file-line)))
    (setq pad (make-string pad-count ? ))
    (setq file-line (concat file-line pad "*/\n"))
    (setq incl-name (concat hfile-base "_h_included"))
    (insert (concat "#ifndef " incl-name "\n"))
    (insert (concat "#define " incl-name " 1\n"))
    (insert "\n")
    (insert "/*!\n")
    (insert (concat " * \\file " hfile "\n"))
    (insert " *\n")
    (insert (concat " * \\brief Contains the definition of " hfile-base "\n"))
    (insert " *\n")
    (insert " * \\author " shu-cpp-author " \n")
    (insert " */\n")
    (insert "\n")
    (insert "#include <iostream>\n")
    (insert "\n")
    (insert "\n")
    (insert "\n")
    (insert (concat (shu-make-padded-line "#endif" (1- comment-start)) "/* "))
    (insert (concat (shu-make-padded-line incl-name comment-length) "*/\n"))
    (forward-line -3)
    ))

;;
;;  shu-new-c-file
;;
(defun shu-new-c-file ()
  "Generate a skeleton code file for a C or C++ file."
  (interactive)
  (let ((debug-on-error t)
        (comment-start  36)
        (comment-end    78)
        (comment-length nil)
        (comment-length nil)
        (pad-count      nil)
        (pad            nil)
        (ecomment       nil)
        (tcomment       nil)
        (hfile (file-name-nondirectory (buffer-file-name)))
        (hfile-base (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
        (file-line  "/*  FILE_NAME: ")
        (slength        nil)
        (incl-name      nil)
        (base-name (file-name-sans-extension (buffer-file-name))))
    (setq incl-name (concat hfile-base ".h"))
    (when (boundp 's-comment-start)
      (setq comment-start s-comment-start))
    (when (boundp 's-comment-end)
      (setq comment-end s-comment-end))
    (setq comment-length (- comment-end comment-start 4))


    (setq pad-count (- comment-end 4))
    (setq slength (- comment-end 2))
    (setq pad (make-string pad-count ? ))
    (setq ecomment (concat "/*" pad "*/\n"))
    (setq pad (make-string pad-count ?*))
    (setq tcomment (concat "/*" pad "*/\n"))
    (setq file-line (concat file-line hfile))
    (setq pad-count (- comment-end 2 (length file-line)))
    (setq pad (make-string pad-count ? ))
    (setq file-line (concat file-line pad "*/\n"))
    (insert "\n")
    (insert "/*!\n")
    (insert (concat " * \\file " hfile "\n"))
    (insert " *\n")
    (insert (concat " * \\brief Contains the implementation of " hfile-base "\n"))
    (insert " *\n")
    (insert " * \\author " shu-cpp-author "\n")
    (insert " */\n")
    (insert "\n")
    (insert (concat "#include \"" incl-name "\""))
    (insert "\n")
    (insert "\nusing namespace ::std;\n")
    ;;  (forward-line -3)
    ))

;;
;;  shu-author
;;
(defun shu-author ()
  "Insert the doxygen author tag in an existing file."
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (insert " *\n")
  (insert " * \\author " shu-cpp-author "\n")
  )

;;
;;  shu-make-padded-line
;;
(defun shu-make-padded-line (line tlen)
  "Add sufficient spaces to make LINE the length TLEN."
  (let ((clen       (length line))
        (pad        "")
        (pad-count  nil))
    (setq pad-count (- tlen clen))
    (when (> pad-count 0)
      (setq pad (make-string pad-count ? ))  )
    (concat line pad)
    ))

;;
;;  shu-getters
;;
(defun shu-getters (start end)
  "Mark a region in a file that contains C++ instance variable declarations.
This function will create get and set functions for all of the instance
variables."
  (interactive "r")                 ; bounds of marked region (start end)
  (save-excursion
    (let ((sline (shu-the-line-at start)) ; Remember start line
          (eline (shu-the-line-at end))   ; Remember end line
          (line-diff 0)               ; The difference between the number of lines we
                                        ;  tried to move and the number we actually moved
          (eol       nil)
          (dir-name  nil)
          ;;        (shu-cpp-buffer (get-buffer-create shu-cpp-buffer-name))
          (m (copy-marker 300 ) ) )
      (setq shu-cpp-project-list nil)
      (goto-char start)               ; Move to region start
                                        ; While we have not reached last line and
      (while (and (<= (shu-current-line) eline) (= line-diff 0)) ; there are more lines
        (setq eol (save-excursion (end-of-line) (point)))
        (when (> eol (point))
          (get-set)
          )
        (setq line-diff (forward-line 1))
        )
      )
    )
  )

;;
;;  shu-get-set - Emit getters and setters for an instance variable
;;
(defun shu-get-set ()
  "Generate get and set functions for an instance variable in a C++ class.
Position the cursor ahead of the Doxygen comment above the variable.  The get
and set functions will be placed in the buffer *get-set*."
  (interactive)
  (let ((non-white1 "[^[:space:]\n\t\v\f]")
        (non-white2 "[^[:space:]]")
        (cstart )        ;; Comment start position
        (cend )          ;; Comment end position
        (comment "")     ;; Original comment
        (shu-lc-comment "")  ;; Comment with first letter downcased
        )
    (when (re-search-forward non-white1 nil t)
      (backward-char 1)
      (when (looking-at "//!")
        (forward-char 3)
        (re-search-forward non-white2 nil t)
        (setq cstart (1- (point)))
        (setq cend (save-excursion (end-of-line) (point)))
        (setq comment (buffer-substring cstart cend))
        (setq cstart (substring comment 0 1))
        (setq cstart (downcase cstart))
        (setq shu-lc-comment (concat cstart (substring comment 1)))
        (forward-line 1)
        )
      (shu-internal-get-set comment shu-lc-comment)
      )
    ))


;;
;;  shu-internal-get-set - Emit getters and setters for a instance variable
;;
;; This code assumes that the declaration fits entirely on the current line.  It
;; first scans the line to find the terminating semi-colon.  Then is scans backwards
;; to find the variable name and anything of interest in front of it.  The things
;; that might be in front of it are:
;;
;; *      - This is a pointer
;; &      - This is a reference
;; *const - This is a const pointer
;;
;; Spaces are allowed where one might expect.  The following declarations are fine:
;;
;;  foo * const _bar;
;;  foo  _bar;
;;  foo   *   _bar;
;;  foo    &_bar;
;;
(defun shu-internal-get-set (comment shu-lc-comment)
  "Generate get and set functions for an instance variable in a C++ class."
  (interactive)
  (let (
        ;;    (gbuf      (get-buffer-create shu-unit-test-buffer))
        (gs-buf    (get-buffer-create "*get-set*"))
        (cbuf (current-buffer))
        (bol (save-excursion (beginning-of-line) (point)))
        (eol (save-excursion (end-of-line) (point)))
        (got-semi )   ; True if we found semi-colon at end of declaration
        (name-exp (concat "\\s-*" shu-cpp-name))
        (op (regexp-opt (list "*" "&") nil))
        (op-or-space (regexp-opt (list " " "*" "&") nil))
        (cnx "\\*+\\s-*const\\s-*")
        (name-end )   ; Point at end of attribute name
        (name-start ) ; Point at start of attribute name
        (shu-attr-name )  ; Name of attribute
        (shu-var-name )   ; Corresponding variable name
        (found-op )   ; Set to "*" or "&" if this is a pointer or reference
        (vtype )      ; Data type of attribute
        (shu-nc-vtype )   ; Non-const data type
        (non-blank "[^ \t]")
        (vlist )
        (tlist )
        (tblank )
        (nc-tblank )
        (tword )
        (is-base-type ) ; True if base type (int, long, double, etc)
        (shu-is-const )     ; True if declared to be const
        (shu-is-const-ptr ) ; True if a const pointer (*const x;)
        (mpfx   shu-cpp-member-prefix)
        (mpfx-len (length shu-cpp-member-prefix))
        (debug-on-error t))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward non-blank bol t)
        (setq bol (point)))
      (setq got-semi (search-forward ";" eol t))
      (if (not got-semi)
          (error "%s" "No terminating semi-colon found.")
        (backward-char 1)
        (re-search-backward name-exp bol t)
        (setq name-end (1+ (point)))
        (re-search-backward op-or-space bol t) ;; Look for beginning of name
        (setq name-start (1+ (point)))
        (setq shu-attr-name (buffer-substring name-start name-end))
        (setq shu-var-name shu-attr-name)
        (when (string= (substring shu-attr-name 0 mpfx-len) mpfx)
          (setq shu-var-name (substring shu-attr-name mpfx-len)))
        (if (looking-at op) ;; Name immediately preceded by "*" or "&"
            (progn
              (setq found-op (char-after (point)))
              (re-search-backward non-blank bol t)
              (setq eol (1+ (point)))
              )
          ;; Name immediately preceded by space
          (if (re-search-backward cnx bol t)
              (progn  ;; This is a const pointer (*const x;)
                (setq found-op ?*)
                (setq shu-is-const-ptr t)
                (when (re-search-backward non-blank bol t)
                  (setq eol (1+ (point))))
                )
            (when (re-search-backward non-blank bol t)
              (if (not (looking-at op))
                  (setq eol (1+ (point)))
                (setq found-op (char-after (point)))
                (when (re-search-backward non-blank bol t)
                  (setq eol (1+ (point))))
                )
              )
            )
          )
        (when found-op
          (setq found-op (char-to-string found-op)))
        ;; Fetch the data type and split it into a list of words (eg const unsigned int)
        (setq vtype (buffer-substring bol eol))
        (setq vlist (split-string vtype "[ \t]+" t))
        ;;      (princ vlist (get-buffer gbuf))
        (setq vtype nil)
        (setq tblank "")
        (setq nc-tblank "")
        (setq tlist vlist)
        ;; Check to see if it is const and whether it is a base type (int, long, etc)
        (while tlist
          (setq tword (car tlist))
          (when (string= tword "const")
            (setq shu-is-const t))
          (when (member tword shu-cpp-base-types)
            (setq is-base-type t))
          (setq tlist (cdr tlist))
          )
        ;; Turn the list of words back into a type declaration
        (setq tlist vlist)
        (while tlist
          (setq tword (car tlist))
          (when  (not (string= tword "const"))
            (setq shu-nc-vtype (concat shu-nc-vtype nc-tblank tword))
            (setq nc-tblank " "))
          (setq vtype (concat vtype tblank tword))
          (setq tblank " ")
          (setq tlist (cdr tlist))
          )
                                        ;      (message "shu-attr-name = \"%s\", shu-var-name = \"%s\", found-op = %s, vtype = \"%s\", shu-nc-vtype = \"%s\"" shu-attr-name shu-var-name found-op vtype shu-nc-vtype)
        (switch-to-buffer (get-buffer gs-buf))
        (goto-char (point-max))
        (cond
         ((string= found-op "&")
          (shu-return-ref)
          )
         ((string= found-op "*")
          (shu-return-ptr)
          (when (not shu-is-const-ptr)
            (shu-set-ptr))
          )
         (t ;; No operator at all
          (if is-base-type
              (progn
                (shu-emit-get)
                (insert (concat "  " shu-nc-vtype  "  " shu-var-name "() const\n"
                                "    {\n        return " shu-attr-name ";\n    }\n"))
                (shu-emit-set shu-var-name)
                (insert (concat
                         "  void set_" shu-var-name "(\n"
                         "    " vtype "   " shu-var-name ")\n"
                         "    {\n    "        shu-attr-name " = " shu-var-name ";\n    }\n"))
                ) ;; is-base-type
            ;; Not a base type, return a reference to it
            (shu-return-ref)
            (shu-set-obj)
            )
          )
         )
        (switch-to-buffer (get-buffer cbuf))
        )
      ) ; save-excursion
    ))

;;
;;  shu-return-ref  -  Return a reference to an instance variable
;;
(defun shu-return-ref ()
  (when (not shu-is-const)
    (shu-emit-get)
    (insert (concat "    " shu-nc-vtype "  &" shu-var-name "()\n"
                    "    {\n        return " shu-attr-name ";\n    }\n"))
    )
  (shu-emit-get)
  (insert (concat "    const " shu-nc-vtype "  &" shu-var-name "() const\n"
                  "    {\n        return " shu-attr-name ";\n    }\n"))
  )

;;
;;  shu-set-obj
;;
(defun shu-set-obj ()
  (shu-emit-set shu-var-name)
  (insert (concat
           "    void set_" shu-var-name "(\n"
           "        const " shu-nc-vtype "   &" shu-var-name ")\n"
           "    {\n    "        shu-attr-name " = " shu-var-name ";\n    }\n"))

  )

;;
;;  shu-return-ptr - Return a reference to something pointed to by an instance variable
;;                   This part emits the function declaration
;;
(defun shu-return-ptr ()
  (when (not shu-is-const)
    (shu-emit-get)
    (insert (concat "    " shu-nc-vtype "  &" shu-var-name "()\n"))
    (shu-gen-return-ptr)
    )
  (shu-emit-get)
  (insert (concat "    const " shu-nc-vtype "  &" shu-var-name "() const\n"))
  (shu-gen-return-ptr)
  )

;;
;;  shu-set-ptr
;;
(defun shu-set-ptr ()
  (shu-emit-set shu-var-name)
  (insert (concat
           "    void set_" shu-var-name "(\n"
           "        const " shu-nc-vtype "   &" shu-var-name ")\n"
           "    {\n    "        shu-attr-name " = &" shu-var-name ";\n    }\n"))

  )

;;
;;  shu-gen-return-ptr - Return a reference to something pointed to by an instance variable
;;                       This part emits the body of the function
;;
(defun shu-gen-return-ptr ()
  (insert (concat "    {\n"
                  "        assert(" shu-attr-name " != 0);\n"
                  "        return *" shu-attr-name ";\n"
                  "    }\n"))
  )

;;
;;  shu-emit-get
;;
(defun shu-emit-get ()
  (insert (concat
           "    \n\n"
           "    /*!\n"
           "     * \\brief Get " shu-lc-comment "\n"
           "     *\n"
           "     */\n"))
  )

;;
;;  shu-emit-set
;;
(defun shu-emit-set (arg)
  (insert (concat
           "    \n\n"
           "    /*!\n"
           "     * \\brief Set " shu-lc-comment "\n"
           "     *\n"
           "     */\n"))
  )


;;
;;  shu-csplit
;;
(defun shu-csplit ()
  "Split a C++ string into multiple strings in order to keep the line length
below a certain minimum length, currently hard coded to column 76.

For example, you may copy a very long line of text into a section of code as
follows:

     static const std::string x(\"This is a very long line of text that looks as though it will go on forever.\");

To be polite to future code readers, you want to split this into multiple lines.
This can be a bit cumbersome if the text is very long.  This function splits the
text at a somewhat arbitrary boundary so that it can be read by others whose
text editors do not show code much beyond column 80 or so.  This is an example
of the above line after csplit was invoked:

     static const std::string x(\"This is a very long line of text that look\"
                                \"s as though it will go on forever.\");"
  (interactive)
  (let ((xquote "[^\\]\"") ;; quote not preceded by escape
        (pad-count )
        (pad )
        (tstart )
        (previous-char )
        (line-min 76)
        (debug-on-error t))
    ;; Ensure that we are positioned between two non-escaped quotes
    (setq tstart (shu-point-in-string))
    (if (not tstart)
        (progn
          (ding)
          (message "%s" "Not in a string."))
      ;; We appear to be inside a string
      (goto-char tstart)
      (setq pad-count (1- (current-column))) ;; Build pad string
      (setq pad (concat "\"\n" (make-string pad-count ? ) "\""))
      (while (not (looking-at xquote))
        (setq previous-char (buffer-substring-no-properties (point) (1+ (point))))
        (forward-char 1)
        (when (>= (1+ (current-column)) line-min)
          (when (string= previous-char "\\")
            (forward-char -1))
          (insert pad)))
      (forward-char 2))
    ))



;;
;;  shu-cunsplit
;;
(defun shu-cunsplit ()
  "The beginnings of a re-write of SHU-CUNSPLIT.
Needs more testing.
Undo the split that was done by csplit.  Place the cursor anywhere
in any of the strings and invoke this function."
  (interactive)
  (let ((white-quote (concat shu-all-whitespace-regexp "*" "[^\\]\""))
        (x (shu-point-in-string))
        (going)
        (eos)       ;; End of last string
        (bos)       ;; Beginning of first string
        (cend)      ;; End of current string
        (del-count) ;; Number of characters to delete
        (pbegin))   ;; Beginning of previous string
    (if (not x)
        (progn
          (ding)
          (message "%s" "Not in a string."))
      ;;; Search forward for last string in the group
      (setq going t)
      (while going
        (setq x (shu-end-of-string "\""))
        (if (not x)
            (setq going nil)
          (setq eos (- x 2))
          (skip-chars-forward shu-all-whitespace-regexp-scf)
          (if (not (looking-at "\""))
              (setq going nil)
            (forward-char 1)
            (setq x (shu-point-in-string))
            (when (not x)
              (setq going nil)))))
      ;;; End of last string
      (goto-char eos)
      (setq x (shu-point-in-string))
      (setq going t)
      ;;; Walk backwards deleting string separators
      (while going
        (setq bos x)
        (if (not (>= (- x 2) (point-min)))
            (setq going nil)
          (goto-char (- x 2))
          (setq pbegin (1- x))
          (skip-chars-backward shu-all-whitespace-regexp-scf)
          (if (not (> (point) (point-min)))
              (setq going nil)
            (when (not (looking-at "\""))
              (backward-char 1))
            (if (not (looking-at "\""))
                (setq going nil)
              (setq cend (point))
              (backward-char 1)
              (setq x (shu-point-in-string))
              (when (and pbegin cend)
                (setq del-count (- pbegin cend))
                (save-excursion
                  (goto-char cend)
                  (delete-char (1+ del-count))
                  (setq pbegin nil)
                  (setq cend nil)))
              (when (not x)
                (setq going nil)))))))
    ))




;;
;;  shu-creplace
;;
(defun shu-creplace ()
  "This function will replace the C++ string in which point is placed with the
C++ string in the kill ring.  The C++ string in the kill ring is expected to be
a single string with or without quotes.  The C++ string in which point is placed
may have been split into smaller substrings in order to avoid long lines.

Assume you have the sample string that is shown in SHU-CSPLIT

     static const std::string x(\"This is a very long line of text that look\"
                                \"s as though it will go on forever.\");

You wish to replace it with a slightly different line of text, perhaps something
that came from the output of a program.  Copy the new string into the kill ring.
Then put the cursor into any part of any line of the string to be replaced
and invoke this function.  This function will remove the old string, replace it
with the contents of the string in the kill ring, and then split it up into
shorter lines as in the following example.  The string in the kill ring may have
opening and closing quotes or not.

     static const std::string x(\"This is a very long line of text that look\"
                                \"s as though it will go on forever and prob\"
                                \"ably already has done so or is threatening\"
                                \" to do so.\");

This is especially useful if you have a a string constant in a unit test and you
have modified the code that creates the string.  gtest will complain that the
expected string did not match the actual string.  If the actual string is
correct, copy it into the kill ring, go into your unit test, find the old
string, place the cursor in the old string, and replace it with the new."
  (interactive)
  (let
      ((xquote "[^\\]\"") ;; quote not preceded by escape
       (start-quote-present)  ;; True if start quote in kill ring
       (end-quote-present)    ;; True if end quote in kill ring
       (have-quotes)          ;; True if kill ring string is quoted
       (unbalanced-quotes)    ;; True if kill ring quotes unbalanced
       (tstart)               ;; Start pos of quoted text in buffer
       (sos )                 ;; Start of string
       (eos )                 ;; End of string
       (del-count ))          ;; Count of chars we deleted in buffer
    (if (not kill-ring)
        (progn
          (ding)
          (message "%s" "Kill ring is empty"))
      ;;
      (with-temp-buffer
        (yank)
        (goto-char (point-min))
        (setq start-quote-present (looking-at "\""))
        (goto-char (1- (point-max)))
        (setq end-quote-present (looking-at "\"")))
      (when (or start-quote-present end-quote-present)
        ;; Have either start or end quote in kill ring
        (setq have-quotes t)
        (when (or (not start-quote-present) (not end-quote-present))
          ;; Missing either start or end quote in kill ring
          (setq unbalanced-quotes t)
          (when start-quote-present
            (ding)
            (message "%s" "String in kill-ring has quote at start but not at end"))
          (when end-quote-present
            (ding)
            (message "%s" "String in kill-ring has quote at end but not at start"))))
      (when (not unbalanced-quotes)
        ;; Find start of string in buffer
        (setq tstart (shu-point-in-string))
        (if (not tstart)
            ;; Point not positioned in a string in the buffer
            (progn
              (ding)
              (message "%s" "Not in string"))
          ;;
          (goto-char tstart) ; Position just after the quote
          (save-excursion  (shu-cunsplit)) ; Make it all one big string
          (setq sos (shu-point-in-string))
          (when have-quotes (setq sos (1- sos))) ;; Delete existing quote
          (re-search-forward xquote nil t)
          (forward-char -1) ;; Now sitting on top of closing quote
          (setq eos (point))
          (setq del-count (- eos sos))
          (when have-quotes (setq del-count (1+ del-count)))
          (goto-char sos)
          (delete-char del-count)
          (save-excursion (yank))
          (when have-quotes (forward-char 1))
          (shu-csplit))))
    ))



;;
;;  shu-s-mode-find-long-line
;;
(defun shu-s-mode-find-long-line ()
  "Place point in column 79 of the next line whose length exceeds 79 characters.
No movement occurs if no lines, starting with the current position, exceed 79
characters in length."
  (interactive)
  (let ((eol )        ;; point at end of current line
        (last-col )   ;; last column in current line
        (done )       ;; loop termination flag
        (start (point)) ;; Remember our start position
        (found1 )     ;; true if we find a long line
        (max-length 79))
    (while (not done)
      (setq last-col (save-excursion (end-of-line) (current-column)))
      (when (> last-col max-length)
        (setq found1 t)
        (setq done t)
        (beginning-of-line)
        (goto-char (1- (+ (point) max-length)))
        )
      (when (not done)
        (forward-line 1)
        (setq eol (save-excursion (end-of-line) (point)))
        (when (>= eol (point-max))
          (setq done t)))
      )
    (when (not found1)
      (message "%s" "No long lines")
      (goto-char start))
    ))


;;
;;  shu-cif
;;
(defun shu-cif ()
  "Insert an empty if statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "if ()\n"
                        pad "{\n"
                        pad "}"))
    (goto-char (+ start pad-count 4))
    ))


;;
;;  shu-celse
;;
(defun shu-celse ()
  "Insert an empty else statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (insert (concat     "else\n"
                        pad "{\n"
                        pad "    "))
    (setq start (point))
    (insert "\n")
    (insert (concat pad "}\n"))
    (goto-char start)
    ))


;;
;;  shu-cfor
;;
(defun shu-cfor ()
  "Insert an empty for statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "for ()\n"
                        pad "{\n"
                        pad "}"))
    (goto-char (+ start pad-count 5))
    ))


;;
;;  shu-cwhile
;;
(defun shu-cwhile ()
  "Insert an empty while statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "while ()\n"
                        pad "{\n"
                        pad "}"))
    (goto-char (+ start pad-count 7))
    ))


;;
;;  shu-cdo
;;
(defun shu-cdo ()
  "Insert an empty do statement."
  (interactive)
  (let ((pad )
        (pad-count (current-column))
        (start ))
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "do\n"
                        pad "{\n"))
    (setq start (1- (point)))
    (insert (concat  pad "} while();"))
    (goto-char (+ start pad-count 9))
    ))



;;
;;  shu-new-deallocate
;;
(defun shu-new-deallocate (var-name class-name)
  "Insert the code to do a standard deallocation of memory allocated by a
specific allocator.  First prompt reads the variable name that points to the
memory to be deallocated.  Second prompt reads the name of the class whose
destructor is to be called.

This generates a code sequence as follows:

        if (var-name)
        {
            var-name->~class-name();
            m_allocator->deallocate(var-name);
            var-name = 0;
        }

VAR-NAME and CLASS-NAME are read from two prompts.  The number of spaces to
indent inside that braces is defined in the custom variable
shu-cpp-indent-length.  The name of the member variable that points to the
allocator in use by the class comes from the custom variable
shu-cpp-default-allocator-name"
  (interactive "*sVariable name?: \nsClass name?: ")
  (let ((pad)
        (pad-count (current-column))
        (start)
        (ipad (make-string shu-cpp-indent-length ? )))
    (setq pad (make-string pad-count ? ))
    (insert
     (concat
      "if (" var-name ")\n"
      pad "{\n"
      pad ipad var-name "->~" class-name "();\n"
      pad ipad shu-cpp-default-allocator-name "->deallocate(" var-name ");\n"
      pad ipad var-name " = 0;\n"
      pad "}\n"))
    ))



;;
;;  shu-citerate
;;
(defun shu-citerate (type-name var-name)
  "Insert the code to iterate through a data structure of type TYPE-NAME whose
instance is identified by VAR-NAME.  First prompt reads the variable name.
Second prompt read the variable name.

The generated code sequence is as follows:

      for (type_name::iterator it = var_name.begin();
           it != var_name.end(); ++it)
      {
      }

The number of spaces to indent inside that braces is defined in the custom
variable shu-cpp-indent-length."
  (interactive "*sType name?: \nsVariable name?: ")
  (shu-internal-citerate type-name var-name)
    )



;;
;;  shu-cciterate
;;
(defun shu-cciterate (type-name var-name)
  "Insert the code to iterate through a data structure of type TYPE-NAME whose
instance is identified by VAR-NAME.  First prompt reads the variable name.
Second prompt read the variable name.

The generated code sequence is as follows:

      for (type_name::const_iterator it = var_name.begin();
           it != var_name.end(); ++it)
      {
      }

The number of spaces to indent inside that braces is defined in the custom
variable shu-cpp-indent-length."
  (interactive "*sType name?: \nsVariable name?: ")
  (shu-internal-citerate type-name var-name t)
    )



;;
;;  shu-internal-citerate
;;
(defun shu-internal-citerate (type-name var-name &optional const)
  "Insert the code to iterate through a data structure of type TYPE-NAME whose
instance is identified by VAR-NAME.  First prompt reads the variable name.
Second prompt read the variable name.

The generated code sequence is as follows:

      for (type_name::iterator it = var_name.begin();
           it != var_name.end(); ++it)
      {
      }

If optional CONST is true, a const iterator is generated."
  (interactive "*sType name?: \nsVariable name?: ")
  (let ((const-qual (if const "const_" ""))
        (rpoint)
        (pad)
        (pad-count (current-column))
        (start)
        (ipad (make-string shu-cpp-indent-length ? )))
    (setq pad (make-string pad-count ? ))
    (insert
     (concat
      "for (" type-name "::" const-qual "iterator it = " var-name ".begin();\n"
      pad
      "     it != " var-name  ".end(); ++it)\n"
      pad "{\n"
      pad ipad))
    (setq rpoint (point))
    (insert
     (concat
      "\n"
      pad "}\n"))
    (goto-char rpoint)
    ))



;;
;;  TODO: Any bunch of stuff that appears between << operators must have
;;        balanced parenthesis.
;;

;;
;;  shu-cpp-check-streaming-op
;;
(defun shu-cpp-check-streaming-op (start end)
  "Check a streaming operation.   Mark a region that contains a set of streaming
operators and invoke this function.  It will make sure that you have no unterminated
strings and that you are not missing any occurrences of <<."
  (interactive "r")                 ; bounds of marked region (start end)
  (let (
        (ret-val)
        (token-list)
        (error-token-info)
        (info)
        (token)
        (token-type)
        (ext-info)
        (point-pair)
        (error-message)
        (tbuf      (get-buffer-create shu-unit-test-buffer))
        (emsg)
        (spoint)
        (debug-on-error t)
        (epoint)
        )
    (setq ret-val (shu-cpp-tokenize-region start end))
    (setq token-list (cdr ret-val))
    (shu-cpp-tokenize-show-list token-list)

    (setq error-token-info (car ret-val))
    (when (not error-token-info)
      (setq error-token-info (shu-cpp-internal-stream-check token-list))
      )
    (if (not error-token-info)
        (message "%s" "OK")
      (shu-cpp-token-show-token-info error-token-info)
      (setq info (car error-token-info))
      (setq token (cdr error-token-info))
      (setq token-type (car info))
      (setq ext-info (cdr info))
      (setq error-message (car ext-info))
      (setq point-pair (cdr ext-info))
      (setq spoint (car point-pair))
      (setq epoint (cdr point-pair))
      (setq emsg error-message)
      (when (not error-message)
        (setq emsg "Uknown error at point"))
      (goto-char spoint)
      (message "%s" emsg)
      )
    ))


;;
;;  shu-cpp-internal-stream-check
;;
(defun shu-cpp-internal-stream-check (token-list)
  "Take a list of tokens found in a C++ streaming operation and check to
ensure that every other token is a << operator.  Two adjacent occurrences of <<
represent an extraneous << operator.  Two adjacent occurrences of tokens that
are not << represent a missing << operator."
  (let
      ((tlist token-list)
       (token-info)
       (token)
       (last-op-token "")
       (expecting-op)
       (done)
       (tbuf      (get-buffer-create shu-unit-test-buffer))
       (info)
       (error-message)
       (token)
       (ext-info)
       (token-type)
       (point-pair)
       (spoint)
       (epoint)
       (ok-op)
       (debug-on-error t)
       (error-token-info))
    (setq token-info (car tlist))
    (setq info (car token-info))
    (setq token (cdr token-info))
    (setq token-type (car info))
    (setq expecting-op nil)
    (when (= token-type shu-cpp-token-type-op)
      (setq expecting-op t))
    (setq done nil)
    (while (not done)
      (setq token-info (car tlist))
      (setq info (car token-info))
      (setq token (cdr token-info))
      (setq token-type (car info))
      (when (/= token-type shu-cpp-token-type-ct) ;; Ignore comments
        (setq ext-info (cdr info))
        (setq point-pair (cdr ext-info))
        (setq spoint (car point-pair))
        (setq epoint (cdr point-pair))
        (if expecting-op
            ;; Expecting an operator and we found one
            (if (= token-type shu-cpp-token-type-op)
                (setq expecting-op nil) ; No longer expecting an op
              ;; Expecting an operator and we did not find one
              (setq error-message "Missing << at point")
              (setq error-token-info (shu-cpp-make-token-info token token-type spoint epoint error-message))
              (setq done t))
          ;; Not expecting an operator and we did not find one
          (if (/= token-type shu-cpp-token-type-op)
              (setq expecting-op t) ; Now we are expecting an operator
            ;; Not expecting an operator and we did find one
            (setq ok-op (or (shu-cpp-is-enclosing-op last-op-token) (string= token "(")))
            (when (not ok-op)
              (setq error-message "Extraneous << at point")
              (setq error-token-info (shu-cpp-make-token-info token token-type spoint epoint error-message))
              (setq done t))))
        (when (= token-type shu-cpp-token-type-op)
          (setq last-op-token token)))
      (setq tlist (cdr tlist))
      (when (not tlist)
        (setq done t)))
    error-token-info
    ))


(defun shu-cpp-is-enclosing-op (op)
  "Return true if the single character in OP is an enclosing character, a left
or right parenthesis or a left or right square bracket."
  (let ((is-enc))
    (setq is-enc (or
                  (string= op ")")
                  (string= op "(")
                  (string= op "]")
                  (string= op "[")))
    is-enc
    ))






;;
;;  shu-cpp-rmv-using
;;
(defun shu-cpp-rmv-using (class-list &optional top-name)
  "Remove \"using namespace\" directives from a C++ file, adding the appropriate
namespace qualifier to all of the unqualified class names.  CLASS-LIST is an
a-list in which the car of each entry is a namespace and the cdr of each entry
is a list of class names.  Here is an example of such an a-list:

     (list
      (cons \"std\"    (list \"set\" \"string\" \"vector\"))
      (cons \"world\"  (list \"Hello\" \"Goodbye\")))

TOP-NAME, if present is a higher level namespace.  Given a top level namespace
of \"WhammoCorp\", then the following line:

     using namespace WhammoCorp::world;

would be interpreted as though it had been written:

     using namespace world;"
  (let* ((gb-name "**shu-chgs**")
         (gb (get-buffer-create gb-name))
         (ct 0)
         (count 0)
         (uc 0)
         (unk "")
         (looking)
         (name)
         (mbeg)
         (item)
         (added-item)
         (duplicates)
         (x)
         (classes)
         (namespace)
         (case-fold-search nil))
    (if (shu-cpp-rmv-blocked class-list top-name gb)
        (progn
          (ding)
          (message "Class ambiguity prevents change.  See buffer %s" gb-name))
      (goto-char (point-min))
      (setq looking t)
      (while looking
        (setq name (shu-cpp-find-using top-name))
        (if (not name)
            (setq looking nil)
          (setq mbeg (match-beginning 0))
          (setq item (cons name (line-number-at-pos mbeg)))
          (shu-add-to-alist added-item item duplicates)
          (if (not (eq added-item item)) ;; Name is a duplicate
              (delete-region (line-beginning-position) (line-end-position))
            (setq x (assoc name class-list))
            (if (not x)
                (progn
                  (princ (format "Unknown namespace: \"%s\"\n" name) gb)
                  (setq uc (1+ uc)))
              (delete-region (line-beginning-position) (line-end-position))
              (setq namespace (car x))
              (setq classes (cdr x))
              (save-excursion
                (setq ct (shu-cpp-qualify-classes classes namespace gb)))
              (setq count (+ count ct))))))
      (goto-char (point-min))
      (when (not (= 0 uc))
        (setq unk (format " %d unknown namespaces. " uc)))
      (message "Replaced %d occurrences.%s  See buffer %s" count unk gb-name))
    count
    ))



;;
;;  shu-cpp-rmv-blocked
;;
(defun shu-cpp-rmv-blocked (class-list top-name gb)
  "Do a pre-check on a file to see if we will be able to remove its \"using
namespace\" directives.  CLASS-LIST is the a-list passed to SHU-CPP-RMV-USING.
USING is the regular expression used to search for \"using namespace\"
directives.  TOP-QUAL is the regular expression used to strip out a higher level
qualifier from the class name in a \"using namespace\" directive, if any.  GB is
the buffer into which diagnostic messages are written.

This function finds all of the \"using namespace\" directives in the file and
checks to see if there is any ambiguity in the resulting class list.  For
example, if namespace \"mumble\" contains class \"Bumble\" and namespace
\"stubble\" also contains class \"Bumble\", we will not know which namespace to
apply to instances of class \"Bumble\".  But this is not an ambiguity if there
is a \"using namespace\" directive for only one of those classes.  That is why
we do the ambiguity check only for namespaces referenced by \"using namespace\"
directives.

This function returns true if such an ambiguity exists."
  (let ((name)
        (mbeg)
        (bol)
        (x)
        (z)
        (uc 0)
        (looking)
        (item)
        (added-item)
        (duplicates)
        (clist)
        (cl)
        (ns)
        (classes)
        (class)
        (listc)
        (blocked))
    (save-excursion
      (goto-char (point-min))
      (setq looking t)
      (while looking
        (setq name (shu-cpp-find-using top-name))
        (if (not name)
            (setq looking nil)
          (setq mbeg (match-beginning 0))
          (setq item (cons name (line-number-at-pos mbeg)))
          (shu-add-to-alist added-item item duplicates)
          (when (eq added-item item) ;; Name is not duplicate
            (setq x (assoc name class-list))
            (when x
              (setq clist (cons x clist)))))))
    (setq cl clist)
    (while cl
      (setq x (car cl))
      (setq ns (car x))
      (setq classes (cdr x))
      (while classes
        (setq class (car classes))
        (setq x (cons class ns))
        (if (not listc)
            (setq listc (cons x listc))
          (setq z (assoc class listc))
          (if (not z)
              (setq listc (cons x listc))
            (princ (format "class %s in namespace %s conflicts with class %s in namespace %s\n"
                           class ns (car z) (cdr z)) gb)
            (setq blocked t)))
        (setq classes (cdr classes)))
      (setq cl (cdr cl)))
    blocked
    ))



;;
;;  shu-cpp-find-using
;;
(defun shu-cpp-find-using (&optional top-name)
  "Return the name of the class found on the next \"using namespace\" directive
or nil of no such directive found.

TOP-NAME, if present is a higher level namespace.  Given a top level namespace
of \"WhammoCorp\", then the following line:

     using namespace WhammoCorp::world;

would be interpreted as though it had been written:

     using namespace world;"
  (interactive)
  (let ((using "using\\s-+namespace\\s-+\\([a-zA-Z0-9:_$]+\\)\\s-*;")
        (looking t)
        (top-qual (when top-name (concat top-name "::\\([a-zA-Z0-9_$]+\\)")))
        (name)
        (using-name)
        (mbeg)
        (bol)
        (not-comment))
    (while looking
      (setq using-name nil)
      (setq not-comment nil)
      (if (not (re-search-forward using nil t))
          (setq looking nil)
        (setq name (match-string 1))
        (setq mbeg (match-beginning 0))
        (setq bol (line-beginning-position))
        (save-match-data
          (save-excursion
            (when (not (shu-point-in-string (1- (point))))
              (setq not-comment t)
              (goto-char bol)
              (when (search-forward "//" mbeg t)
                (setq not-comment nil)))
            (when not-comment
              (when top-qual
                (when (string-match top-qual name)
                  (setq name (match-string 1 name)))))
            (when not-comment
              (setq using-name name)
              (setq looking nil))))))
    using-name
    ))




;;
;;  shu-qualify-class-name
;;
(defun shu-qualify-class-name (target-name namespace)
  "Find all instances of the class name TARGET-NAME and add an explicit namespace
qualifier NAMESPACE.  If the TARGET-NAME is \"Mumble\" and the NAMESPACE is
\"abcd\", then \"Mumble\" becomes \"abcd::Mumble\".  But variable names such
as \"d_Mumble\" or \"MumbleIn\" remain unchanged and already qualified class
names remain unchanged.
This is intended to help rescue code that has one or more \"using namespace\"
directives in it.  The problem with \"using namespace\" is that you now have
class names from other namespaces with no easy way to identify the namespace
to which they belong.  The best thing to do is get rid of the \"using
namespace\" statements and explicitly qualify the class names.  But if you
use a simple replace to do that, you will qualify variable names that resemble
class names as well as class names that are already qualified.  This function
only adds a namespace to a class name that does not already have a namespace
qualifier."
  (shu-internal-replace-class-name target-name 'shu-qualify-class-fun namespace)
  )



;;
;;  shu-qualify-class-fun
;;
(defun shu-qualify-class-fun (namespace name)
  "This is the replacement function for SHU-QUALIFY-CLASS-NAME.  It is called
with the NAMESPACE to be applied to the class whose name is NAME.  It
constructs a new name and issues replace-match to replace it."
  (let ((rename (concat namespace "::" name)))
    (replace-match rename t t)
    ))



;;
;;  shu-replace-class-name
;;
(defun shu-replace-class-name (target-name new-name &optional in-string in-comment)
  "Find all instances of the class name TARGET-NAME and replace it with the name
NEW-NAME.  If the target name is \"Mumble\", then all instances of \"Mumble\"
that resemble class names are replaced.  But names such as \"d_Mumble\" or
\"MumbleIn\" remain unchanged.  if IN-STRING is true, then instances of the
class name found inside a string are replaced.  if IN-COMMENT is true, then
instances of the class name found inside a comment are replaced."
  (shu-internal-replace-class-name target-name 'shu-replace-class-fun
                                   new-name in-string in-comment)
  )



;;
;;  shu-replace-class-fun
;;
(defun shu-replace-class-fun (new-name name)
  "This is the replacement function for SHU-REPLACE-CLASS-NAME.  It is called
with the NEW-NAME to replace the class NAME.  It calls replace-match to replace
NAME with NEW-NAME."
    (replace-match new-name t t)
    )



;;
;;  shu-internal-replace-class-name
;;
(defun shu-internal-replace-class-name (target-name replace-func replace-arg
                                                    &optional in-string in-comment)
  "Find all instances of the class name TARGET-NAME and if it actually appears
to be a class name, call REPLACE-FUN passing to it REPLACE-ARG and the class
name.  REPLACE-FUN issues the appropriate replace-match call, constructing the
replacement for the class name from some combination of REPLACE-ARG and the
class name.  IN-STRING is true if a class name inside of a string is to be
replaced.  IN-COMMENT is true if a class name inside of a comment is to be
replaced."
  (let ((bol)
        (eol)
        (mbeg)
        (mend)
        (have-match)
        (name)
        (rename)
        (count 0)
        (prefix-rx "[:>.]")
        (case-fold-search nil)
        )
    (while (search-forward target-name nil t)
      (setq bol (line-beginning-position))
      (setq eol (line-end-position))
      (setq name (match-string 0))
      (setq mbeg (match-beginning 0))
      (setq mend (match-end 0))
      (setq have-match t)
      (save-match-data
        (when (> mbeg bol)
          (save-excursion
            (if (shu-class-is-blocked mbeg in-string in-comment)
                (setq have-match nil)
              (goto-char (1- mbeg))
              (if (looking-at shu-cpp-name)
                  (setq have-match nil)
                (save-match-data
                  (if (looking-at shu-not-all-whitespace-regexp)
                      (progn
                        (when (looking-at prefix-rx)
                          (setq have-match nil)))
                    (when (re-search-backward shu-not-all-whitespace-regexp bol t)
                      (when (looking-at prefix-rx)
                        (setq have-match nil))))))))))
      (when have-match
        (when (< mend eol)
          (save-excursion
            (goto-char mend)
            (when (looking-at shu-cpp-name)
              (setq have-match nil)))))
      (when have-match
        (save-excursion
          (save-match-data
            (goto-char bol)
            (when (re-search-forward "#\\s-*include" mbeg t)
              (setq have-match nil)))))
      (when have-match
        (funcall replace-func replace-arg name)
        (setq count (1+ count))))
    count
    ))



;;
;;  shu-class-is-blocked
;;
(defun shu-class-is-blocked (pos  &optional in-string in-comment)
  "Return true if a class name should be ignored because it is either in a
string or a comment.

We have found something at point POS that looks as though it might be a class
name.  If it is in a string or is preceded on the same line by \"//\" (also not
in a string), then it is either in a string or is probably in a comment, so we
may want to ignore it.  IN-STRING is true if a class name inside of a string is
to be replaced.  IN-COMMENT is true if a class name inside of a comment is to be
replaced.

Return true if the class name should be ignored."
  (let ((bol (line-beginning-position))
        (no-string (not in-string))
        (no-comment (not in-comment))
        (blocked))
    (save-excursion
      (if (and no-string
               (shu-point-in-string pos))
          (setq blocked t)
        (goto-char bol)
        (when no-comment
          (when (search-forward "//" pos t)
            (when (not (shu-point-in-string))
              (setq blocked t))))))
    blocked
    ))




;;
;;  shu-interactive-qualify-class-name
;;
(defun shu-interactive-qualify-class-name ()
  "Interactively call SHU-QUALIFY-CLASS-NAME to find all instances of a class name and
add a namespace qualifier to it.  First prompt is for the class name.  If a fully qualified
class name is supplied, then the given namespace is applied to the class name.  If the name
supplied is not a namespace qualified class name, then a second prompt is given to read the
namespace.
This is intended to help rescue code that has one or more \"using namespace\"
directives in it.  The problem with \"using namespace\" is that you now have
class names from other namespaces with no easy way to identify the namespace
to which they belong.  The best thing to do is get rid of the \"using
namespace\" statements and explicitly qualify the class names.  But if you
use a simple replace to do that, you will qualify variable names that resemble
class names as well as class names that are already qualified.  This function
only adds a namespace to a class name that does not already have a namespace
qualifier."
  (interactive)
  (let ((class "")
        (qclass "")
        (qual "")
        (sqr (concat "\\(" shu-cpp-name "+\\)\\s-*::\\s-*\\(" shu-cpp-name "+\\)"))
        (count 0)
        (case-fold-search nil)
        (minibuffer-allow-text-properties nil))
    (while (= 0 (length qclass))
      (setq qclass (read-from-minibuffer "Class name? ")))
    (setq class qclass)
    (when (string-match sqr qclass)
      (setq qual (match-string 1 qclass))
      (setq class (match-string 2 qclass)))
    (while (= 0 (length qual))
      (setq qual (read-from-minibuffer "namespace? ")))
    (setq count (shu-qualify-class-name class qual))
    (message "Replaced %d occurrences" count)
    ))



;;
;;  shu-cpp-qualify-classes
;;
(defun shu-cpp-qualify-classes (class-list namespace &optional buffer)
  "Repeatedly call SHU-QUALIFY-CLASS-NAME for all class names in CLASS-LIST.
NAMESPACE is either the name of a single namespace to apply to all classes
in CLASS-LIST or is a list of namespaces each of which has a one to one
correspondence with a class name in CLASS-LIST.  The optional BUFFER
argument may be a buffer in which the actions are recorded.  Return the
number of names changed."
  (let ((classes class-list)
        (names namespace)
        (buf-msg "")
        (class-name)
        (cnt)
        (count 0)
        (ct)
        (ns))
    (when buffer
      (when (not (bufferp buffer))
        (error "Supplied BUFFER argument is not a buffer")))
    (cond
     ((stringp names)
      (setq ns names)
      (while classes
        (setq class-name (car classes))
        (goto-char (point-min))
        (setq ct (shu-qualify-class-name class-name ns))
        (setq count (+ count ct))
        (setq cnt (shu-fixed-format-num ct 8))
        (when buffer
          (princ (format "%s: %s::%s\n" cnt ns class-name) buffer))
        (setq classes (cdr classes))))
     ((listp names)
      (when (not (= (length classes) (length names)))
        (error "Length of CLASS-LIST list (%d) not same as length of NAMESPACE list (%d)"
               (length classes) (length names)))
      (while classes
        (setq class-name (car classes))
        (setq ns (car names))
        (goto-char (point-min))
        (setq ct (shu-qualify-class-name class-name ns))
        (setq count (+ count ct))
        (setq cnt (shu-fixed-format-num ct 8))
        (when buffer
          (princ (format "%s: %s::%s\n" cnt ns class-name) buffer)
          (setq names (cdr names)))
        (setq classes (cdr classes))))
     (t
      (error "NAMES argument is neither a string nor a list")))
    (goto-char (point-min))
    (when buffer
      (setq buf-msg (concat "  See buffer " (buffer-name buffer))))
    (message "Replaced %d occurrences.%s" count buf-msg)
    count
    ))



;;
;;  shu-qualify-namespace-std
;;
(defun shu-qualify-namespace-std ()
  "Add \"std\" namespace qualifier to some of the classes in \"std\".  Return the
count of class names changed."
  (interactive)
  (let ((gb (get-buffer-create "**chgs**"))
        (namespace "std")
        (classes (list
                  "endl"
                  "ifstream"
                  "ios_base"
                  "map"
                  "ostringstream"
                  "pair"
                  "set"
                  "setfill"
                  "setw"
                  "string"
                  "vector"))
        (count 0))
    (setq count (shu-cpp-qualify-classes classes namespace gb))
    count
    ))



;;
;;  shu-qualify-namespace-bsl
;;
(defun shu-qualify-namespace-bsl ()
  "Add \"bsl\" namespace qualifier to some of the classes in \"bsl\".  Return the
count of class names changed."
  (interactive)
  (let ((gb (get-buffer-create "**chgs**"))
        (namespace "bsl")
        (classes (list
                  "endl"
                  "ifstream"
                  "ios_base"
                  "map"
                  "ostringstream"
                  "pair"
                  "set"
                  "setfill"
                  "setw"
                  "string"
                  "vector"))
        (count 0))
    (setq count (shu-cpp-qualify-classes classes namespace gb))
    count
    ))



;;
;;  shu-dbx-summarize-malloc
;;
(defun shu-dbx-summarize-malloc ()
  "Go through the output of a dbx malloc dump and generate a summary.  dbx is
the AIX debugger.  It has a malloc command that goes through the heap and prints
one line for every allocated buffer.  Here is a sample of some of its output:

         ADDRESS         SIZE HEAP    ALLOCATOR
      0x30635678          680    0     YORKTOWN
      0x30635928          680    0     YORKTOWN
      0x30635bd8          680    0     YORKTOWN

YORKTOWN is the name of the default allocator on AIX.  This function goes
through the malloc output and gets the number and sizes of all buffers
allocated.  This tells you how many buffers were allocated, the total number of
bytes allocated, and the total number of buffers allocated by size.  The output
is placed in a separate buffer called **shu-aix-malloc**."
  (interactive)
  (let ((gb (get-buffer-create "**shu-aix-malloc**"))
        (rs   "0x\\([a-f0-9]+\\)\\s-+\\([0-9]+\\)\\s-+[0-9]+\\s-+YORKTOWN")
        (address 0)
        (size 0)
        (sizes)
        (count 0)
        (x)
        (z)
        (buf-count 0))
    (goto-char (point-min))
    (while (re-search-forward rs nil t)
      (setq address (match-string 1))
      (setq size (match-string 2))
      (setq size (string-to-number size))
      (if (not sizes)
          (setq sizes (list (cons size 1)))
        (setq x (assoc size sizes))
        (if x
            (progn
              (setq count (cdr x))
              (setq count (1+ count))
              (setcdr x count))
          (setq count 1)
          (setq x (cons size count))
          (setq sizes (cons x sizes)))))
    (setq sizes (sort sizes (lambda(lhs rhs) (< (car lhs) (car rhs)))))
    (shu-aix-show-malloc-list sizes gb)
    (setq sizes (sort sizes (lambda(lhs rhs) (< (cdr lhs) (cdr rhs)))))
    (shu-aix-show-malloc-list sizes gb)
    (setq sizes (sort sizes (lambda(lhs rhs) (< (* (car lhs) (cdr lhs))(* (car rhs) (cdr rhs))))))
    (shu-aix-show-malloc-list sizes gb)
    ))



;;
;;  shu-aix-show-malloc-list
;;
(defun shu-aix-show-malloc-list (mlist gb)
  "Print the number of buffers allocated by size from an AIX dbx malloc command."
  (let ((x mlist)
        (z)
        (size)
        (count 0)
        (total 0)
        (this 0)
        (nthis)
        (nsize)
        (ncount)
        (buf-count 0)
        (ntotal)
        (nbuf-count))
    (princ "\n\n" gb)
    (princ "      buffer    buffer\n" gb)
    (princ "        size     count\n" gb)
    (while x
      (setq z (car x))
      (setq size (car z))
      (setq count (cdr z))
      (setq this (* size count))
      (setq total (+ total this))
      (setq nsize (shu-fixed-format-num size 12))
      (setq nthis (shu-fixed-format-num this 12))
      (setq ncount (shu-fixed-format-num count 8))
      (princ (concat nsize ": " ncount "   (" nthis ")" "\n") gb)
      (setq buf-count (+ count buf-count))
      (setq x (cdr x)))
    (setq ntotal (shu-group-number total))
    (setq nbuf-count (shu-group-number buf-count))
    (princ (format "\nbuf-count: %s buffers allocated\n" nbuf-count) gb)
    (princ (format "total: %s bytes allocated\n" ntotal) gb)
    (princ (format "%s distinct buffer sizes found\n" (shu-group-number (length mlist))) gb)
    (cons total buf-count)
    ))



;;
;;  shu-cpp-fix-prototype
;;
(defun shu-cpp-fix-prototype ()
  "Place the cursor on the beginning of a function declaration that has been
copied from a .cpp file to a .h file.  This function fixes up the function
prototype to make it suitable for a .h file.
For example, this declaration:

      double Frobnitz::hitRatio(
          const int  reads,
          const int  writes)
      const

would be transformed into

          double hitRatio(
              const int  reads,
              const int  writes)
          const;"
  (interactive)
  (let ((all-white (concat shu-not-all-whitespace-regexp "*"))
        (bol (line-beginning-position))
        (eol (line-end-position))
        (eos (+ 10 (line-end-position)))
        (bon)
        (eon)
        (ns-length)
        (limit))
    (if (not (re-search-forward all-white eol t))
        (progn
          (ding)
          (message "%s" "No whitespace found to start namespace qualifier"))
      (setq bon (1+ (point)))
      (if (not (search-forward "::" eol t))
          (progn
            (ding)
            (message "%s" "Cannot find \"::\" namespace separator"))
        (setq eon (point))
        (setq ns-length (- eon bon))
        (if (not (search-forward "(" eos t))
            (progn
              (ding)
              (message "%s" "Cannot find opening left parenthesis"))
          (backward-char 1)
          (forward-sexp)
          (setq eos (point))
          (setq limit (+ 11 eos))
          (when (search-forward "const" limit t)
            (setq eos (point)))
          (goto-char eos)
          (insert ";")
          (setq eos (- eos ns-length))
          (delete-region bon eon)
          (shu-shift-region-of-text shu-cpp-indent-length bol eos))))
    ))


;;
;;  shu-simple-hother-file
;;
(defun shu-simple-hother-file ()
  "Return the name of the .h file that corresponds to the .cpp file or .t.cpp file
that is in the current buffer.  This version of the function creates the name of
the .h file from the name of the file in the current buffer.  This is in contrast
with the function shu-hother which finds the corresponding .h file from the list
of files in the current project."
  (let ((base-name (file-name-sans-extension (buffer-file-name)))
        (newfile ))
    (when (string= (file-name-extension base-name) "t")
      (setq base-name (file-name-sans-extension base-name)))
    (setq newfile (concat base-name ".h"))
    (if (file-readable-p newfile)
        newfile
      nil)
    ))



;;
;;  shu-cpp-find-h-definition
;;
(defun shu-cpp-find-h-definition ()
  "While in a cpp file, position point on a variable name that is defined in the
corresponding header file and invoke this function.  It will find all occurrences of
the name in the header file and put them in the message area."
  (interactive)
  (let ((hfile)
        (fbuf)
        (file-buf)
        (var-name)
        (lines)
        (spoint))
    (setq hfile (shu-simple-hother-file))
    (if (not hfile)
        (progn
          (ding)
          (message "%s" "There is no other h file")
          )
      (setq fbuf (get-file-buffer hfile))
      (if fbuf
          (setq file-buf fbuf)
        (setq file-buf (find-file-noselect hfile)))
      (setq var-name (shu-cpp-get-variable-name))
      (with-current-buffer file-buf
        (goto-char (point-min))
        (setq lines (shu-cpp-find-variable-name-lines-by-token var-name))
        (if (not lines)
            (progn
              (ding)
              (message "%s not found" var-name))
          (message "%s" lines)))
      (when (not fbuf) ;; We created buffer
        (kill-buffer file-buf)))
    ))



;;
;;  shu-cpp-get-variable-name
;;
(defun shu-cpp-get-variable-name ()
  "If point is sitting on something that looks like a legal variable name, return it,
otherwise, return nil."
  (let ((target-char shu-cpp-name)
        (target-name (concat shu-cpp-name "+"))
        (bol (line-beginning-position))
        (eol (line-end-position))
        (var-name))
    (save-excursion
      (when (looking-at target-char) ;; Looking at a legal variable name character
        (while (and (looking-at target-char) ;; Still on a variable name char
                    (> (point) bol)) ;; And still on same line
          (backward-char 1))            ;; Keep moving back until we aren't on a variable name char
        ;;  or we hit the beginning of the line
        (when (not (looking-at target-char)) ;; Moved backward past beginning of name
          (forward-char 1))             ;; Move forward to what might be the beginning
        (when (re-search-forward target-name eol t)
          (setq var-name (match-string 0))
          ))) ;; Have something that matches variable name syntax
    var-name
    ))



;;
;;  shu-cpp-get-variable-name-position
;;
(defun shu-cpp-get-variable-name-position ()
  "If point is sitting on something that looks like a legal variable name,
return a cons cell that contains the start and end positions of the name
otherwise, return nil."
  (let ((target-char shu-cpp-name)
        (target-name (concat shu-cpp-name "+"))
        (bol (line-beginning-position))
        (eol (line-end-position))
        (start-pos)
        (end-pos)
        (ret-val))
    (save-excursion
      (when (looking-at target-char) ;; Looking at a legal variable name character
        (while (and (looking-at target-char) ;; Still on a variable name char
                    (> (point) bol)) ;; And still on same line
          (backward-char 1))            ;; Keep moving back until we aren't on a variable name char
        ;;  or we hit the beginning of the line
        (when (not (looking-at target-char)) ;; Moved backward past beginning of name
          (forward-char 1))             ;; Move forward to what might be the beginning
        (when (re-search-forward target-name eol t)
          (setq start-pos (match-beginning 0))
          (setq end-pos (match-end 0))
          (setq ret-val (cons start-pos end-pos))
          ))) ;; Have something that matches variable name syntax
    ret-val
    ))



;;
;;  shu-cpp-find-variable-name-by-token
;;
(defun shu-cpp-find-variable-name-by-token (var-name)
  "Tokenize the entire buffer and return the position of the first token
that matches var-name."
  (let ((token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
        (tlist)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message))
    (setq tlist token-list)
    (while tlist
      (setq token-info (car tlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq token (shu-cpp-token-extract-token token-info))
        (when (string= token var-name)
          (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)))
      (setq tlist (cdr tlist)))
    spoint
    ))



;;
;;  shu-cpp-find-variable-name-lines-by-token
;;
(defun shu-cpp-find-variable-name-lines-by-token (var-name)
  "Tokenize the entire buffer and return a string that is composed of each
line that contains the token."
  (let ((token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
        (tlist)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message)
        (line)
        (got-lines)
        (lines "")
        (prefix "")
        (line-no))
    (setq tlist token-list)
    (while tlist
      (setq token-info (car tlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq token (shu-cpp-token-extract-token token-info))
        (when (string= token var-name)
          (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
          (goto-char spoint)
          (setq line-no (number-to-string (shu-current-line)))
          (setq line
                (concat line-no ": "
                        (shu-trim
                         (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
          (setq lines (concat lines prefix line))
          (setq prefix "\n")
          (setq got-lines t)))
      (setq tlist (cdr tlist)))
    (when (not got-lines)
      (setq lines nil))
    lines
    ))



;;
;;  shu-to-snake
;;
(defun shu-to-snake ()
  "Convert the variable name at point from camel case to snake case.

For example, \"mumbleSomethingOther\" becomes \"mumble_something_other\"."
  (interactive)
  (let ((pos (shu-cpp-get-variable-name-position))
        (start-pos)
        (end-pos)
        (case-fold-search nil))
    (if (not pos)
        (progn
          (ding)
          (message "%s" "Not sitting on a variable name"))
      (save-excursion
      (setq start-pos (car pos))
      (setq end-pos (cdr pos))
      (goto-char start-pos)
      (while (re-search-forward "\\([A-Z]\\)" end-pos t)
        (replace-match (concat "_" (downcase (match-string 1))) t t)
        (setq end-pos (1+ end-pos)))))
    ))




;;
;;  shu-cpp-general-set-alias
;;
(defun shu-cpp-general-set-alias ()
  "Set the common alias names for the functions in shu-cpp-general.
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'cpp1-class 'shu-cpp1-class)
  (defalias 'cpp2-class 'shu-cpp2-class)
  (defalias 'new-c-class 'shu-new-c-class)
  (defalias 'operators 'shu-operators)
  (defalias 'shu-dox-hdr 'shu-shu-dox-hdr)
  (defalias 'dox-brief 'shu-dox-brief)
  (defalias 'dox2-hdr 'shu-dox2-hdr)
  (defalias 'dcc 'shu-dcc)
  (defalias 'dce 'shu-dce)
  (defalias 'clc 'shu-clc)
  (defalias 'drc 'shu-drc)
  (defalias 'binclude 'shu-binclude)
  (defalias 'ginclude 'shu-ginclude)
  (defalias 'dox-cvt 'shu-dox-cvt)
  (defalias 'dox-cbt 'shu-dox-cbt)
  (defalias 'new-x-file 'shu-new-x-file)
  (defalias 'new-h-file 'shu-new-h-file)
  (defalias 'new-c-file 'shu-new-c-file)
  (defalias 'author 'shu-author)
  (defalias 'getters 'shu-getters)
  (defalias 'get-set 'shu-get-set)
  (defalias 'csplit 'shu-csplit)
  (defalias 'cunsplit 'shu-cunsplit)
  (defalias 'creplace 'shu-creplace)
  (defalias 'cif 'shu-cif)
  (defalias 'celse 'shu-celse)
  (defalias 'cfor 'shu-cfor)
  (defalias 'cwhile 'shu-cwhile)
  (defalias 'cdo 'shu-cdo)
  (defalias `new-deallocate `shu-new-deallocate)
  (defalias `citerate `shu-citerate)
  (defalias `cciterate `shu-cciterate)
  (defalias 'ck 'shu-cpp-check-streaming-op)
  (defalias 'set-default-namespace 'shu-set-default-namespace)
  (defalias 'qualify-class 'shu-interactive-qualify-class-name)
  (defalias 'qualify-std 'shu-qualify-namespace-std)
  (defalias 'qualify-bsl 'shu-qualify-namespace-bsl)
  (defalias 'dbx-malloc 'shu-dbx-summarize-malloc)
  (defalias 'fixp 'shu-cpp-fix-prototype)
  (defalias 'getdef 'shu-cpp-find-h-definition)
  (defalias 'to-snake 'shu-to-snake)
  )

;;; shu-cpp-general.el ends here
