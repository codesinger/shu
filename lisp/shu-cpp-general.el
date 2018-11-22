;;; shu-cpp-general.el --- Shu project code for dealing wth C++ in Emacs
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
;; shu-cpp-general.el
;;
;; A collection of useful functions for dealing with C++ code

(provide 'shu-cpp-general)
(require 'shu-base)


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
"A list of all of tbe base types in C and C++.  This may be modified by shu-add-cpp-base-types")

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

;;
;;  Functions for customzing
;;


;;
;;  shu-add-cpp-base-types
;;
(defun shu-add-cpp-base-types (ntypes)
  "Add one or more data types to the list of C++ native data types defined in shu-cpp-base-types
in shu-cpp-general.el.  Argument may be a single type in a string or a list of strings.
This moifies shu-cpp-base-types."
  (let (
    (nt ntypes)
       )
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
  (let (
    (debug-on-error t)
       )
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
  (let (
    (debug-on-error t)
    (hfile (file-name-nondirectory (buffer-file-name)))
    (hfile-base (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
       )
  (shu-internal-cpp2-class hfile-base)
))

;;
;;  shu-internal-cpp2-class
;;
(defun shu-internal-cpp2-class (class-name)
  "Place a skeleton class definition in the current buffer at point."
  (let (
    (debug-on-error t)
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
    (comment2       "!< The object to be streamed")
       )

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
  (let (
    (debug-on-error t)
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
    (comment2       "!< The object to be streamed")
       )

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
  (interactive)
  (setq shu-cpp-author name))


;;
;;  shu-set-default-global-namespace
;;
(defun shu-set-default-global-namespace (name)
  (interactive "sName?: ")
  (setq shu-cpp-default-global-namespace name))


;;
;;  shu-set-default-namespace
;;
(defun shu-set-default-namespace (name)
  (interactive "sName?: ")
  (setq shu-cpp-default-namespace name))


;;
;;  shu-dox-hdr
;;
(defun shu-dox-hdr ()
  "Place a skeleton Doxygen header definition at point."
  (interactive)
  (let (
    (debug-on-error t)
    (ccol           (current-column))
    (pad-count      (current-column))
    (pad            nil)
       )
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
  (let (
    (debug-on-error t)
    (ccol           (current-column))
    (pad-count      (current-column))
    (pad            nil)
       )
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
  (let (
    (debug-on-error t)
    (ccol           (current-column))
    (pad-count      (current-column))
    (pad            nil)
       )
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
  (let (
    (debug-on-error t)
    (ccol           (current-column))
    (pad-count      (current-column))
    (pad            nil)
       )
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
  (let (
    (debug-on-error t)
    (ccol           (current-column))
    (pad-count      (current-column))
    (pad            nil)
       )
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
  (let (
    (debug-on-error t)
    (ccol           (current-column))
    (pad-count      (current-column))
    (pad            nil)
       )
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
  (let (
    (debug-on-error t)
    (ccol           (current-column))
    (pad-count      (current-column))
    (pad            nil)
       )
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
;;  shu-other
;;
(defun shu-other ()
  "Visit an h file from a c file or a c file from an h file
If visiting a .h file, invoke SHU-OTHER and you will be taken to the
.c or .cpp file.  If visiting a .c or .cpp file, invoke other and you
will be taken to the corresponding .h file"
  (interactive)
  (let (
    (debug-on-error t)
    (ext       (file-name-extension (buffer-file-name)))
    (base-name (file-name-sans-extension (buffer-file-name)))
    (newext    nil)
    (tryc      nil)
    (newfile   nil)
       )
  (when (string= ext "h")
    (setq tryc t))

  (if tryc
    (progn
      (setq newfile (concat base-name ".cpp"))
      (if (file-readable-p newfile)
        (find-file newfile)
      ;
        (setq newfile (concat base-name ".c"))
        (if (file-readable-p newfile)
          (find-file newfile)
        (message (concat "Cannot find other file for " base-name))))
    )
  ;
    (setq newfile (concat base-name ".h"))
    (if (file-readable-p newfile)
      (find-file newfile)
        (message (concat "Cannot find other file for " base-name))
    ))
))


;;
;;  shu-tother
;;
(defun shu-tother ()
  "Visit a t.cpp file from the corresponding .cpp or .h file.  If visiting a .c or
.cpp file, invoke SHU-TOTHER and you will be taken to the corresponding .t.cpp
file."
  (interactive)
  (let ((base-name (file-name-sans-extension (buffer-file-name)))
        (newfile ))

    (setq newfile (concat base-name ".t.cpp"))
    (if (file-readable-p newfile)
        (find-file newfile)
      (message (concat "Cannot find test file for " base-name)))
    ))


;;
;;  shu-hother
;;
(defun shu-hother ()
  "Visit a .h file from the corresponding .cpp or t.cpp file.  If visiting a .cpp or
t.cpp file, invoke SHU-HOTHER and you will be taken to the corresponding .h file."
  (interactive)
  (let ((base-name (file-name-sans-extension (buffer-file-name)))
        (newfile ))

    (when (string= (file-name-extension base-name) "t")
      (setq base-name (file-name-sans-extension base-name)))
    (setq newfile (concat base-name ".h"))
    (if (file-readable-p newfile)
        (find-file newfile)
      (message (concat "Cannot find H file for " base-name)))
    ))



;;
;;  shu-cother
;;
(defun shu-cother ()
  "Visit a .cpp file from the corresponding .t.cpp or .h file.  If visiting a t.cpp or .h
file, invoke SHU-COTHER and you will be taken to the corresponding .cpp or .c file."
  (interactive)
  (let ((base-name (file-name-sans-extension (buffer-file-name)))
        (newfile ))

    (when (string= (file-name-extension base-name) "t")
      (setq base-name (file-name-sans-extension base-name)))
    (setq newfile (concat base-name ".cpp"))
    (if (file-readable-p newfile)
        (find-file newfile)
      (setq newfile (concat base-name ".c"))
      (if (file-readable-p newfile)
          (find-file newfile)
        (message (concat "Cannot find C file for " base-name))))
    ))


;;
;; shu-dox-cvt
;;
(defun shu-dox-cvt ()
  "Convert a section of comments delimited by // into Doxygen format."
  (interactive)
  (let (
    (debug-on-error t)
    (pad            nil)
    (eol            nil)
    (spos           nil)
    (slash-pos      nil)
    (not-done       t)
       )
  (beginning-of-line)
  (setq eol (save-excursion (forward-line 1) (end-of-line) (point)))
  (setq spos (search-forward "//" eol t))
  (if (not spos)
    (error "Unable to find any // near here")
  ;
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
      ;
        (progn
        (setq not-done nil)
        (insert (concat pad " */\n"))))
    )
  ))
))


;;
;; shu-dox-cbt
;;
(defun shu-dox-cbt ()
  "Convert a section of comments delimited by //! into Doxygen brief format."
  (interactive)
  (let (
        (debug-on-error t)
        (bol (save-excursion (beginning-of-line) (point)))
        (pad  )
        (eol  )
        (spos )
        (slash-pos )
        (not-done       t)
        (j              0)
        )
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
        (end-of-line)
        )
      )
    ))


;;
;;  shu-new-x-file
;;
(defun shu-new-x-file ()
  "Generate a skeleton Doxygen \\file directive."
  (interactive)
  (let (
    (xfile (file-name-nondirectory (buffer-file-name)))
       )
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
  (let (
    (debug-on-error t)
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
       )
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
  (let (
    (debug-on-error t)
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
    (base-name (file-name-sans-extension (buffer-file-name)))
       )
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
  (let (
    (clen       (length line))
    (pad        "")
    (pad-count  nil)
       )
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
    (setq debug-on-error t)
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
  (let (
    (non-white1 "[^[:space:]\n\t\v\f]")
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
    (debug-on-error t)
       )
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
  "Split a C++ string into multiple strings in order to keep the line length below a
certain minimum length, currently hard coded to column 76."
  (interactive)
  (let (
    (xquote "[^\\]\"") ;; quote not preceded by escape
    (pad-count )
    (pad )
    (tstart )
    (previous-char )
    (line-min 76)
    (debug-on-error t)
       )
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
  "Undo the split that was done by csplit."
  (interactive)
  (let (
        (xquote "[^\\]\"") ;; quote not preceded by escape
        (white-quote "\\s-*[^\\]\"") ;; Optional whitespace, followed by non-escaped quote
        (eol )   ;; End of current line
        (eop )   ;; End of previous string
        (bon )   ;; Beginning of next string
        (neol )  ;; Next end of line
        (del-count ) ;; Number of intervening chars to delete betwen strings
        (done )
        )
    (if (not (shu-point-in-string))
        (progn
          (ding)
          (message "%s" "Not in a string."))
      ;; We appear to be inside a string
      (while (not done)
        (setq eol (save-excursion (end-of-line) (point)))
        (if (not (re-search-forward xquote eol t))
            (setq done t)  ;; Current string not terminated?
          ;;  Found end of current string
          (setq eop (1- (point))) ;; Remember where it is
          (if (= (point) (point-max))
              (setq done t)
            (setq neol (save-excursion (end-of-line) (forward-char 1) (end-of-line) (point)))
            (if (not (re-search-forward white-quote neol t))
                (setq done t)  ;; Have no start of next string?
              ;;  Found start of next string
              (setq bon (point))
              (setq del-count (- bon eop))
              (goto-char eop)
              (delete-char del-count))))))
    ))


;;
;;  shu-creplace
;;
(defun shu-creplace ()
  "This function will replace the C++ string in which point is placed with the C++ string
in the kill ring.  The C++ string in the kill ring is expected to be a single string with
or without quotes.  The C++ string in which point is placed may have been split into
smaller substrings in order to avoid long lines."
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
          (setq sos (point))
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
  (let (
    (eol )        ;; point at end of current line
    (last-col )   ;; last column in current line
    (done )       ;; loop termination flag
    (start (point)) ;; Remember our start position
    (found1 )     ;; true if we find a long line
    (max-length 79)
       )
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
  (let (
    (pad )
    (pad-count (current-column))
    (start )
       )
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
  (let (
    (pad )
    (pad-count (current-column))
    (start )
       )
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
  (let (
    (pad )
    (pad-count (current-column))
    (start )
       )
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
  (let (
    (pad )
    (pad-count (current-column))
    (start )
       )
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
  (let (
    (pad )
    (pad-count (current-column))
    (start )
       )
    (setq pad (make-string pad-count ? ))
    (setq start (save-excursion (beginning-of-line) (point)))
    (insert (concat     "do\n"
                    pad "{\n"))
    (setq start (1- (point)))
    (insert (concat  pad "} while();"))
    (goto-char (+ start pad-count 9))
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
  (let ((is-enc))
    (setq is-enc (or
                  (string= op ")")
                  (string= op "(")
                  (string= op "]")
                  (string= op "[")))
    is-enc
    ))



;;
;;  shu-qualify-class-name
;;
(defun shu-qualify-class-name (target-name namespace)
  "Find all instances of the class name TARGET-NAME and add an explicit namespace
qualifier NAMESPACE.  If the TARGET-NAME is \"Mumble\" and the NAMESPACE is
\"abcd\", then \"Mumble\" becomes \"abcd::Mumble\".  But variable names such
as \"d_Mumble\" or \"MumbleIn\" remain unchanged and already qualified class
names remain unchanged."
  (interactive)
  (let ((name-target (concat shu-cpp-name "+"))
        (bol)
        (eol)
        (mbeg)
        (mend)
        (have-match)
        (name)
        (rename)
        (count 0)
        (case-fold-search nil))
    (while (search-forward target-name nil t)
      (setq bol (save-excursion (beginning-of-line) (point)))
      (setq eol (save-excursion (end-of-line) (point)))
      (setq name (match-string 0))
      (setq mbeg (match-beginning 0))
      (setq mend (match-end 0))
      (setq have-match t)
      (when (> mbeg bol)
        (save-excursion
          (goto-char (1- mbeg))
          (if (looking-at shu-cpp-name)
              (setq have-match nil)
            (save-match-data
              (when (re-search-backward shu-not-all-whitespace-regexp nil t)
                (when (looking-at ":")
                  (setq have-match nil)))))))
      (when have-match
        (when (< mend eol)
          (save-excursion
            (goto-char mend)
            (when (looking-at shu-cpp-name)
              (setq have-match nil)))))
      (when have-match
        (setq rename (concat namespace "::" name))
        (replace-match rename t t)
        (setq count (1+ count))))
    count
    ))


;;
;;  shu-interactive-qualify-class-name
;;
(defun shu-interactive-qualify-class-name ()
  "Interactively call SHU-QUALIFY-CLASS-NAME to find all instances of a class name and
add a namespace qualifier to it.  First prompt is for the class name.  If a fully qualified
class name is supplied, then the given namespace is applied to the class name.  If the name
supplied is not a namespace qualified class name, then a second prompt is given to read the
namespace."
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
  (defalias 'other 'shu-other)
  (defalias 'cother 'shu-cother)
  (defalias 'hother 'shu-hother)
  (defalias 'tother 'shu-tother)
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
  (defalias 'ck 'shu-cpp-check-streaming-op)
  (defalias 'set-default-namespace 'shu-set-default-namespace)
  (defalias 'qualify-class 'shu-interactive-qualify-class-name)
)
