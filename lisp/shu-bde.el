;;; shu-bde.el --- Shu project code for dealing wth BDE style code in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-bde
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
;;

;;; Commentary:

;; A collection of useful functions for generating C++ skeleton code files
;; and classes for code written in Bloomberg, L.P. BDE style.

;;; Code:


(provide 'shu-bde)
(require 'shu-base)

(defcustom shu-bde-gen-file-identifier-hook nil
  "Generate the text that constitutes a source file identifier, if any."
  :type 'hook
  :group 'shu-bde)


(defcustom shu-bde-gen-h-includes-hook nil
  "Generate the code for the standard includes in a header file."
  :type 'hook
  :group 'shu-bde)


(defcustom shu-bde-gen-hfile-copyright-hook nil
  "Generate the text that is the copyright notice placed in a header file,
if any."
  :type 'hook
  :group 'shu-bde)


(defcustom shu-bde-gen-cfile-copyright-hook nil
  "Generate the text that is the copyright notice placed in a code file,
if any."
  :type 'hook
  :group 'shu-bde)


(defcustom shu-bde-gen-tfile-copyright-hook nil
  "Generate the text that is the copyright notice placed in a unit test
file, if any."
  :type 'hook
  :group 'shu-bde)


;;
;;  shu-bde-add-guard
;;
(defun shu-bde-add-guard ()
  "Add the BDE include guards around an existing #include directive.  If the line
before the #include directive contains a valid guard, then we do not add a guard
and position point to the line following the #include.  This makes it possible to
run bde-all-guard on a file that contains some guarded #includes and some unguarded
#includes.  Only the unguarded ones will have the guard added."
  (interactive)
  (let
      ((gg (concat "#include\s*<\\(" shu-cpp-file-name "+\\)>"))
       (eol (save-excursion (end-of-line) (point)))
       (eol2 )
       (guard )
       (fn )
       (gmatch )
       (bol ))
    (beginning-of-line)
    (if (re-search-forward gg eol t)
        (progn  ;; Found an include directive that we might guard
          (setq fn (match-string 1))  ;; Remember the file name
          (setq guard (shu-bde-include-guard fn))
          (setq gmatch (concat "#ifndef\s+" guard))
          (setq bol (save-excursion (beginning-of-line) (point)))
          (forward-line -1)
          (if (= bol (point)) ;; We are already at top of file
              (shu-bde-insert-guard fn t)
            ;;  We were able to go up one line
            (beginning-of-line)
            (setq eol2 (save-excursion (end-of-line) (point)))
            (if (re-search-forward gmatch eol2 t)
                (progn
                  (message "Include is already guarded")
                  (forward-line 2))
              ;;
              (end-of-line)
              (insert "\n")
              (shu-bde-insert-guard fn nil))))
      ;;
      (message "%s" "No #include on this line")
      (ding))))


;;
;;  shu-bde-insert-guard
;;
(defun shu-bde-insert-guard(fn at-top)
  "Insert a #ifndef / #endif guard around an #include directive.  FN is the name of
the included file.  AT-TOP is true if the #include directive is located on the first
line of the file so there is no line above it."
  (let*
      ((guard-name (shu-bde-include-guard fn))
       (guard-string (concat "#ifndef " guard-name)))
    (when at-top
      (setq guard-string (concat guard-string "\n")))
    (beginning-of-line)
    (insert guard-string)
    (forward-line 1)
    (end-of-line)
    (if at-top
        (insert "#endif")
      ;;
      (insert "\n#endif"))
    (forward-line 1)))


;;
;;  shu-bde-all-guard
;;
(defun shu-bde-all-guard ()
  "Add the BDE include guards around all of the #include directives in a file
or narrowed region."
  (interactive)
  (let
      ((gg (concat "#include\s*<\\(" shu-cpp-file-name "+\\)>"))
       (eoln (save-excursion (end-of-line) (point))))
    (while (re-search-forward gg nil t)
      (shu-bde-add-guard))))


;;
;;  shu-bde-include
;;
(defun shu-bde-include (fn)
  "Insert at the current line, the BDE include guard sequence of
#ifndef INCLUDED_GUARD
#include <guard.h>
#endif"
  (interactive "sName?: ")
  (beginning-of-line)
  (insert (concat "#ifndef " (shu-bde-include-guard fn) "\n"))
  (insert (concat "#include <" fn ">\n"))
  (insert "#endif\n"))


;;
;;  shu-bde-include-guard-fn
;;
(defun shu-bde-include-guard-fn (&optional fn)
  "Return the file name name of the macro variable to be used in a BDE style include
guard.  Name of the current buffer file name is used if no file name is passed in as
the only optional argument.  This is only the file name part of the include guard.
If the name of the file is foo_something.h, then this function returns
FOO_SOMETHING.  The full name of the macro variable would be
INCLUDED_FOO_SOMETHING.  See also shu-bde-include-guard"
  (let ((file-name (or fn (file-name-nondirectory (buffer-file-name))))
        (guard ))
    (setq guard (upcase (file-name-sans-extension file-name)))
    ))

;;
;;  shu-bde-include-guard
;;
(defun shu-bde-include-guard (&optional fn)
  "Return the name of the macro variable to be used in a BDE style include guard.
Name of the current buffer file name is used if no file name is passed in as the
only optional argument.  This is the name of the macro variable that is used in the
include guard.  If the name of the file is foo_something.h, then this function
returns INCLUDED_FOO_SOMETHING.  See also shu-bde-include-guard-fn"
  (let
      ((guard (concat "INCLUDED_" (shu-bde-include-guard-fn fn))))
    guard))



;;
;;  shu-bde-decl
;;
(defun shu-bde-decl (class-name)
  "Generate a skeleton BDE class declaration at point."
  (interactive "*sClass name?: ")
  (let
      ((std-name "std")
       (ostream-length (length "std::ostream  "))
       (ostream-class-length 0)
       (ostream-pad "")
       (ostream-class-pad "")
       (equal-pad )
       (dash-pad )
       (blank24 )
       (creator-a )
       (starts-with-vowel )
       (copy-ctor )
       (op-equal )
       (class-name-pad )
       (start-pos ))
    (when shu-cpp-use-bde-library
      (setq std-name "bsl"))
    (setq class-name-pad (make-string (length class-name) ? ))
    (setq equal-pad (make-string (+ 6 (length class-name)) ?=))
    (setq dash-pad (make-string (+ 6 (length class-name)) ?-))
    (setq blank24 (make-string 24 ? ))
    (setq ostream-class-length (+ (length "const ") (length class-name) 2))
    (if (> ostream-length ostream-class-length)
        (setq ostream-class-pad (make-string (- ostream-length ostream-class-length) ? ))
      (when (> ostream-class-length ostream-length)
        (setq ostream-pad (make-string (- ostream-class-length ostream-length) ? ))))
    (setq starts-with-vowel (string-match (substring class-name 0 1) "aeioAEIO"))
    (setq creator-a "a")
    (when starts-with-vowel (setq creator-a "an"))

    (setq copy-ctor (concat
                     "    explicit " class-name "(const " class-name " &original);\n"))
    (when (> (length copy-ctor) 79)
      (setq copy-ctor (concat
                       "    explicit " class-name "(\n"
                       "        const " class-name " &original);\n")))

    (setq op-equal (concat
                    "    " class-name " &operator=(const " class-name " &rhs);\n"))
    (when (> (length op-equal) 79)
      (setq op-equal (concat
                      "    " class-name " &operator=(\n"
                      "        const " class-name " &rhs);\n")))

    (insert
     (concat
      "\n"
      "\n"
      blank24 "// " equal-pad "\n"
      blank24 "// class " class-name "\n"
      blank24 "// " equal-pad "\n"
      "\n"
      "class " class-name " {\n"
      "    // This class ...\n"))
    (setq start-pos (save-excursion (forward-line -1) (forward-char 18) (point)))
    (insert
     (concat
      "\n"
      "    // DATA\n"
      "    // SomeType          d_someName;\n"
      "        // Description of 'd_someName'\n"
      "\n"
      "  public:\n"
      "\n"
      "    //CREATORS\n"
      "    explicit " class-name "();\n"
      "        // Create " creator-a " '" class-name "' object ...\n"
      "\n"
      "    // ~" class-name "();\n"
      "        // Destroy this object.\n"
      "\n"
      "    // MANIPULATORS\n"
      "\n"
      "    // ACCESSORS\n"
      "\n"
      "    " std-name "::ostream &printSelf(" std-name "::ostream    &os) const;\n"
      "        // Stream the object to the given stream.  Intended for use by\n"
      "        // 'operator<<()'.\n"
      "\n"
      "  private:\n"
      "\n"
      "    // NOT IMPLEMENTED\n"
      copy-ctor
      "        // Copy constructor is explicit, private, and unimplememted.\n"
      "\n"
      op-equal
      "        // operator=() is private and unimplemented.\n"
      "\n"
      "    // MANIPULATORS\n"
      "\n"
      "    // ACCESSORS\n"
      "\n"
      "};\n"
      "\n"
      "// FREE OPERATORS\n"
      "inline\n"
      std-name "::ostream &operator<<(" std-name "::ostream" ostream-pad "  &os,\n"
      "                         const " class-name ostream-class-pad "  &cn);\n"
      "    // Stream an instance of '" class-name "' to the stream 'os'.\n"))
    (goto-char start-pos)))


;;
;;  shu-bde-gen
;;
(defun shu-bde-gen (class-name)
  "Generate a skeleton BDE class code generation at point."
  (interactive "*sClass name?: ")
  (let ((std-name "std")
        (ostream-length (length "std::ostream  "))
        (ostream-class-length 0)
        (ostream-pad "")
        (ostream-class-pad "")
        (equal-pad )
        (dash-pad )
        (blank24 )
        (class-name-pad )
        (start-pos ))
    (when shu-cpp-use-bde-library
      (setq std-name "bsl"))
    (setq class-name-pad (make-string (length class-name) ? ))
    (setq equal-pad (make-string (+ 6 (length class-name)) ?=))
    (setq dash-pad (make-string (+ 6 (length class-name)) ?-))
    (setq blank24 (make-string 24 ? ))
    (setq ostream-class-length (+ (length "const ") (length class-name) 2))
    (if (> ostream-length ostream-class-length)
        (setq ostream-class-pad (make-string (- ostream-length ostream-class-length) ? ))
      (when (> ostream-class-length ostream-length)
        (setq ostream-pad (make-string (- ostream-class-length ostream-length) ? ))))


    (insert
     (concat
      "\n"
      "// ===========================================================================\n"
      "//                  INLINE AND TEMPLATE FUNCTION IMPLEMENTATIONS\n"
      "// ===========================================================================\n"
      "\n"
      "\n"
      blank24 "// " dash-pad "\n"
      blank24 "// class " class-name "\n"
      blank24 "// " dash-pad "\n"
      "\n"
      "// CREATORS\n"
      "inline\n"
      class-name "::" class-name "()\n"))
    (setq start-pos (save-excursion (forward-line -1) (end-of-line) (forward-char -1) (point)))
    (insert
     (concat
      "{\n"
      "}\n"
      "\n"
      "    // MANIPULATORS\n"
      "\n"
      "    // ACCESSORS\n"
      "\n"
      "inline\n"
      std-name "::ostream &" class-name "::printSelf(" std-name "::ostream    &os) const\n"
      "{\n"
      "    os << \"Instance of '" class-name "'\";\n"
      "\n"
      "    return os;\n"
      "}\n"
      "\n"
      "// FREE OPERATORS\n"
      "inline\n"
      std-name "::ostream &operator<<(" std-name "::ostream" ostream-pad "  &os,\n"
      "                         const " class-name ostream-class-pad "  &cn)\n"
      "{\n"
      "    return cn.printSelf(os);\n"
      "}\n"
      ))
    (goto-char start-pos)
    ))

;;
;;  shu-bde-sdecl
;;
(defun shu-bde-sdecl (class-name)
  "Generate a skeleton BDE struct definition at point."
  (interactive "*sStruct name?: ")
  (let ((equal-pad )
        (blank24 )
        (class-name-pad )
        (start-pos ))
    (setq class-name-pad (make-string (length class-name) ? ))
    (setq equal-pad (make-string (+ 7 (length class-name)) ?=))
    (setq blank24 (make-string 24 ? ))

    (insert
     (concat
      "\n"
      "\n"
      blank24 "// " equal-pad "\n"
      blank24 "// struct " class-name "\n"
      blank24 "// " equal-pad "\n"
      "\n"
      "struct " class-name " {\n"
      "    // This 'struct' provides a namespace for utility functions that\n"
      "    // \n"))

    (setq start-pos (save-excursion (forward-line -1) (end-of-line) (point)))

    (insert
     (concat
      "\n"
      "    // CLASS METHODS\n"
      "\n"
      "\n"
      "};\n"
      ))
    (goto-char start-pos)
    ))


;;
;;  shu-bde-sgen
;;
(defun shu-bde-sgen (class-name)
  "Generate a skeleton BDE struct code generation at point."
  (interactive "*sStruct name?: ")
  (let ((dash-pad )
        (blank24 )
        (class-name-pad )
        (start-pos ))
    (setq class-name-pad (make-string (length class-name) ? ))
    (setq dash-pad (make-string (+ 7 (length class-name)) ?-))
    (setq blank24 (make-string 24 ? ))

    (insert
     (concat
      "\n"
      "// ===========================================================================\n"
      "//                  INLINE AND TEMPLATE FUNCTION IMPLEMENTATIONS\n"
      "// ===========================================================================\n"
      "\n"
      "\n"
      blank24 "// " dash-pad "\n"
      blank24 "// struct " class-name "\n"
      blank24 "// " dash-pad "\n"
      "\n"
      "// CLASS METHODS\n"
      "inline\n"
      "void " class-name "::???()\n"))
    (setq start-pos (save-excursion (forward-line -1) (end-of-line) (forward-char -5) (point)))
    (insert
     (concat
      "{\n"
      "}\n"
      "\n"
      ))
    (goto-char start-pos)
    ))






;;
;;  shu-gen-bde-component
;;
(defun shu-gen-bde-component (class-name)
  "Generate the three files for a new component: .cpp, .h, and .t.cpp"
  (interactive "sClass name?: ")
  (let* ((gitbuf (get-buffer-create "**git-add**"))
         (debug-on-error t)
         (author shu-cpp-author)
         (namespace shu-cpp-default-namespace)
         (base-class-name (downcase class-name))
;;         (file-prefix (if shu-cpp-completion-prefix shu-cpp-completion-prefix (concat namespace "_")))
         (file-prefix (if shu-cpp-completion-prefix shu-cpp-completion-prefix ""))
         (base-name (concat file-prefix base-class-name))
         (hfile-name (concat base-name ".h"))
         (cfile-name (concat base-name ".cpp"))
         (tfile-name (concat base-name ".t.cpp"))
         (got-files )
         (found-files "")
         (file-comma "")
         (got-count 0)
         (file-file "File"))
    (when (not namespace)
      (setq base-name base-class-name)
      (setq hfile-name (concat base-name ".h"))
      (setq cfile-name (concat base-name ".cpp"))
      (setq tfile-name (concat base-name ".t.cpp")))
    (when (file-readable-p hfile-name)
      (setq got-files t)
      (setq found-files (concat found-files file-comma hfile-name))
      (setq got-count (1+ got-count))
      (setq file-comma ", "))
    (when (file-readable-p cfile-name)
      (setq got-files t)
      (setq found-files (concat found-files file-comma cfile-name))
      (setq got-count (1+ got-count))
      (setq file-comma ", "))
    (when (file-readable-p tfile-name)
      (setq got-files t)
      (setq found-files (concat found-files file-comma tfile-name))
      (setq got-count (1+ got-count))
      (setq file-comma ", "))

    (if got-files
        (progn
          (when (> got-count 1)
            (setq file-file "Files"))
          (message "%s already here: %s" file-file found-files))
      (when (not namespace)
        (message "%s" "Warning: No namespace set.  See shu-det-default-namespace"))
      (find-file cfile-name)
      (shu-generate-bde-cfile author namespace class-name)
      (save-buffer)
      (goto-char (point-min))
      (find-file tfile-name)
      (shu-generate-bde-tfile author namespace class-name)
      (save-buffer)
      (find-file hfile-name)
      (goto-char (point-min))
      (shu-generate-bde-hfile author namespace class-name)
      (save-buffer)
      (goto-char (point-min))

      (princ (concat "git add " hfile-name "\n") gitbuf)
      (princ (concat "git add " cfile-name "\n") gitbuf)
      (princ (concat "git add " tfile-name "\n") gitbuf)

      )))



;;
;;  shu-generate-bde-hfile
;;
(defun shu-generate-bde-hfile (author namespace class-name)
  "Generate a skeleton header file"
  (let* ((hfile-name (file-name-nondirectory (buffer-file-name)))
         (guard-name (shu-bde-include-guard hfile-name))
         (open-line (concat (shu-make-padded-line (concat "// " hfile-name) 70) "-*-C++-*-"))
         (namespace-name namespace)
         (namespace-sep "::")
         (inner-close "}  // close package namespace\n")
         (outer-namespace)
         (outer-close "}  // close enterprise namespace\n")
         (inner-namespace "")
         (decl-point))

    (if shu-cpp-default-global-namespace
        (setq outer-namespace (concat "namespace " shu-cpp-default-global-namespace " {\n"))
      (setq outer-close ""))
    (if namespace
        (setq inner-namespace (concat "namespace " namespace-name " {\n"))
      (setq namespace-sep "")
      (setq namespace-name "")
      (setq inner-close ""))

    (insert
     (concat
      open-line "\n"
      "#ifndef " guard-name "\n"
      "#define " guard-name "\n"
      "\n"
      "/*!\n"
      " * \\file " hfile-name "\n"
      " *\n"
      " * \\brief Declaration of " class-name "\n"
      " *\n"
      " * \\author " author "\n"
      " */\n"
      "\n"))
    (run-hooks 'shu-bde-gen-file-identifier-hook)
    (insert
     (concat
      "\n"
      "//@PURPOSE: Provide an  ...\n"
      "//         \n"
      "//@CLASSES:\n"
      "//   " namespace-name  namespace-sep class-name "\n"
      "//\n"
      "//@AUTHOR: " author "\n"))
    (run-hooks 'shu-bde-gen-h-includes-hook)
    (insert
     (concat
      "\n"
      "\n"
      outer-namespace
      inner-namespace
      "\n"
      "\n"))

    (setq decl-point (point))

    (insert
     (concat
      "\n"
      "\n"
      inner-close
      outer-close
      "\n"
      "#endif  // " guard-name "\n"
      "\n"))
    (run-hooks 'shu-bde-gen-hfile-copyright-hook)
    (insert
     (concat
      "// ----------------------------- END-OF-FILE ---------------------------------\n"))

    (goto-char decl-point)
    (beginning-of-line)
    (shu-cpp-cdecl class-name)
    ))



;;
;;  shu-generate-bde-cfile
;;
(defun shu-generate-bde-cfile (author namespace class-name)
  "Generate a skeleton cpp file"
  (let* ((cfile-name (file-name-nondirectory (buffer-file-name)))
         (hfile-name (concat (file-name-sans-extension cfile-name) ".h"))
         (rcs-file-name (concat (file-name-sans-extension cfile-name) "_cpp"))
         (guard-name (shu-bde-include-guard cfile-name))
         (open-line (concat (shu-make-padded-line (concat "// " cfile-name) 70) "-*-C++-*-"))
         (inner-namespace "")
         (inner-close-namespace "")
         (outer-namespace)
         (outer-close "}  // close enterprise namespace\n")
         (left-include-delim "\"")
         (right-include-delim "\""))
    (when shu-cpp-include-user-brackets
      (setq left-include-delim "<")
      (setq right-include-delim ">"))
    (when namespace
      (setq inner-namespace (concat "namespace " namespace " {\n"))
      (setq inner-close-namespace (concat "}  // close package namespace\n")))
    (if shu-cpp-default-global-namespace
        (setq outer-namespace (concat "namespace " shu-cpp-default-global-namespace " {\n"))
      (setq outer-close ""))

    (insert
     (concat
      open-line "\n"
      "\n"
      "/*!\n"
      " * \\file " cfile-name "\n"
      " *\n"
      " * \\brief Compilation file for " class-name "\n"
      " *\n"
      " * \\author " author "\n"
      " */\n"
      "\n"
      "#include "left-include-delim hfile-name right-include-delim "\n"
      "\n"))
    (run-hooks 'shu-bde-gen-file-identifier-hook)
    (insert
     (concat
      "\n"
      "\n"
      outer-namespace
      inner-namespace
      "\n"
      "\n"
      "\n"
      "namespace {\n"
      "\n"
      "}  // close anonymous namespace\n"
      "\n"
      "\n"
      "\n"
      "\n"
      inner-close-namespace
      outer-close
      "\n"
      "\n"))
    (insert
     (concat
      (run-hooks 'shu-bde-gen-cfile-copyright-hook)
      "// ----------------------------- END-OF-FILE ---------------------------------\n"))
    ))




;;
;;  shu-generate-bde-tfile
;;
(defun shu-generate-bde-tfile (author namespace class-name)
  "Generate a skeleton t.cpp file"
  (interactive)
  (let* ((tfile-name (file-name-nondirectory (buffer-file-name)))
         (tbase-name (file-name-sans-extension tfile-name))
         (ext2 (file-name-extension tfile-name))
         (ext1 (file-name-extension tbase-name))
         (base-name (file-name-sans-extension tbase-name))
         (hfile-name (concat base-name ".h"))
         (open-line (concat (shu-make-padded-line (concat "// " tfile-name) 70) "-*-C++-*-"))
         (namespace-sep "::")
         (inner-using "")
         (outer-using "")
         (outer-qualifier)
         (left-include-delim "\"")
         (right-include-delim "\""))
    (when shu-cpp-include-user-brackets
      (setq left-include-delim "<")
      (setq right-include-delim ">"))
    (when shu-cpp-default-global-namespace
      (setq outer-using (concat "using namespace " shu-cpp-default-global-namespace ";\n")))
    (when shu-cpp-default-global-namespace
      (setq outer-qualifier (concat shu-cpp-default-global-namespace "::")))

    (if namespace
        (setq inner-using (concat "using namespace " outer-qualifier namespace ";\n"))
      (setq namespace-sep ""))

    (insert
     (concat
      open-line "\n"
      "\n"
      "/*!\n"
      " * \\file " tfile-name "\n"
      " *\n"
      " * \\brief Unit tests for " class-name "\n"
      " *\n"
      " * \\author " author "\n"
      " */\n"
      "\n"
      "#include " left-include-delim hfile-name right-include-delim "\n"
      "\n"
      "#include <gtest/gtest.h>\n"
      "\n"
      "\n"
      outer-using
      inner-using
      "\n"
      "\n"
      "\n"
      "//=============================================================================\n"
      "//                        HELPER FUNCTIONS AND CLASSES\n"
      "//=============================================================================\n"
      "namespace {\n"
      "\n"
      "\n"
      "\n"
      "}  // close unnamed namespace\n"
      "\n"
      "\n"
      "\n"
      "\n"
      "// ============================================================================\n"
      "//                            MAIN PROGRAM\n"
      "// ----------------------------------------------------------------------------\n"
      "\n"
      "\n"
      "// --------------------------------------------------------------------\n"
      "// TEST\n"
      "// --------------------------------------------------------------------\n"
      "TEST(Test" class-name ", initialConstruction)\n"
      "{\n"
      "\n"
      "}\n"
      "\n"
      "\n"
      "\n"))
    (run-hooks 'shu-bde-gen-tfile-copyright-hook)
    (insert
     (concat
      "// ----------------------------- END-OF-FILE ----------------------------------\n"))
    ))


;;
;;  shu-bde-set-alias
;;
(defun shu-bde-set-alias ()
  "Set the common alias names for the functions in shu-bde.
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'bde-add-guard 'shu-bde-add-guard)
  (defalias 'bde-all-guard 'shu-bde-all-guard)
  (defalias 'bde-include 'shu-bde-include)
  (defalias 'bde-decl 'shu-bde-decl)
  (defalias 'bde-gen 'shu-bde-gen)
  (defalias 'bde-sdecl 'shu-bde-sdecl)
  (defalias 'bde-sgen 'shu-bde-sgen)
  (defalias 'gen-bde-component 'shu-gen-bde-component)
  )

;;; shu-bde.el ends here
