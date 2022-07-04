;;; shu-bde-new.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2022 Stewart L. Palmer
;;
;; Package: shu-date
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


;;; Code:



;;
;;  try
;;
(defun try ()
  "Doc string."
  (interactive)
  (let (
        (a-val)
        (b-val)
        )
    (setq a-val (read-string "a? "))
    (setq b-val (read-string "b? "))
    (message "a-val '%s', b-val: '%s'" a-val b-val)
    ))




;;
;;  ccc
;;
(defun ccc ()
  "Doc string."
  (interactive)
  (let (
        (gb (get-buffer-create "**goo**"))
        (str "a, b, c, d")
        (str2 "a b c d")
        (aa)
        (a)
        )
    (setq aa (split-string str "[,]+" t (concat shu-all-whitespace-regexp "+")))
    (princ aa gb)(princ "\n" gb)
    (while aa
      (setq a (car aa))
      (princ (concat "'" a "'\n") gb)
      (setq aa (cdr aa))
      )
    (setq aa (split-string str2 "[,]+" t (concat shu-all-whitespace-regexp "+")))
    (princ aa gb)(princ "\n" gb)
    (while aa
      (setq a (car aa))
      (princ (concat "'" a "'\n") gb)
      (setq aa (cdr aa))
      )

    ))



;;
;;  shu-gen-bde-create-prompt
;;
(defun shu-gen-bde-create-prompt ()
  "This function fetches the prompt string from SHU-GEN-BDE-MAKE-PROMPT-STRING
issues the query, and returns the result."
  (let ((query (shu-gen-bde-make-prompt-string)))
    (read-string query)
    ))



;;
;;  shu-gen-bde-create-prompt-template
;;
(defun shu-gen-bde-create-prompt-template ()
  "This function fetches the prompt string from SHU-GEN-BDE-MAKE-PROMPT-STRING
issues the query, and then issues a query for the comma separated list of
template parameter names.  It returns a list with twoitens on it:

    1. The name of the new component

    2. The list of template parameter names

If the comma separated list of template parameter names is empty, the list of
template parameter names (Item 2 above) is nil"
  (let (
        (gb (get-buffer-create "**boo**"))
        (query (shu-gen-bde-make-prompt-string))
        (tquery "Comma separated template parameter names? ")
        (a1)
        (a2)
        (plist)
        (answers)
        )
    (setq a1 (read-string query))
    (princ (concat "a1: '" a1 "'\n") gb)
    (setq a2 (read-string tquery))
    (princ (concat "a2: '" a2 "'\n") gb)
    (push a2 answers)
    (push a1 answers)
    (princ "answers: " gb)(princ answers gb)(princ "\n" gb)
    answers
    ))



;;
;;  shu-cpp-split-template-parameter-list
;;
(defun shu-cpp-split-template-parameter-list (tp-string)
  "TP-STRING is a comma separated list of template parameter names.  This
function splits the string into a list of names and returns that list."
  (let ((tp-list  (split-string tp-string "[,]+" t (concat shu-all-whitespace-regexp "+"))))
    tp-list
    ))



;;
;;  shu-gen-bde-make-prompt-string
;;
(defun shu-gen-bde-make-prompt-string ()
  "This function creates the prompt for the interactive special form of the
function SHU-GEN-BDE-COMPONENT.  The prompt includes the namespace in which the
new class will be created or the string \"NO NAMESPACE\" if there is no default
namespace set.  If the name of the current directory does not match the default
namespace, the prompt also includes the directory name to remind the user that
the current directory name does not match the namespace."
  (let ((query)
        (namespace (if shu-cpp-default-namespace shu-cpp-default-namespace "NO NAMESPACE"))
        (prefix (shu-get-directory-prefix)))
    (setq query (concat "Class name in namespace " namespace "? "))
    (when (and shu-cpp-default-namespace (not (string= prefix namespace)))
      (setq query (concat "Class name in namespace " namespace  " in directory '" prefix "'? ")))
    query
    ))



;;
;;  shu-gen-bde-template
;;
(defun shu-gen-bde-template (class-name template-string)
  "Generate the three files for a new component: .cpp, .h, and .t.cpp"
  (interactive (shu-gen-bde-create-prompt-template))
  (let (
        (gb (get-buffer-create "**boo**"))
        (template-list (shu-cpp-split-template-parameter-list template-string))
        (author shu-cpp-author)
        (namespace shu-cpp-default-namespace)
        (file-prefix (if shu-cpp-completion-prefix shu-cpp-completion-prefix "")
                       )
        )
    (princ (concat "class-name: " class-name "\n") gb)
    (princ "template-list: " gb)(princ template-list gb)(princ "\n" gb)
    (shu-internal-gen-bde-template class-name template-list author namespace file-prefix)
    ))




;;
;;  shu-internal-gen-bde-template
;;
(defun shu-internal-gen-bde-template (class-name template-list author namespace file-prefix)
  "Generate the three files for a new component: .cpp, .h, and .t.cpp"
  (let* ((gitbuf (get-buffer-create "**git-add**"))
         (debug-on-error t)
         (base-class-name (downcase class-name))
         ;;         (file-prefix (if shu-cpp-completion-prefix shu-cpp-completion-prefix (concat namespace "_")))
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
      (shu-generate-bde-cfile-template author namespace class-name template-list)
      (save-buffer)
      (goto-char (point-min))
      (find-file tfile-name)
      (shu-generate-bde-tfile author namespace class-name)
      (save-buffer)
      (find-file hfile-name)
      (goto-char (point-min))
      (shu-generate-bde-hfile-template author namespace class-name template-list)
      (save-buffer)
      (goto-char (point-min))

      (princ (concat "git add " hfile-name "\n") gitbuf)
      (princ (concat "git add " cfile-name "\n") gitbuf)
      (princ (concat "git add " tfile-name "\n") gitbuf)

      (princ (concat "git reset HEAD " hfile-name "\n") gitbuf)
      (princ (concat "git reset HEAD " cfile-name "\n") gitbuf)
      (princ (concat "git reset HEAD " tfile-name "\n") gitbuf)

      (princ (concat "git restore --staged " hfile-name "\n") gitbuf)
      (princ (concat "git restore --staged " cfile-name "\n") gitbuf)
      (princ (concat "git restore --staged " tfile-name "\n") gitbuf)

      (princ (concat "rm " hfile-name "\n") gitbuf)
      (princ (concat "rm " cfile-name "\n") gitbuf)
      (princ (concat "rm " tfile-name "\n") gitbuf)

      (shu-generate-git-add hfile-name gitbuf)
      (shu-generate-git-add cfile-name gitbuf)
      (shu-generate-git-add tfile-name gitbuf)
      )))



;;
;;  shu-generate-bde-hfile-template
;;
(defun shu-generate-bde-hfile-template (author namespace class-name template-list)
  "Generate a skeleton header file"
  (let* (
         (qualified-class-name (shu-cpp-make-qualified-class-name class-name template-list))
         (hfile-name (file-name-nondirectory (buffer-file-name)))
         (guard-name (shu-bde-include-guard hfile-name))
         (open-line (concat (shu-make-padded-line (concat "// " hfile-name) 70) "-*-C++-*-"))
         (namespace-name namespace)
         (namespace-sep "::")
         (inner-close "}  // close package namespace\n")
         (outer-namespace)
         (outer-close "}  // close enterprise namespace\n")
         (inner-namespace "")
         (decl-point)
           )

    (if shu-cpp-default-global-namespace
        (setq outer-namespace (concat "namespace " shu-cpp-default-global-namespace " {\n"))
      (setq outer-close "")
      )
    (if namespace
        (setq inner-namespace (concat "namespace " namespace-name " {\n"))
      (setq namespace-sep "")
      (setq namespace-name "")
      (setq inner-close "")
      )

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
    (shu-cpp-cdecl-template class-name template-list)
    ))



;;
;;  shu-generate-bde-cfile-template
;;
(defun shu-generate-bde-cfile-template (author namespace class-name template-list)
  "Generate a skeleton cpp file"
  (let* (
         (qualified-class-name (shu-cpp-make-qualified-class-name class-name template-list))
         (cfile-name (file-name-nondirectory (buffer-file-name)))
         (hfile-name (concat (file-name-sans-extension cfile-name) ".h"))
         (rcs-file-name (concat (file-name-sans-extension cfile-name) "_cpp"))
         (guard-name (shu-bde-include-guard cfile-name))
         (open-line (concat (shu-make-padded-line (concat "// " cfile-name) 70) "-*-C++-*-"))
         (inner-namespace "")
         (inner-close-namespace "")
         (outer-namespace)
         (outer-close "}  // close enterprise namespace\n")
         (left-include-delim "\"")
         (right-include-delim "\"")
         (cgen-point)
           )
    (when shu-cpp-include-user-brackets
      (setq left-include-delim "<")
      (setq right-include-delim ">")
      )
    (when namespace
      (setq inner-namespace (concat "namespace " namespace " {\n"))
      (setq inner-close-namespace (concat "}  // close package namespace\n"))
      )
    (if shu-cpp-default-global-namespace
        (setq outer-namespace (concat "namespace " shu-cpp-default-global-namespace " {\n"))
      (setq outer-close "")
      )

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
      "#include " left-include-delim hfile-name right-include-delim "\n"
      "\n"))
    (run-hooks 'shu-bde-gen-file-identifier-hook)
    (insert
     (concat
      "\n"
      "#include <bslma_default.h>\n"
      "\n"
      "\n"
      "\n"
      outer-namespace
      inner-namespace
      "\n"
      "\n"))
    (save-excursion
      (beginning-of-line)
      (setq cgen-point (point))

      )
    (insert
     (concat
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
    (goto-char cgen-point)
    (shu-cpp-acgen-template class-name template-list)
    ))



;;
;;  shu-cpp-acgen-template
;;
(defun shu-cpp-acgen-template (class-name template-list)
  "Generate a skeleton class code generation at point."
  (interactive "*sClass name?: ")
  (let
      (
       (use-allocator shu-cpp-use-bde-library)
       (start-pos )
       (header-pos (point))
       (have-include )
       (include-line )
       (ipad (make-string shu-cpp-indent-length ? ))
       )
    (shu-cpp-decl-cpp-class-name class-name)
    (insert (concat
             "\n"
             "// CREATORS\n\n"))
    (setq start-pos (point))
    (when (not template-list)
      (shu-cpp-impl-cpp-constructor class-name template-list use-allocator)
               )
    (insert
     (concat
      "\n"
      "// MANIPULATORS\n"
      "\n"
      "// ACCESSORS"
      "\n"))
    (when (not template-list)
      (shu-cpp-decl-cpp-print-self class-name)
      )
    (insert
     (concat
      "\n"
      "// FREE OPERATORS\n\n\n"
      ))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (search-forward "#include <bslma_default.h>" nil t)
          (setq have-include t)
          )
        )
      )
    (when (not have-include)
      (goto-char header-pos)
      (beginning-of-line)
      (setq include-line
            (concat
             "\n"
             "#include <bslma_default.h>\n"))
      (setq start-pos (+ start-pos (length include-line)))
      (insert include-line)
      )
    (goto-char start-pos)
    ))




;;
;;  shu-cpp-cdecl-template - Skeleton class that may not be copied
;;
(defun shu-cpp-cdecl-template (class-name template-list)
  "Generate a skeleton class declaration at point."
  (interactive "*sClass name?: ")
  (let (
        (use-allocator shu-cpp-use-bde-library)
        )
    (shu-cpp-inner-cdecl-template class-name template-list nil use-allocator)))




;;
;;  shu-cpp-ccdecl - Skeleton class that may be copied
;;
(defun shu-cpp-ccdecl-template (class-name template-list)
  "Generate a skeleton class declaration at point."
  (interactive "*sClass name?: ")
  (let (
        (use-allocator shu-cpp-use-bde-library)
        )
  (shu-cpp-inner-cdecl-template class-name template-list t use-allocator)
  ))




;; TODO: Honor the column in which point is located for generating
;;       nested class declarations?
;;
;; TODO: Allow a template class spec.  Something like a class name of
;;       FooBar<DataType, thingType>
;;
;;      This would generate
;;
;;      template<class DataType, class thingType>
;;      class FooBar
;;
;;  shu-cpp-inner-cdecl
;;
(defun shu-cpp-inner-cdecl-template (class-name template-list copy-allowed use-allocator)
  "Generate a skeleton class declaration at point."
  (let ((ipad (make-string shu-cpp-indent-length ? ))
        (header-pos (point))
        (have-include )
        (start-pos ))
    (setq start-pos (shu-cpp-gen-h-class-intro-template class-name template-list))
    (insert (concat "\n" ipad "// DATA\n\n"))
    (when use-allocator
      (insert
       (concat
        ipad "bslma::Allocator                 *" shu-cpp-default-allocator-name ";\n"
        )))
    (insert
     (concat
      "\n"
      "  public:\n"))
    (when use-allocator
      (shu-cpp-misc-gen-nested-traits class-name))
    (insert
     (concat
      "\n"
      ipad "// CREATORS\n"))
    (shu-cpp-misc-gen-h-ctor class-name use-allocator)
    (when (and shu-cpp-modern (not copy-allowed))
      (shu-cpp-misc-gen-ctor-not-implemented class-name))
    (shu-cpp-misc-gen-h-dtor class-name)
    (insert
     (concat
      "\n"
      ipad "// MANIPULATORS\n"))
    (when (and shu-cpp-modern (not copy-allowed))
      (shu-cpp-misc-gen-op-equal-not-implemented class-name))
    (insert
     (concat
      "\n"
      ipad "// ACCESSORS\n"))
    (shu-cpp-decl-h-print-self)
    (shu-cpp-gen-decl-h-private class-name copy-allowed)
    (insert
     (concat
      "\n"
      "// FREE OPERATORS\n"))
    (shu-cpp-decl-h-stream-template class-name template-list)
    (insert
     (concat
      "\n"
      "\n"))
    (shu-cpp-hcgen-template class-name template-list use-allocator)
    (when use-allocator
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (search-forward "#include <bslma_allocator.h>" nil t)
            (setq have-include t))))
      (when (not have-include)
        (goto-char header-pos)
        (beginning-of-line)
        (insert
         (concat
          "\n"
          "#include <bslma_allocator.h>\n"))))
    (goto-char start-pos)
    ))




;;
;;  shu-cpp-gen-h-class-intro-template
;;
(defun shu-cpp-gen-h-class-intro-template (class-name template-list)
  "Generate the preamble to a class declaration in a header file.  This is all
of the code that precedes the \\ DATA comment.  Return the position at which
the class comment was placed."
  (let ((start-pos))
    (insert "\n")
    (shu-cpp-decl-h-class-name class-name)
    (insert
     (concat
      "\n"
      "/*!\n"
      " * \\brief "))
    (setq start-pos (point))
    (insert
     (concat
      "Description of " class-name " FIXME!  FIXME!  FIXME!  FIXME!\n"
      " */\n"))
    (when template-list
      (insert
       (concat
        (shu-cpp-make-decl-template template-list) "\n"))
      )
    (insert
     (concat
      "class " class-name "\n"
      "{\n"))
    start-pos
    ))



;;
;;  shu-cpp-hcgen-template
;;
(defun shu-cpp-hcgen-template (class-name template-list use-allocator)
  "Generate a skeleton class code generation at point."
  (interactive "*sClass name?: ")
  (let (
        (start-pos )
        (ipad (make-string shu-cpp-indent-length ? ))
        (have-header))
    (save-excursion
      (save-restriction
        (widen)
        (setq have-header (search-backward shu-cpp-misc-inline-template-label nil t))))
    (insert "\n")
    (when (not have-header)
      (shu-cpp-gen-inline-template-header)
      )
    (shu-cpp-decl-cpp-class-name class-name)
    (insert (concat
             "\n"
             "// CREATORS\n"))
    (setq start-pos (save-excursion (forward-line -1) (end-of-line) (forward-char -1) (point)))
    (when template-list
      (insert "\n")
      (shu-cpp-impl-cpp-constructor class-name template-list use-allocator)
      )
    (insert
     (concat
      "\n"
      "// MANIPULATORS\n"
      "\n"
      "// ACCESSORS\n"))
    (when template-list
    (insert "\n")
    (shu-cpp-impl-cpp-print-self class-name template-list)
    )
    (insert
     (concat
      "\n"
      "// FREE OPERATORS\n"
      "\n"
      ))
    (shu-cpp-decl-cpp-stream-template class-name template-list)
    (goto-char start-pos)
    ))




;;
;;  shu-cpp-impl-cpp-constructor
;;
(defun shu-cpp-impl-cpp-constructor (class-name template-list use-allocator)
  "Insert the skeleton constructor implementation."
  (let (
        (qualified-class-name (shu-cpp-make-qualified-class-name class-name template-list))
        (ipad (make-string shu-cpp-indent-length ? ))
        )
    (shu-cpp-insert-template-decl template-list)
    (insert
     qualified-class-name "::" class-name "(")
    (if use-allocator
        (progn
          (insert
           (concat
            "\n"
            ipad "bslma::Allocator    *allocator)\n"
            ":\n"
            shu-cpp-default-allocator-name "(bslma::Default::allocator(allocator))\n")))
      )
    (insert "{\n"
            "}\n"
            )
    ))




;;
;;  shu-cpp-impl-cpp-print-self
;;
(defun shu-cpp-impl-cpp-print-self (class-name template-list)
  "Generate the skeleton code for the printSelf() function.
CLASS-NAME is the name of the containing C++ class."
  (let (
        (std-name (if shu-cpp-use-bde-library shu-cpp-std-namespace "std"))
        (qualified-class-name (shu-cpp-make-qualified-class-name class-name template-list))
        (ipad (make-string shu-cpp-indent-length ? ))
          )
    (shu-cpp-insert-template-decl template-list)
    (insert
     (concat
      std-name "::ostream &" qualified-class-name "::printSelf(\n"
      ipad std-name "::ostream    &os)\n"
      "const\n"
      "{\n"
      ipad "os << \"Instance of '" class-name "'\";\n"
      "\n"
      ipad "return os;\n"
      "}\n"))
    ))



;;
;;  shu-cpp-decl-h-stream-template
;;
(defun shu-cpp-decl-h-stream-template (class-name template-list)
  "Generate the declaration for the streaming operator (operator<<()).
CLASS-NAME is the name of the containing C++ class."
  (let* (
        (std-name (if shu-cpp-use-bde-library shu-cpp-std-namespace "std"))
        (qualified-class-name (shu-cpp-make-qualified-class-name class-name template-list))
        (ipad (make-string shu-cpp-indent-length ? ))
        (ostream-length (length "std::ostream  "))
        (ostream-class-length (+ (length "const ") (length qualified-class-name) 2))
        (ostream-pad "")
        (ostream-class-pad "")
          )
    (if (> ostream-length ostream-class-length)
        (setq ostream-class-pad (make-string (- ostream-length ostream-class-length) ? ))
      (when (> ostream-class-length ostream-length)
        (setq ostream-pad (make-string (- ostream-class-length ostream-length) ? ))
        )
      )
    (insert
     (concat
      "\n"
      "/*!\n"
      " *  \\brief Stream an instance of " class-name " to the stream `os`\n"
      " */\n"))
    (when template-list
      (insert
       (concat
        (shu-cpp-make-decl-template template-list)
        "\n"
        ))
        )
    (insert
     (concat
      std-name "::ostream &operator<<(\n"
      ipad std-name "::ostream" ostream-pad "  &os,\n"
      ipad  "const " qualified-class-name ostream-class-pad "  &cn);\n"))
    ))



;;
;;  shu-cpp-decl-cpp-stream-template
;;
(defun shu-cpp-decl-cpp-stream-template (class-name template-list)
  "Generate the code for the streaming operator (operator<<()).  CLASS-NAME is the
name of the containing C++ class."
  (let* (
         (std-name (if shu-cpp-use-bde-library shu-cpp-std-namespace "std"))
        (qualified-class-name (shu-cpp-make-qualified-class-name class-name template-list))
        (ipad (make-string shu-cpp-indent-length ? ))
        (ostream-length (length "std::ostream  "))
        (ostream-class-length (+ (length "const ") (length qualified-class-name) 2))
        (ostream-pad "")
        (ostream-class-pad "")
        (inline "")
           )
    (if (> ostream-length ostream-class-length)
        (setq ostream-class-pad (make-string (- ostream-length ostream-class-length) ? ))
      (when (> ostream-class-length ostream-length)
        (setq ostream-pad (make-string (- ostream-class-length ostream-length) ? ))
        )
      )
    (shu-cpp-insert-template-decl template-list)
    (when (not template-list)
      (setq inline "inline\n")
      )
    (insert
     (concat
      inline
      std-name "::ostream &operator<<(\n"
      ipad std-name "::ostream" ostream-pad "  &os,\n"
      ipad  "const " qualified-class-name ostream-class-pad "  &cn)\n"
      "{\n"
      ipad "return cn.printSelf(os);\n"
      "}\n"))

                     ))



;;
;;  shu-cpp-make-qualified-class-name
;;
(defun shu-cpp-make-qualified-class-name (class-name template-list)
  "The input is a CLASS-NAME and TEMPLATE-LIST.  The output is a class name
followed by the comma separated list of template parameter names.  If the
template parameter names are T and S and the class name is MumbleMar, the
returned value is MumbleBar<T, S>.  If TEMPLATE-LIST is nil or empty,
the original class name is returned."
  (let ((tlist (shu-cpp-make-template-list template-list)))
    (concat class-name tlist)
    ))



;;
;;  shu-cpp-insert-template-decl
;;
(defun shu-cpp-insert-template-decl (template-list)
  "If TEMPLATE-LIST holds a list of template parameter names, insert into the
buffer the declaration

        template<typename A, typename B>
        inline

If TEMPLATE-LIST  is nil, do nothing."
  (let (
        )
    (when template-list
      (insert
       (concat
        (shu-cpp-make-decl-template template-list))
       "\n"
       "inline"
       "\n"
       )
      )
    ))



;;
;;  shu-cpp-make-decl-template
;;
(defun shu-cpp-make-decl-template (template-list)
  "Create the declaration

        template<typename A, typename B>

from the list of template parameter names"
  (let ((pname)
        (tdecl "")
        (sep ""))
    (when template-list
      (setq tdecl "template<")
      (while template-list
        (setq pname (car template-list))
        (setq tdecl (concat tdecl sep "typename " pname))
        (setq sep ", ")
        (setq template-list (cdr template-list)))
      (setq tdecl (concat tdecl ">")))
    tdecl
    ))



;;
;;  shu-cpp-make-template-list
;;
(defun shu-cpp-make-template-list (template-list)
  "Create the declaration

        <A, B>

from the list of template parameter names.
An empty string is returned if TEMPLATE-LIST is nil or empty."
  (let ((pname)
        (count 0)
        (tlist "<")
        (sep "")
        (result ""))
    (when template-list
      (while template-list
        (setq pname (car template-list))
        (setq tlist (concat tlist sep pname))
        (setq sep ", ")
        (setq count (1+ count))
        (setq template-list (cdr template-list)))
      (when (/= count 0)
        (setq result (concat tlist ">"))))
    result
    ))



;;
;;  shu-test-shu-cpp-insert-template-decl-1
;;
(ert-deftest shu-test-shu-cpp-insert-template-decl-1 ()
  (let (
        (template-list)
        (actual)
        (expected "")
        )
    (with-temp-buffer
      (shu-cpp-insert-template-decl template-list)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      )
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-insert-template-decl-2
;;
(ert-deftest shu-test-shu-cpp-insert-template-decl-2 ()
  (let (
        (template-list (list "S" "T"))
        (actual "")
        (expected "template<typename S, typename T>\ninline\n")
        )
    (with-temp-buffer
      (shu-cpp-insert-template-decl template-list)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      )
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-1
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-1 ()
  (let ((template-list (list "T"))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar<T>"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-2
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-2 ()
  (let ((template-list (list "T" "S" "Q"))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar<T, S, Q>"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-3
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-3 ()
  (let ((template-list (list))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-4
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-4 ()
  (let ((template-list)
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-1
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-1 ()
  (let ((tp-string "a")
        (tp-list))
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should tp-list)
    (should (listp tp-list))
    (should (= 1 (length tp-list)))
    (should (string= "a" (car tp-list)))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-2
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-2 ()
  (let (
        (tp-string "a, b")
        (tp-list)
        )
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should tp-list)
    (should (listp tp-list))
    (should (= 2 (length tp-list)))
    (should (string= "a" (car tp-list)))
    (should (string= "b" (cadr tp-list)))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-3
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-3 ()
  (let ((tp-string "")
        (tp-list))
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should (not tp-list))
    ))





;;
;;  shu-test-shu-cpp-make-decl-template-1
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-1 ()
  (let ((template-list (list "T"))
        (actual)
        (expected "template<typename T>"))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-decl-template-2
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-2 ()
  (let ((template-list (list "T" "P" "Q"))
        (actual)
        (expected "template<typename T, typename P, typename Q>"))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-decl-template-3
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-3 ()
  (let ((template-list)
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-1
;;
(ert-deftest shu-test-shu-cpp-make-template-list-1 ()
  (let ((template-list (list "T"))
        (actual)
        (expected "<T>"))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-2
;;
(ert-deftest shu-test-shu-cpp-make-template-list-2 ()
  (let ((template-list (list "T" "P" "Q"))
        (actual)
        (expected "<T, P, Q>"))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-3
;;
(ert-deftest shu-test-shu-cpp-make-template-list-3 ()
  (let ((template-list (list))
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-4
;;
(ert-deftest shu-test-shu-cpp-make-template-list-4 ()
  (let ((template-list)
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;; shu-bde-new.el ends here
