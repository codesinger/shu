;;; shu-bde-cpp.el --- Shu project code for dealing wth BDE style code in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-bde-cpp
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
;;

;;; Commentary:

;; A collection of useful functions for generating C++ skeleton code files
;; and classes for code written in Bloomberg, L.P. BDE style.

;;; Code:

(provide 'shu-bde-cpp)
(require 'shu-base)


;;; shu-cpp-completion-prefix

;;
;;  shu-gen-bb-component
;;
(defun shu-gen-bb-component (class-name)
  "Generate the three files for a new component: .cpp, .h, and .t.cpp"
  (interactive "sClass name?: ")
  (let* ((author shu-cpp-author)
         (namespace shu-cpp-default-namespace)
         (base-class-name (downcase class-name))
;;;         (file-prefix (if shu-cpp-completion-prefix shu-cpp-completion-prefix (concat namespace "_")))
         (file-prefix shu-cpp-completion-prefix)
         (base-name (concat file-prefix base-class-name))
         (hfile-name (concat base-name ".h"))
         (cfile-name (concat base-name ".cpp"))
         (tfile-name (concat base-name ".t.cpp"))
         (got-files )
         (found-files "")
         (file-comma "")
         (got-count 0)
         (file-file "File")
         )
    (when (not file-prefix)
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
      (shu-generate-bb-cfile author namespace class-name)
      (save-buffer)
      (goto-char (point-min))
      (find-file tfile-name)
      (shu-generate-bb-tfile author namespace class-name)
      (save-buffer)
      (find-file hfile-name)
      (goto-char (point-min))
      (shu-generate-bb-hfile author namespace class-name)
      (save-buffer)
      (goto-char (point-min))

      )))



;;
;;  shu-generate-bb-hfile
;;
(defun shu-generate-bb-hfile (author namespace class-name)
  "Generate a skeleton header file"
  (let* (
         (hfile-name (file-name-nondirectory (buffer-file-name)))
         (guard-name (shu-bde-include-guard hfile-name))
         (open-line (concat (shu-make-padded-line (concat "// " hfile-name) 70) "-*-C++-*-"))
         (namespace-name namespace)
         (namespace-sep "::")
         (inner-close "}  // close package namespace\n")
         (inner-namespace "")
         (decl-point)
         )

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
      "//@AUTHOR: " author "\n"
      "\n"
      "#ifndef INCLUDED_BSLALG_TYPETRAITS\n"
      "#include <bslalg_typetraits.h>\n"
      "#endif\n"
      "\n"
      "#ifndef INCLUDED_BSL_IOSTREAM\n"
      "#include <bsl_iostream.h>\n"
      "#endif\n"
      "\n"
      "\n"
      "namespace BloombergLP {\n"
      inner-namespace
      "\n"
      "\n"))

    (setq decl-point (point))

    (insert
     (concat
      "\n"
      "\n"
      inner-close
      "}  // close enterprise namespace\n"
      "\n"
      "#endif  // " guard-name "\n"
      "\n"
      "// ---------------------------------------------------------------------------\n"
      "// NOTICE:\n"
      "//      Copyright (C) Bloomberg L.P., " (format-time-string "%Y") "\n"
      "//      All Rights Reserved.\n"
      "//      Property of Bloomberg L.P. (BLP)\n"
      "//      This software is made available solely pursuant to the\n"
      "//      terms of a BLP license agreement which governs its use.\n"
      "// ----------------------------- END-OF-FILE ---------------------------------\n"))

    (goto-char decl-point)
    (beginning-of-line)
    (shu-cpp-cdecl class-name)
    ))



;;
;;  shu-generate-bb-cfile
;;
(defun shu-generate-bb-cfile (author namespace class-name)
  "Generate a skeleton cpp file"
  (let* (
         (cfile-name (file-name-nondirectory (buffer-file-name)))
         (hfile-name (concat (file-name-sans-extension cfile-name) ".h"))
         (rcs-file-name (concat (file-name-sans-extension cfile-name) "_cpp"))
         (guard-name (shu-bde-include-guard cfile-name))
         (open-line (concat (shu-make-padded-line (concat "// " cfile-name) 70) "-*-C++-*-"))
         (inner-namespace "")
         (inner-close-namespace "")
         )

    (when namespace
      (setq inner-namespace (concat "namespace " namespace " {\n"))
      (setq inner-close-namespace (concat "}  // close package namespace\n")))

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
      "#include <" hfile-name ">\n"
      "\n"))
    (run-hooks 'shu-bde-gen-file-identifier-hook)
    (insert
     (concat
      "\n"
      "\n"
      "namespace BloombergLP {\n"
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
      "}  // close enterprise namespace\n"
      "\n"
      "\n"
      "// ---------------------------------------------------------------------------\n"
      "// NOTICE:\n"
      "//      Copyright (C) Bloomberg L.P., " (format-time-string "%Y") "\n"
      "//      All Rights Reserved.\n"
      "//      Property of Bloomberg L.P. (BLP)\n"
      "//      This software is made available solely pursuant to the\n"
      "//      terms of a BLP license agreement which governs its use.\n"
      "// ----------------------------- END-OF-FILE ---------------------------------\n"))
    ))




;;
;;  shu-generate-bb-tfile
;;
(defun shu-generate-bb-tfile (author namespace class-name)
  "Generate a skeleton t.cpp file"
  (interactive)
  (let* (
         (tfile-name (file-name-nondirectory (buffer-file-name)))
         (tbase-name (file-name-sans-extension tfile-name))
         (ext2 (file-name-extension tfile-name))
         (ext1 (file-name-extension tbase-name))
         (base-name (file-name-sans-extension tbase-name))
         (hfile-name (concat base-name ".h"))
         (open-line (concat (shu-make-padded-line (concat "// " tfile-name) 70) "-*-C++-*-"))
         (namespace-sep "::")
         (inner-using "")
         )

    (if namespace
        (setq inner-using (concat "using namespace BloombergLP::" namespace ";\n"))
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
      "#include <" hfile-name ">\n"
      "\n"
      "#include <gtest/gtest.h>\n"
      "\n"
      "\n"
      "using namespace BloombergLP;\n"
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
      "\n"
      "// ----------------------------------------------------------------------------\n"
      "// Copyright (C) " (format-time-string "%Y") " Bloomberg L.P.\n"
      "//\n"
      "// Permission is hereby granted, free of charge, to any person obtaining a copy\n"
      "// of this software and associated documentation files (the \"Software\"), to\n"
      "// deal in the Software without restriction, including without limitation the\n"
      "// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or\n"
      "// sell copies of the Software, and to permit persons to whom the Software is\n"
      "// furnished to do so, subject to the following conditions:\n"
      "//\n"
      "// The above copyright notice and this permission notice shall be included in\n"
      "// all copies or substantial portions of the Software.\n"
      "//\n"
      "// THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\n"
      "// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\n"
      "// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE\n"
      "// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\n"
      "// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING\n"
      "// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS\n"
      "// IN THE SOFTWARE.\n"
      "// ----------------------------- END-OF-FILE ----------------------------------\n"))
    ))




;;
;;  shu-bb-cpp-set-alias
;;
(defun shu-bb-cpp-set-alias ()
  "Set the common alias names for the functions in shu-bb-cpp.
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'gen-bb-component 'shu-gen-bb-component)
  )

;;; shu-bde-cpp.el ends here
