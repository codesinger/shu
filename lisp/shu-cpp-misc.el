;;; shu-cpp-misc.el --- Shu project code for dealing wth C++ in Emacs
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
;; shu-cpp-misc.el
;;
;; A collection of useful functions for dealing with C++ code

(provide 'shu-cpp-misc)
(require 'shu-base)



;;
;;  shu-gen-component
;;
(defun shu-gen-component (class-name)
  "Generate the three files for a new component: .cpp, .h, and .t.cpp"
  (interactive "sClass name?: ")
  (let* (
         (debug-on-error t)
         (author shu-cpp-author)
         (namespace shu-cpp-default-namespace)
         (base-class-name (downcase class-name))
         (base-name (concat namespace "_" base-class-name))
         (hfile-name (concat base-name ".h"))
         (cfile-name (concat base-name ".cpp"))
         (tfile-name (concat base-name ".t.cpp"))
         (got-files )
         (found-files "")
         (file-comma "")
         (got-count 0)
         (file-file "File")
         )
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
      (shu-generate-cfile author namespace class-name)
      (save-buffer)
      (goto-char (point-min))
      (find-file tfile-name)
      (shu-generate-tfile author namespace class-name)
      (save-buffer)
      (find-file hfile-name)
      (goto-char (point-min))
      (shu-generate-hfile author namespace class-name)
      (save-buffer)
      (goto-char (point-min))

      )))



;;
;;  shu-generate-hfile
;;
(defun shu-generate-hfile (author namespace class-name)
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
      "\n"
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
      "\n"))

    (goto-char decl-point)
    (beginning-of-line)
    (shu-cpp-cdecl class-name)
    ))



;;
;;  shu-generate-cfile
;;
(defun shu-generate-cfile (author namespace class-name)
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
      "\n"
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
      inner-close-namespace
      "}  // close enterprise namespace\n"))
    ))




;;
;;  shu-generate-tfile
;;
(defun shu-generate-tfile (author namespace class-name)
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
      "}\n"))
    ))




;;
;;  shu-cpp-cdecl - Skeleton class that may not be copied
;;
(defun shu-cpp-cdecl (class-name)
  "Generate a skeleton class declaration at point."
  (interactive "*sClass name?: ")
  (let (
        (use-allocator shu-cpp-use-bde-library)
        )
  (shu-cpp-inner-cdecl class-name nil use-allocator)))




;;
;;  shu-cpp-ccdecl - Skeleton class that may be copied
;;
(defun shu-cpp-ccdecl (class-name)
  "Generate a skeleton class declaration at point."
  (interactive "*sClass name?: ")
  (shu-cpp-inner-cdecl class-name t))




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
(defun shu-cpp-inner-cdecl (class-name copy-allowed &optional use-allocator)
  "Generate a skeleton class declaration at point."
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
       (ipad (make-string shu-cpp-indent-length ? ))
       (header-pos (point))
       (have-include )
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
                     ipad "explicit " class-name "(\n"
                     ipad ipad "const " class-name " &original);\n"))

    (setq op-equal (concat
                    ipad class-name " &operator=(\n"
                    ipad ipad "const " class-name " &rhs);\n"))

    (insert
     (concat
      "\n"
      "\n"
      blank24 "// " equal-pad "\n"
      blank24 "// class " class-name "\n"
      blank24 "// " equal-pad "\n"
      "\n"
      "/*!\n"
      " * \\brief "))
    (setq start-pos (point))
    (insert
     (concat
      "Description of " class-name "\n"
      " *\n"
      " * ingroup \n"
      " */\n"
      "class " class-name "\n"
      "{\n"
      "\n"
      ipad "// DATA\n\n"))
    (when use-allocator
    (insert
     (concat
      ipad "bslma::Allocator                 *d_allocator;\n"
      )))
    (insert
     (concat
      "\n"
      "  public:\n"
      "\n"
      "    // CREATORS\n\n"
      ipad "/*!\n"
      ipad " * \\brief Create " creator-a " " class-name " object ...\n"
      ipad " */\n"
      "    explicit " class-name "("))
    (when use-allocator
    (insert
     (concat
      "\n"
      ipad "    bslma::Allocator    *allocator = 0")))
    (insert
     (concat
      ");\n"
      "\n"
      ipad "/*!\n"
      ipad " * \\brief Destroy this object\n"
      ipad " */\n"
      ipad "// ~" class-name "();\n"
      "\n"
      ipad "// MANIPULATORS\n"
      "\n"
      ipad "// ACCESSORS\n"
      "\n"
      ipad "/*!\n"
      ipad " *  \\brief Stream object out to a stream\n"
      ipad " *\n"
      ipad " * Intended for use by operator<<()\n"
      ipad " */\n"
      ipad std-name "::ostream &printSelf(\n"
      ipad "    " std-name "::ostream    &os)\n"
      ipad "const;\n"
      "\n"
      "  private:\n"))
    (when (not copy-allowed)
      (insert
       (concat
      "\n"
      ipad "// NOT IMPLEMENTED\n"
      ipad "/*!\n"
      ipad " * \\brief The copy constructor is deliberately private and unimplemented.\n"
      ipad " *\n"
      ipad " * \\param original the object from which we are to be constructed\n"
      ipad " */\n"
      copy-ctor
      "\n"
      ipad "/*!\n"
      ipad " * \\brief operator=() is deliberately private and unimplemented.\n"
      ipad " *\n"
      ipad " * \\param rhs the object from which we are to be assigned\n"
      ipad " *\n"
      ipad " * \\return reference to self to allow for chained operators\n"
      ipad " */\n"
      op-equal)))
      (insert
       (concat
      "\n"
      ipad "// MANIPULATORS\n"
      "\n"
      ipad "// ACCESSORS\n"
      "\n"
      "};\n"
      "\n"
      "// FREE OPERATORS\n"
      "\n"
      "/*!\n"
      " *  \\brief Stream an instance of " class-name " to the stream <code>os</code>\n"
      " */\n"
      std-name "::ostream &operator<<(\n"
      "    " std-name "::ostream" ostream-pad "  &os,\n"
      "    const " class-name ostream-class-pad "  &cn);\n"))
      (when use-allocator
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when (search-forward "INCLUDED_BSLMA_ALLOCATOR" nil t)
              (setq have-include t))))
        (when (not have-include)
          (goto-char header-pos)
          (beginning-of-line)
          (insert
           (concat
            "\n"
            "#ifndef INCLUDED_BSLMA_ALLOCATOR\n"
            "#include <bslma_allocator.h>\n"
            "#endif\n"))))
    (goto-char start-pos)))



;;
;;  shu-cpp-cgen
;;
(defun shu-cpp-cgen (class-name &optional use-allocator)
  "Generate a skeleton class code generation at point."
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
       (class-name-pad )
       (start-pos )
       (ipad (make-string shu-cpp-indent-length ? )))
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
    (insert (concat
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
             "// CREATORS\n\n"
             "inline\n"
             class-name "::" class-name "("))
    (if use-allocator
        (progn
          (insert
           (concat
            "bslma::Allocator    *allocator)\n"
            ":\n"
            "d_allocator(bslma::Default::allocator(allocator))\n")))
      (insert ")\n"))
    (setq start-pos (save-excursion (forward-line -1) (end-of-line) (forward-char -1) (point)))
    (insert
     (concat
      "{\n"
      "}\n"
      "\n"
      "// MANIPULATORS\n"
      "\n"
      "// ACCESSORS\n"
      "\n"
      "inline\n"
      std-name "::ostream &" class-name "::printSelf(\n"
      "    " std-name "::ostream    &os)\n"
      "const\n"
      "{\n"
      ipad "os << \"Instance of '" class-name "'\";\n"
      "\n"
      ipad "return os;\n"
      "}\n"
      "\n"
      "// FREE OPERATORS\n\n"
      "inline\n"
      std-name "::ostream &operator<<(\n"
      "    " std-name "::ostream" ostream-pad "  &os,\n"
      "    const " class-name ostream-class-pad "  &cn)\n"
      "{\n"
      ipad "return cn.printSelf(os);\n"
      "}\n"
      ))
    (goto-char start-pos)
    ))



;;
;;  shu-cpp-ccgen
;;
(defun shu-cpp-ccgen (class-name)
  "Generate a skeleton class code generation at point."
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
       (class-name-pad )
       (start-pos )
       (ipad (make-string shu-cpp-indent-length ? )))
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
    (insert (concat
             "\n"
             "\n"
             "\n"
             blank24 "// " dash-pad "\n"
             blank24 "// class " class-name "\n"
             blank24 "// " dash-pad "\n"
             "\n"
             "// CREATORS\n\n"
             class-name "::" class-name "()\n"))
    (setq start-pos (save-excursion (forward-line -1) (end-of-line) (forward-char -1) (point)))
    (insert
     (concat
      "{\n"
      "}\n"
      "\n"
      "// MANIPULATORS\n"
      "\n"
      "// ACCESSORS\n"
      "\n"
      std-name "::ostream &" class-name "::printSelf(\n"
      "    " std-name "::ostream    &os)\n"
      "const\n"
      "{\n"
      ipad "os << \"Instance of '" class-name "'\";\n"
      "\n"
      ipad "return os;\n"
      "}\n"
      "\n"
      "// FREE OPERATORS\n\n\n"
      ))
    (goto-char start-pos)
    ))



;;
;;  shu-cpp-acgen
;;
(defun shu-cpp-acgen (class-name)
  "Generate a skeleton class code generation at point."
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
       (class-name-pad )
       (start-pos )
       (header-pos (point))
       (have-include )
       (include-line )
       (ipad (make-string shu-cpp-indent-length ? )))
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
    (insert (concat
             "\n"
             "\n"
             "\n"
             blank24 "// " dash-pad "\n"
             blank24 "// class " class-name "\n"
             blank24 "// " dash-pad "\n"
             "\n"
             "// CREATORS\n\n"
             class-name "::" class-name "(\n"))
    (setq start-pos (save-excursion (forward-line -1) (end-of-line) (point)))
    (insert (concat
             "    bslma::Allocator    *allocator)\n"
             ":\n"
             "d_allocator(bslma::Default::allocator(allocator))\n"))
    (insert
     (concat
      "{\n"))
    (insert
     (concat
      "}\n"
      "\n"
      "// MANIPULATORS\n"
      "\n"
      "// ACCESSORS\n"
      "\n"
      std-name "::ostream &" class-name "::printSelf(\n"
      "    " std-name "::ostream    &os)\n"
      "const\n"
      "{\n"
      ipad "os << \"Instance of '" class-name "'\";\n"
      "\n"
      ipad "return os;\n"
      "}\n"
      "\n"
      "// FREE OPERATORS\n\n\n"
      ))
    (save-excursion
      (save-restriction
        (widen)
         (goto-char (point-min))
         (when (search-forward "INCLUDED_BSLMA_DEFAULT" nil t)
           (setq have-include t))))
    (when (not have-include)
      (goto-char header-pos)
      (beginning-of-line)
      (setq include-line
            (concat
        "\n"
        "#ifndef INCLUDED_BSLMA_DEFAULT\n"
        "#include <bslma_default.h>\n"
        "#endif\n"))
      (setq start-pos (+ start-pos (length include-line)))
      (insert include-line))
    (goto-char start-pos)
    ))



;;
;;  shu-cpp-hcgen
;;
(defun shu-cpp-hcgen (class-name)
  "Generate a skeleton class code generation at point."
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
       (class-name-pad )
       (start-pos )
       (ipad (make-string shu-cpp-indent-length ? ))
       (header "INLINE AND TEMPLATE FUNCTION IMPLEMENTATIONS")
       (have-header))
    (when shu-cpp-use-bde-library
      (setq std-name "bsl"))
    (save-excursion
      (save-restriction
        (widen)
         (setq have-header (search-backward header nil t))))
    (setq class-name-pad (make-string (length class-name) ? ))
    (setq equal-pad (make-string (+ 6 (length class-name)) ?=))
    (setq dash-pad (make-string (+ 6 (length class-name)) ?-))
    (setq blank24 (make-string 24 ? ))
    (setq ostream-class-length (+ (length "const ") (length class-name) 2))
    (if (> ostream-length ostream-class-length)
        (setq ostream-class-pad (make-string (- ostream-length ostream-class-length) ? ))
      (when (> ostream-class-length ostream-length)
        (setq ostream-pad (make-string (- ostream-class-length ostream-length) ? ))))
    (insert "\n")
    (when (not have-header)
      (insert (concat
               "// ===========================================================================\n"
               "//                  " header "\n"
               "// ===========================================================================\n")))
    (insert (concat
             "\n"
             "\n"
             blank24 "// " dash-pad "\n"
             blank24 "// class " class-name "\n"
             blank24 "// " dash-pad "\n"
             "\n"
             "// CREATORS\n"))
    (setq start-pos (save-excursion (forward-line -1) (end-of-line) (forward-char -1) (point)))
    (insert
     (concat
      "\n"
      "// MANIPULATORS\n"
      "\n"
      "// ACCESSORS\n"
      "\n"
      "// FREE OPERATORS\n\n"
      "inline\n"
      std-name "::ostream &operator<<(\n"
      "    " std-name "::ostream" ostream-pad "  &os,\n"
      "    const " class-name ostream-class-pad "  &cn)\n"
      "{\n"
      ipad "return cn.printSelf(os);\n"
      "}\n"
      ))
    (goto-char start-pos)
    ))


;;
;;  shu-fline
;;
(defun shu-fline ()
  "Place a stream of __FILE__ and __LINE__ at point."
  (interactive)
  (insert " << __FILE__ << \":\" << __LINE__ << \": \" <<")
)


;;
;;  shu-dox-file
;;
(defun shu-dox-file ()
  "Place a skeleton Doxygen file definition at point."
  (interactive)
  (let
      ((start-pos))
  (beginning-of-line)
  (insert
   (concat
    "\n"
    "/*!\n"
    " * \\file " (file-name-nondirectory (buffer-file-name))  "\n"
    " * \n"
    " * \\brief "))
  (setq start-pos (point))
  (insert
   (concat
    "\n"
    " * \n"
    " */\n"))
  (goto-char start-pos)
))

;;
;;  shu-shu-misc-set-alias
;;
(defun shu-cpp-misc-set-alias ()
  "Set the common alias names for the functions in shu-cpp-misc.
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'gen-component 'shu-gen-component)
  (defalias 'cdecl 'shu-cpp-cdecl)
  (defalias 'ccdecl 'shu-cpp-ccdecl)
  (defalias 'cgen 'shu-cpp-cgen)
  (defalias 'acgen 'shu-cpp-acgen)
  (defalias 'ccgen 'shu-cpp-ccgen)
  (defalias 'hcgen 'shu-cpp-hcgen)
  (defalias 'fline 'shu-fline)
  (defalias 'dox-file 'shu-dox-file)
)
