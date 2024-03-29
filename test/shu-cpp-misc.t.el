;;; shu-cpp-misc.t.el --- Shu project unit tests for code in shu-misc.el
;;
;; Copyright (C) 2020 Stewart L. Palmer
;;
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

;;
;;  shu-cpp-misc.t.el
;;
;;  unit tests for code in shu-cpp-misc.el
;;


(require 'ert)
(require 'shu-cpp-misc)

;;; Code




;;
;;  shu-test-shu-cpp-decl-cpp-class-name
;;
(ert-deftest shu-test-shu-cpp-decl-cpp-class-name ()
  (let* ((class-name "MumbleFrotz")
        (expected
         (concat
          "                        // -----------------\n"
          "                        // class MumbleFrotz\n"
          "                        // -----------------\n"))
        (actual))
    (with-temp-buffer
     (goto-char (point-min))
     (shu-cpp-decl-cpp-class-name class-name)
     (goto-char (point-min))
     (search-forward "----" nil t)
     (beginning-of-line)
     (setq actual (buffer-substring-no-properties (point) (point-max)))
     (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-decl-h-class-name
;;
(ert-deftest shu-test-shu-cpp-decl-h-class-name ()
  (let* ((class-name "MumbleFrotz")
        (expected
         (concat
          "                        // =================\n"
          "                        // class MumbleFrotz\n"
          "                        // =================\n"))
        (actual))
    (with-temp-buffer
     (goto-char (point-min))
     (shu-cpp-decl-h-class-name class-name)
     (goto-char (point-min))
     (search-forward "====" nil t)
     (beginning-of-line)
     (setq actual (buffer-substring-no-properties (point) (point-max)))
     (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-misc-gen-h-ctor-1
;;
(ert-deftest shu-test-shu-cpp-misc-gen-h-ctor-1 ()
  (let* ((class-name "MumbleFrotz")
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           ipad "/*!\n"
           ipad " * \\brief Create a " class-name " object ...\n"
           ipad " */\n"
           ipad "explicit " class-name "();\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-misc-gen-h-ctor class-name)
      (goto-char (point-min))
      (should (search-forward "/*!" nil t))
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-misc-gen-h-ctor-2
;;
(ert-deftest shu-test-shu-cpp-misc-gen-h-ctor-2 ()
  (let* ((class-name "OutstandingRefresher")
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           ipad "/*!\n"
           ipad " * \\brief Create an " class-name " object ...\n"
           ipad " */\n"
           ipad "explicit " class-name "();\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-misc-gen-h-ctor class-name)
      (goto-char (point-min))
      (should (search-forward "/*!" nil t))
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-misc-gen-h-ctor-3
;;
(ert-deftest shu-test-shu-cpp-misc-gen-h-ctor-3 ()
  (let* ((class-name "LoneRamblerGuy")
         (use-allocator t)
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           ipad "/*!\n"
           ipad " * \\brief Create a " class-name " object ...\n"
           ipad " */\n"
           ipad "explicit " class-name "(\n"
           ipad ipad shu-cpp-default-allocator-type "    *allocator = nullptr);\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-misc-gen-h-ctor class-name use-allocator)
      (goto-char (point-min))
      (should (search-forward "/*!" nil t))
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-misc-gen-h-ctor-4
;;
(ert-deftest shu-test-shu-cpp-misc-gen-h-ctor-4 ()
  (let* ((class-name "OutstandingRefresher")
         (use-allocator t)
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           ipad "/*!\n"
           ipad " * \\brief Create an " class-name " object ...\n"
           ipad " */\n"
           ipad "explicit " class-name "(\n"
           ipad ipad shu-cpp-default-allocator-type "    *allocator = nullptr);\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-misc-gen-h-ctor class-name use-allocator)
      (goto-char (point-min))
      (should (search-forward "/*!" nil t))
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-misc-gen-h-dtor
;;
(ert-deftest shu-test-shu-cpp-misc-gen-h-dtor ()
  (let* ((class-name "MumbleFrotz")
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           ipad "/*!\n"
           ipad " * \\brief Destroy this object\n"
           ipad " */\n"
           ipad "// ~" class-name "();\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-misc-gen-h-dtor class-name)
      (goto-char (point-min))
      (should (search-forward "/*!" nil t))
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-misc-gen-nested-traits
;;
(ert-deftest shu-test-shu-cpp-misc-gen-nested-traits ()
  (let* ((class-name "MumbleFrotz")
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           ipad "// TRAITS\n"
           ipad "\n"
           ipad "BSLMF_NESTED_TRAIT_DECLARATION(" class-name ", bslma::UsesBslmaAllocator);\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-misc-gen-nested-traits class-name)
      (goto-char (point-min))
      (should (search-forward "TRAITS" nil t))
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cpp-decl-h-print-self
;;
(ert-deftest shu-test-shu-cpp-decl-h-print-self ()
  (let* ((ipad (make-string shu-cpp-indent-length ? ))
         (std-name (if shu-cpp-use-bde-library "bsl" "std"))
         (expected
          (concat
           ipad "/*!\n"
           ipad " *  \\brief Stream object out to a stream\n"
           ipad " *\n"
           ipad " * Intended for use by operator<<()\n"
           ipad " */\n"
           ipad std-name "::ostream &printSelf(\n"
           ipad "    " std-name "::ostream    &os)\n"
           ipad "const;\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-decl-h-print-self)
      (goto-char (point-min))
      (search-forward "/*!" nil t)
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cpp-decl-cpp-print-self
;;
(ert-deftest shu-test-shu-cpp-decl-cpp-print-self ()
  (let* ((ipad (make-string shu-cpp-indent-length ? ))
         (class-name "MumbleFrotz")
         (std-name (if shu-cpp-use-bde-library "bsl" "std"))
         (expected
          (concat
           std-name "::ostream &" class-name "::printSelf(\n"
           "    " std-name "::ostream    &os)\n"
           "const\n"
           "{\n"
           ipad "os << \"Instance of '" class-name "'\";\n"
           "\n"
           ipad "return os;\n"
           "}\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-decl-cpp-print-self class-name)
      (goto-char (point-min))
      (search-forward "::print" nil t)
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cpp-decl-h-stream
;;
(ert-deftest shu-test-shu-cpp-decl-h-stream ()
  (let* ((ipad (make-string shu-cpp-indent-length ? ))
         (class-name "MumbleFrotz")
         (std-name (if shu-cpp-use-bde-library "bsl" "std"))
         (expected
          (concat
           "/*!\n"
           " *  \\brief Stream an instance of " class-name " to the stream `os`\n"
           " */\n"
           std-name "::ostream &operator<<(\n"
           ipad std-name "::ostream       &os,\n"
           ipad "const " class-name "  &cn);\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-decl-h-stream class-name)
      (goto-char (point-min))
      (search-forward "/*!" nil t)
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cpp-decl-cpp-stream
;;
(ert-deftest shu-test-shu-cpp-decl-cpp-stream ()
  (let* ((class-name "MumbleFrotz")
         (std-name (if shu-cpp-use-bde-library "bsl" "std"))
         (expected
          (concat
           "inline\n"
           std-name "::ostream &operator<<(\n"
           "    " std-name "::ostream       &os,\n"
           "    const MumbleFrotz  &cn)\n"
           "{\n"
           "    return cn.printSelf(os);\n"
           "}\n"))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-decl-cpp-stream class-name)
      (goto-char (point-min))
      (search-forward "inline" nil t)
      (beginning-of-line)
      (setq actual (buffer-substring-no-properties (point) (point-max)))
      (should (string= expected actual)))
    ))




;;
;;  shu-test-shu-cpp-gen-inline-template-header
;;
(ert-deftest shu-test-shu-cpp-gen-inline-template-header ()
  (let ((expected
         (concat
          "\n"
          "// ===========================================================================\n"
          "//                  INLINE AND TEMPLATE FUNCTION IMPLEMENTATIONS\n"
          "// ===========================================================================\n"))
        (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-gen-inline-template-header)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))

;;; shu-cpp-misc.t.el ends here
