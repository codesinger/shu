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
           ipad ipad shu-cpp-default-allocator-type "    *allocator = 0);\n"))
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
           ipad ipad shu-cpp-default-allocator-type "    *allocator = 0);\n"))
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
         (expected
          (concat
           ipad "/*!\n"
           ipad " *  \\brief Stream object out to a stream\n"
           ipad " *\n"
           ipad " * Intended for use by operator<<()\n"
           ipad " */\n"
           ipad "std::ostream &printSelf(\n"
           ipad "    std::ostream    &os)\n"
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
  (let* ((class-name "MumbleFrotz")
         (expected
          (concat
           "std::ostream &" class-name "::printSelf(\n"
           "    std::ostream    &os)\n"
           "const\n"
           "{\n"
           "    os << \"Instance of '" class-name "'\";\n"
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
;;  shu-test-shu-cpp-misc-gen-not-implemented
;;
(ert-deftest shu-test-shu-cpp-misc-gen-not-implemented ()
  (let* ((class-name "MumbleFrotz")
         (ipad (make-string shu-cpp-indent-length ? ))
         (expected
          (concat
           ipad "// NOT IMPLEMENTED\n"
           ipad "\n"
           ipad "/*!\n"
           ipad " * \\brief The copy constructor is deliberately private and unimplemented.\n"
           ipad " *\n"
           ipad " * \\param original the object from which we are to be constructed\n"
           ipad " */\n"
           ipad "explicit " class-name "(\n"
           ipad ipad "const " class-name " &original);\n"
           "\n"
           ipad "/*!\n"
           ipad " * \\brief operator=() is deliberately private and unimplemented.\n"
           ipad " *\n"
           ipad " * \\param rhs the object from which we are to be assigned\n"
           ipad " *\n"
           ipad " * \\return reference to self to allow for chained operators\n"
           ipad " */\n"
           ipad class-name " &operator=(\n"
           ipad ipad "const " class-name " &rhs);\n"
           ))
         (actual))
    (with-temp-buffer
      (goto-char (point-min))
      (shu-cpp-misc-gen-not-implemented class-name)
      (goto-char (point-min))
      (should (search-forward "NOT IMPLEMENTED" nil t))
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
         (expected
          (concat
           ipad "/*!\n"
           ipad " *  \\brief Stream an instance of " class-name " to the stream `os`\n"
           ipad " */\n"
           ipad "std::ostream &operator<<(\n"
           ipad "    std::ostream       &os,\n"
           ipad "    const " class-name "  &cn);\n"))
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
         (expected
          (concat
           "inline\n"
           "std::ostream &MumbleFrotz::operator<<(\n"
           "    std::ostream       &os,\n"
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
