;;; shu-capture-doc.t.el --- Shu project code unit tests for shu-capture-doc
;;
;; Copyright (C) 2018 Stewart L. Palmer
;;
;; Author: Stewart L. Pslmer <stewart@stewartpalmer.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Project Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Project Public License for more details.
;;
;; You should have received a copy of the GNU Project Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Code


(require 'ert)
(require 'shu-capture-doc)


;;
;;  shu-test-shu-doc-internal-to-md-1
;;
(ert-deftest shu-test-shu-doc-internal-to-md-1 ()
  (let ((x))
      (setq x (shu-doc-internal-to-md ""))
      (should (= 0 (length x)))
      ))


;;
;;  shu-test-shu-doc-internal-to-md-2
;;
(ert-deftest shu-test-shu-doc-internal-to-md-2 ()
  (let ((x  "This is a doc-string")
        (y))
      (setq y (shu-doc-internal-to-md x))
      ))


;;
;;  shu-test-shu-doc-internal-to-md-3
;;
(ert-deftest shu-test-shu-doc-internal-to-md-3 ()
  (let ((x  "This is a \\\"doc\\\" string")
        (expected "This is a \"doc\" string")
        (actual))
      (setq actual (shu-doc-internal-to-md x))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-doc-internal-to-md-4
;;
(ert-deftest shu-test-shu-doc-internal-to-md-4 ()
  (let ((x  "This is a **buffer-name**")
        (expected (concat "This is a "
                         shu-capture-md-buf-delimiter
                         "**buffer-name**"
                         shu-capture-md-buf-delimiter))
        (actual))
      (setq actual (shu-doc-internal-to-md x))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-doc-internal-to-md-5
;;
(ert-deftest shu-test-shu-doc-internal-to-md-5 ()
  (let ((x  "**buffer-name**")
        (expected (concat
                   shu-capture-md-buf-delimiter
                   "**buffer-name**"
                   shu-capture-md-buf-delimiter))
        (actual))
      (setq actual (shu-doc-internal-to-md x))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-doc-internal-to-md-6
;;
(ert-deftest shu-test-shu-doc-internal-to-md-6 ()
  (let ((x  "ARG-NAME")
        (expected "arg-name")
        (actual))
    (setq actual (downcase x))
    (should (string= expected actual))
      ))


;;
;;  shu-test-shu-doc-internal-to-md-7
;;
(ert-deftest shu-test-shu-doc-internal-to-md-7 ()
  (let ((x  "This is an ARG name.")
        (expected (concat "This is an "
                          shu-capture-md-arg-delimiter
                          "arg" shu-capture-md-arg-delimiter
                          " name."))
        (actual))
      (setq actual (shu-doc-internal-to-md x))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-doc-internal-to-md-8
;;
(ert-deftest shu-test-shu-doc-internal-to-md-8 ()
  (let ((x  "This is an ARG.")
        (expected
         (concat
          "This is an "
          shu-capture-md-arg-delimiter
          "arg"
          shu-capture-md-arg-delimiter
          "."))
        (actual))
      (setq actual (shu-doc-internal-to-md x))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-doc-internal-to-md-9
;;
(ert-deftest shu-test-shu-doc-internal-to-md-9 ()
  (let ((x  "This is an ARG, with")
        (expected
         (concat
          "This is an "
          shu-capture-md-arg-delimiter
          "arg"
          shu-capture-md-arg-delimiter
          ", with"))
        (actual))
      (setq actual (shu-doc-internal-to-md x))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-set-func-def
;;
(ert-deftest shu-test-shu-capture-set-func-def ()
  (let ((func-def)
        (falias)
        (finfo)
        (signature    "some-function (arg1 arg2)")
        (attributes   "I")
        (description  "This is a putative dpc string."))
    (shu-capture-set-func-def func-def signature attributes description)
    (should (string= signature (car-safe func-def)))
    (setq falias (cdr-safe func-def))
    (should falias)
    (setq finfo (cdr-safe falias))
    (should finfo)
    (should (string= attributes (car-safe finfo)))
    (should (string= description (cdr-safe finfo)))
))


;;
;;  shu-test-shu-capture-get-func-def
;;
(ert-deftest shu-test-shu-capture-get-func-def ()
  (let ((func-def)
        (finfo)
        (signature    "some-function (arg1 arg2)")
        (attributes   "I")
        (description  "This is a putative dpc string.")
        (sig)
        (attrs)
        (alias)
        (desc))
    (shu-capture-set-func-def func-def signature attributes description)
    (shu-capture-get-func-def func-def sig attrs desc alias)
    (should (string= signature sig))
    (should (string= attributes attrs))
    (should (string= description desc))
))


;;
;;  shu-test-shu-capture-set-func-def-alias
;;
(ert-deftest shu-test-shu-capture-set-func-def-alias ()
  (let ((func-def)
        (finfo)
        (signature    "some-function (arg1 arg2)")
        (attributes   "I")
        (description  "This is a putative dpc string.")
        (alias "some-alias")
        (sig)
        (attrs)
        (als)
        (desc))
    (shu-capture-set-func-def-alias func-def signature attributes description alias)
    (shu-capture-get-func-def func-def sig attrs desc als)
    (should (string= signature sig))
    (should (string= attributes attrs))
    (should (string= description desc))
    (should (string= alias als))
))


;;
;;  shu-test-shu-capture-get-func-def-sig
;;
(ert-deftest shu-test-shu-capture-get-func-def-sig ()
  (let ((func-def)
        (finfo)
        (signature    "some-function (arg1 arg2)")
        (attributes   "I")
        (description  "This is a putative dpc string.")
        (sig)
        (attrs)
        (desc))
    (shu-capture-set-func-def func-def signature attributes description)
    (shu-capture-get-func-def-sig func-def sig)
    (should (string= signature sig))
))



;;
;;  shu-test-shu-capture-get-name-and-args-1
;;
(ert-deftest shu-test-shu-capture-get-name-and-args-1 ()
  "Doc string."
  (let (
        (signature "do-something (to something)")
        (expected-func-name "do-something")
        (expected-args "to something")
        (func-name)
        (args)
        )
    (shu-capture-get-name-and-args signature func-name args)
    (should func-name)
    (should args)
    (should (string= expected-func-name func-name))
    (should (string= expected-args args))
    ))



;;
;;  shu-test-shu-capture-get-name-and-args-2
;;
(ert-deftest shu-test-shu-capture-get-name-and-args-2 ()
  "Doc string."
  (let (
        (signature "do-something (  to something  )  ")
        (expected-func-name "do-something")
        (expected-args "to something")
        (func-name)
        (args)
        )
    (shu-capture-get-name-and-args signature func-name args)
    (should func-name)
    (should args)
    (should (string= expected-func-name func-name))
    (should (string= expected-args args))
    ))



;;
;;  shu-test-shu-capture-get-name-and-args-3
;;
(ert-deftest shu-test-shu-capture-get-name-and-args-3 ()
  "Doc string."
  (let (
        (signature "do-something ()  ")
        (expected-func-name "do-something")
        (expected-args "")
        (func-name)
        (args)
        )
    (shu-capture-get-name-and-args signature func-name args)
    (should func-name)
    (should args)
    (should (string= expected-func-name func-name))
    (should (string= expected-args args))
    ))



;;
;;  shu-test-shu-capture-get-name-and-args-4
;;
(ert-deftest shu-test-shu-capture-get-name-and-args-4 ()
  "Doc string."
  (let (
        (signature "Happy birthday")
        (expected-func-name "")
        (expected-args "")
        (func-name)
        (args)
        )
    (shu-capture-get-name-and-args signature func-name args)
    (should func-name)
    (should args)
    (should (string= expected-func-name func-name))
    (should (string= expected-args args))
    ))



;;
;;  shu-test-shu-capture-convert-doc-string-1
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-1 ()
  (let ((x)
        (signature "")
        (description "")
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (= 0 (length actual)))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-2
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-2 ()
  (let ((signature "foo (a b)")
        (description  "This is a doc-string")
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-3
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-3 ()
  (let ((signature "foo (a b)")
        (description  "This is a \\\"doc\\\" string")
        (expected "This is a \"doc\" string")
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-4
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-4 ()
  (let ((signature "foo (a b)")
        (description  "This is a **buffer-name**")
        (expected (concat "This is a "
                         shu-capture-md-buf-delimiter
                         "**buffer-name**"
                         shu-capture-md-buf-delimiter))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-5
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-5 ()
  (let ((signature "foo (a b)")
        (description  "**buffer-name**")
        (expected (concat
                   shu-capture-md-buf-delimiter
                   "**buffer-name**"
                   shu-capture-md-buf-delimiter))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-6
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-6 ()
  (let (
        (gb (get-buffer-create "**slp**"))
        (signature "foo (a arg-name)")
        (description  "ARG-NAME")
        (expected (concat shu-capture-md-arg-delimiter "arg-name" shu-capture-md-arg-delimiter))
        (actual))
    (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
    (princ (concat "\n6 expected: \"" expected "\"\n"
                   "    actual: \"" actual "\"\n") gb)
    (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-7
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-7 ()
  (let ((signature "foo (arg b)")
        (description  "This is an ARG name.")
        (expected (concat "This is an "
                          shu-capture-md-arg-delimiter
                          "arg" shu-capture-md-arg-delimiter
                          " name."))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-8
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-8 ()
  (let ((signature "foo (arg b)")
        (description  "This is an ARG.")
        (expected
         (concat
          "This is an "
          shu-capture-md-arg-delimiter
          "arg"
          shu-capture-md-arg-delimiter
          "."))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-9
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-9 ()
  (let ((signature "foo (arg b)")
        (description  "This is an ARG, with")
        (expected
         (concat
          "This is an "
          shu-capture-md-arg-delimiter
          "arg"
          shu-capture-md-arg-delimiter
          ", with"))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-10
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-10 ()
  (let ((x)
        (signature "")
        (description "")
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (= 0 (length actual)))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-11
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-11 ()
  (let ((signature "foo (arg b)")
        (description  "This is an ARG, and NAME with")
        (expected
         (concat
          "This is an "
          shu-capture-md-arg-delimiter
          "arg"
          shu-capture-md-arg-delimiter
          ", and NAME with"))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-md-converters))
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-12
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-12 ()
  (let (
        (gb (get-buffer-create "**slp**"))
        (signature "foo (arg b)")
        (description  "This is an ARG, and NAME with")
        (expected
         (concat
          shu-capture-latex-doc-start "\n"
          "This is an "
          shu-capture-latex-arg-start
          "arg"
          shu-capture-latex-arg-end
          ", and NAME with"
          "\n" shu-capture-latex-doc-end))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-latex-converters))
      (princ (concat "\n12 expected: \"" expected "\"\n"
                     "     actual: \"" actual "\"\n") gb)
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-13
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-13 ()
  (let (
        (gb (get-buffer-create "**slp**"))
        (signature "foo (arg b)")
        (description  "This is an ARG, and NAME with")
        (expected
         (concat
          shu-capture-latex-doc-start "\n"
          "This is an "
          shu-capture-latex-arg-start
          "arg"
          shu-capture-latex-arg-end
          ", and NAME with"
          "\n" shu-capture-latex-doc-end))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-latex-converters))
      (princ (concat "\n13 expected: \"" expected "\"\n"
                     "     actual: \"" actual "\"\n") gb)
      (should (string= expected actual))
      ))


;;
;;  shu-test-shu-capture-convert-doc-string-14
;;
(ert-deftest shu-test-shu-capture-convert-doc-string-14 ()
  (let (
        (gb (get-buffer-create "**slp**"))
        (signature "foo (arg b)")
        (description  "This is an ARG, and NAME with &optional")
        (expected
         (concat
          shu-capture-latex-doc-start "\n"
          "This is an "
          shu-capture-latex-arg-start
          "arg"
          shu-capture-latex-arg-end
          ", and NAME with \\&optional"
          "\n" shu-capture-latex-doc-end))
        (actual))
      (setq actual (shu-capture-convert-doc-string signature description shu-capture-latex-converters))
      (princ (concat "\n14 expected: \"" expected "\"\n"
                     "     actual: \"" actual "\"\n") gb)
      (should (string= expected actual))
      ))




;;
;;  shu-test-shu-capture-code-in-md-1
;;
(ert-deftest shu-test-shu-capture-code-in-md-1 ()
  (let* ((pad (make-string shu-capture-doc-code-indent ? ))
         (line "This is some text to test things.\n")
         (delim-line "```\n")
         (data (concat
                line
                line
                pad line
                pad line
                line))
         (expected (concat
                    line
                    line
                    delim-line
                    pad line
                    pad line
                    delim-line
                    line))
         (actual "")
         (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-code-in-md))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))


;;
;;  shu-test-shu-capture-code-in-md-2
;;
(ert-deftest shu-test-shu-capture-code-in-md-2 ()
  (let* ((pad (make-string (1- shu-capture-doc-code-indent) ? ))
         (line "This is some text to test things.\n")
         (delim-line "```\n")
         (data (concat
                line
                line
                pad line
                pad line
                line))
         (expected data)
         (actual "")
         (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-code-in-md))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-capture-code-in-md-3
;;
(ert-deftest shu-test-shu-capture-code-in-md-3 ()
  (let* ((pad (make-string shu-capture-doc-code-indent ? ))
         (line "This is some text to test things.\n")
         (delim-line "```\n")
         (data (concat
                pad line
                pad line
                line))
         (expected (concat
                    delim-line
                    pad line
                    pad line
                    delim-line
                    line))
         (actual "")
         (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-code-in-md))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-capture-code-in-md-4
;;
(ert-deftest shu-test-shu-capture-code-in-md-4 ()
  (let* ((pad (make-string shu-capture-doc-code-indent ? ))
         (line "This is some text to test things.\n")
         (delim-line "```\n")
         (data (concat
                pad line
                pad line))
         (expected (concat
                    delim-line
                    pad line
                    pad line
                    delim-line))
         (actual "")
         (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-code-in-md))
      (should (= 1 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-capture-code-in-md-5
;;
(ert-deftest shu-test-shu-capture-code-in-md-5 ()
  (let* ((pad (make-string shu-capture-doc-code-indent ? ))
         (line "This is some text to test things.\n")
         (delim-line "```\n")
         (data (concat
                line            ;;;  1
                line            ;;;  2
                pad line        ;;;  3
                pad line        ;;;  4
                line            ;;;  5
                line            ;;;  6
                pad line        ;;;  7
                line            ;;;  8
                pad line        ;;;  9
                line))          ;;; 10
         (expected (concat
                    line        ;;;  1
                    line        ;;;  2
                    delim-line
                    pad line    ;;;  3
                    pad line    ;;;  4
                    delim-line
                    line        ;;;  5
                    line        ;;;  6
                    delim-line
                    pad line    ;;;  7
                    delim-line
                    line        ;;;  8
                    delim-line
                    pad line    ;;;  9
                    delim-line
                    line        ;;; 10
                    ))
         (actual "")
         (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-code-in-md))
      (should (= 3 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-capture-code-in-md-6
;;
(ert-deftest shu-test-shu-capture-code-in-md-6 ()
  (let* ((pad (make-string shu-capture-doc-code-indent ? ))
         (line "This is some text to test things.")
         (data line)
         (expected data)
         (actual "")
         (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-code-in-md))
      (should (= 0 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-capture-doc-convert-args-1
;;
(ert-deftest shu-test-shu-capture-doc-convert-args-1 ()
  (let ((signature "do-somerhing (with these things &optional and &rest others)")
        (data
         (concat
          "This is a description of SOME THINGS that are arguments\n"
          "and SOME that are and WITH are AND sort of NOT.  And perhaps\n"
          "THESE, or WITH.  A test."))
        (expected
         (concat
          "This is a description of SOME "
          shu-capture-md-arg-delimiter
          "things"
          shu-capture-md-arg-delimiter
          " that are arguments\n"
          shu-capture-md-arg-delimiter
          "and"
          shu-capture-md-arg-delimiter
          " SOME that are "
          shu-capture-md-arg-delimiter
          "and"
          shu-capture-md-arg-delimiter
          " "
          shu-capture-md-arg-delimiter
          "with"
          shu-capture-md-arg-delimiter
          " are "
          shu-capture-md-arg-delimiter
          "and"
          shu-capture-md-arg-delimiter
          " sort of NOT.  "
          shu-capture-md-arg-delimiter
          "and"
          shu-capture-md-arg-delimiter
          " perhaps\n"
          shu-capture-md-arg-delimiter
          "these"
          shu-capture-md-arg-delimiter
          ", or "
          shu-capture-md-arg-delimiter
          "with"
          shu-capture-md-arg-delimiter
          ".  A test."))
        (count 0)
        (debug-on-error t)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-doc-convert-args-to-md signature))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 8 count))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-capture-get-args-as-alist-1
;;
(ert-deftest shu-test-shu-capture-get-args-as-alist-1 ()
  "Doc string."
  (let ((signature "func-name (arg1 arg2 arg3)")
        (arg-assoc)
        (x))
    (setq arg-assoc (shu-capture-get-args-as-alist signature))
    (should (listp arg-assoc))
    (should (= 3 (length arg-assoc)))
    (setq x arg-assoc)
    (should (string= "arg3" (caar x)))
    (setq x (cdr x))
    (should (string= "arg2" (caar x)))
    (setq x (cdr x))
    (should (string= "arg1" (caar x)))
    (setq x (cdr x))
    ))




;;
;;  shu-test-shu-capture-get-args-as-alist-2
;;
(ert-deftest shu-test-shu-capture-get-args-as-alist-2 ()
  "Doc string."
  (let ((signature "func-name (arg1 &optional arg2 &rest arg3)")
        (arg-assoc)
        (x))
    (setq arg-assoc (shu-capture-get-args-as-alist signature))
    (should (listp arg-assoc))
    (should (= 3 (length arg-assoc)))
    (setq x arg-assoc)
    (should (string= "arg3" (caar x)))
    (setq x (cdr x))
    (should (string= "arg2" (caar x)))
    (setq x (cdr x))
    (should (string= "arg1" (caar x)))
    (setq x (cdr x))
    ))



;;
;;  shu-test-shu-capture-get-args-as-alist-3
;;
(ert-deftest shu-test-shu-capture-get-args-as-alist-3 ()
  "Doc string."
  (let ((signature "func-name ()")
        (arg-assoc))
    (setq arg-assoc (shu-capture-get-args-as-alist signature))
    (should (not arg-assoc))
    ))



;;
;;  shu-test-shu-capture-convert-quotes-1
;;
(ert-deftest shu-test-shu-capture-convert-quotes-1 ()
  "Doc string."
  (let* ((open-quote "``")
        (close-quote "''")
        (esc "\\")
        (quote    "\"")
        (data (concat esc quote "Hello." esc quote))
        (expected (concat open-quote "Hello." close-quote))
        (count)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-convert-quotes open-quote close-quote))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 2 count))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-convert-quotes-2
;;
(ert-deftest shu-test-shu-capture-convert-quotes-2 ()
  "Doc string."
  (let* ((open-quote "``")
        (close-quote "''")
        (esc "\\")
        (quote    "\"")
        (data (concat " " esc quote "Hello." esc quote))
        (expected (concat " " open-quote "Hello." close-quote))
        (count)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-convert-quotes open-quote close-quote))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 2 count))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-convert-quotes-3
;;
(ert-deftest shu-test-shu-capture-convert-quotes-3 ()
  "Doc string."
  (let* ((open-quote "``")
        (close-quote "''")
        (esc "\\")
        (quote    "\"")
        (data (concat "(" esc quote "Hello." esc quote))
        (expected (concat "(" open-quote "Hello." close-quote))
        (count)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-convert-quotes open-quote close-quote))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 2 count))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-convert-quotes-4
;;
(ert-deftest shu-test-shu-capture-convert-quotes-4 ()
  "Doc string."
  (let* ((open-quote "``")
        (close-quote "''")
        (esc "\\")
        (quote    "\"")
        (data (concat "[" esc quote "Hello." esc quote))
        (expected (concat "[" open-quote "Hello." close-quote))
        (count)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-convert-quotes open-quote close-quote))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 2 count))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-convert-quotes-5
;;
(ert-deftest shu-test-shu-capture-convert-quotes-5 ()
  "Doc string."
  (let* ((open-quote "``")
        (close-quote "''")
        (esc "\\")
        (quote    "\"")
        (data (concat "<" esc quote "Hello." esc quote))
        (expected (concat "<" open-quote "Hello." close-quote))
        (count)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-convert-quotes open-quote close-quote))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 2 count))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-convert-quotes-6
;;
(ert-deftest shu-test-shu-capture-convert-quotes-6 ()
  "Doc string."
  (let* ((open-quote "``")
        (close-quote "''")
        (esc "\\")
        (quote    "\"")
        (data (concat ">" esc quote "Hello." esc quote))
        (expected (concat ">" open-quote "Hello." close-quote))
        (count)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-convert-quotes open-quote close-quote))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 2 count))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-convert-quotes-7
;;
(ert-deftest shu-test-shu-capture-convert-quotes-7 ()
  "Doc string."
  (let* ((open-quote "``")
        (close-quote "''")
        (esc "\\")
        (quote    "\"")
        (data
         (concat
          esc quote "Now is the" esc quote " "
          esc quote "time" esc quote
          " for all good\n"
          esc quote "men" esc quote
          " to come to the aid of the party."))
        (expected
         (concat
          open-quote "Now is the" close-quote " "
          open-quote "time" close-quote
          " for all good\n"
          open-quote "men" close-quote
          " to come to the aid of the party."))
        (count)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-convert-quotes open-quote close-quote))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 6 count))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-convert-quotes-8
;;
(ert-deftest shu-test-shu-capture-convert-quotes-8 ()
  "Doc string."
  (let* ((open-quote "``")
        (close-quote "''")
        (esc "\\")
        (quote    "\"")
        (data (concat " " esc quote "Hello." esc quote esc quote "there" esc quote))
        (expected (concat " " open-quote "Hello." close-quote open-quote "there" close-quote))
        (count)
        (actual))
    (with-temp-buffer
      (insert data)
      (setq count (shu-capture-convert-quotes open-quote close-quote))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should (= 4 count))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-make-md-section
;;
(ert-deftest shu-test-shu-capture-make-md-section ()
  "Doc string."
  (let* ((level 2)
         (delim (make-string level ?#))
         (hdr "This is a header")
         (expected  (concat delim " " hdr " " delim))
         (actual))
    (setq actual (shu-capture-make-md-section level hdr))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-make-latex-section
;;
(ert-deftest shu-test-shu-capture-make-latex-section ()
  "Doc string."
  (let* ((level 2)
         (hdr "This is a header")
         (expected  (concat "\\subsection{" hdr shu-capture-latex-section-end))
         (actual))
    (setq actual (shu-capture-make-latex-section level hdr))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-capture-convert-args-to-markup-1
;;
(ert-deftest shu-test-shu-capture-convert-args-to-markup-1 ()
  (let ((arg-converter (cdr (assoc shu-capture-a-type-arg shu-capture-md-converters)))
        (keywd-converter (cdr (assoc shu-capture-a-type-keywd shu-capture-md-converters)))
        (signature "convert (arg1 arg-convert)")
        (expected-lengths (list 4 11))
        (expected-arg1
         (concat
          shu-capture-md-arg-delimiter "arg1" shu-capture-md-arg-delimiter))
        (expected-arg2
         (concat
          shu-capture-md-arg-delimiter "arg-convert" shu-capture-md-arg-delimiter))
        (result)
        (lengths)
        (markup-args)
        (actual-arg1)
        (actual-arg2))
    (setq result (shu-capture-convert-args-to-markup signature arg-converter keywd-converter))
    (should (consp result))
    (setq lengths (car result))
    (should (listp lengths))
    (setq markup-args (cdr result))
    (should (listp markup-args))
    (should (equal expected-lengths lengths))
    (setq actual-arg1 (car markup-args))
    (should (string= expected-arg1 actual-arg1))
    (should (cdr markup-args))
    (setq markup-args (cdr markup-args))
    (setq actual-arg2 (car markup-args))
    (should (string= expected-arg2 actual-arg2))
    ))



;;
;;  shu-test-shu-capture-convert-args-to-markup-2
;;
(ert-deftest shu-test-shu-capture-convert-args-to-markup-2 ()
  (let ((arg-converter (cdr (assoc shu-capture-a-type-arg shu-capture-md-converters)))
        (keywd-converter (cdr (assoc shu-capture-a-type-keywd shu-capture-md-converters)))
        (signature "convert (arg1 arg-convert &optional other)")
        (expected-lengths (list 4 11 9 5))
        (expected-arg1
         (concat
          shu-capture-md-arg-delimiter "arg1" shu-capture-md-arg-delimiter))
        (expected-arg2
         (concat
          shu-capture-md-arg-delimiter "arg-convert" shu-capture-md-arg-delimiter))
        (expected-arg3
         (concat
          shu-capture-md-keywd-delimiter "&optional" shu-capture-md-keywd-delimiter))
        (expected-arg4
         (concat
          shu-capture-md-arg-delimiter "other" shu-capture-md-arg-delimiter))
        (result)
        (lengths)
        (markup-args)
        (actual-arg1)
        (actual-arg2)
        (actual-arg3)
        (actual-arg4))
    (setq result (shu-capture-convert-args-to-markup signature arg-converter keywd-converter))
    (should (consp result))
    (setq lengths (car result))
    (should (listp lengths))
    (setq markup-args (cdr result))
    (should (listp markup-args))
    (should (equal expected-lengths lengths))
    (should (car markup-args))
    (setq actual-arg1 (car markup-args))
    (should (string= expected-arg1 actual-arg1))
    (should (cdr markup-args))
    (setq markup-args (cdr markup-args))
    (should (car markup-args))
    (setq actual-arg2 (car markup-args))
    (should (string= expected-arg2 actual-arg2))
    (should (cdr markup-args))
    (setq markup-args (cdr markup-args))
    (should (car markup-args))
    (setq actual-arg3 (car markup-args))
    (should (string= expected-arg3 actual-arg3))
    (should (cdr markup-args))
    (setq markup-args (cdr markup-args))
    (should (car markup-args))
    (setq actual-arg4 (car markup-args))
    (should (string= expected-arg4 actual-arg4))
    ))



;;
;;  shu-test-shu-capture-convert-args-to-markup-3
;;
(ert-deftest shu-test-shu-capture-convert-args-to-markup-3 ()
  (let ((arg-converter (cdr (assoc shu-capture-a-type-arg shu-capture-md-converters)))
        (keywd-converter (cdr (assoc shu-capture-a-type-keywd shu-capture-md-converters)))
        (signature "convert ()")
        (result))
    (setq result (shu-capture-convert-args-to-markup signature arg-converter keywd-converter))
    (should (not result))
    ))



;;
;;  shu-test-shu-capture-convert-args-to-markup-4
;;
(ert-deftest shu-test-shu-capture-convert-args-to-markup-4 ()
  (let ((arg-converter (cdr (assoc shu-capture-a-type-arg shu-capture-latex-converters)))
        (keywd-converter (cdr (assoc shu-capture-a-type-keywd shu-capture-latex-converters)))
        (signature "convert (arg1 arg-convert &optional other)")
        (expected-lengths (list 4 11 9 5))
        (expected-arg1
         (concat
          shu-capture-latex-arg-start "arg1" shu-capture-latex-arg-end))
        (expected-arg2
         (concat
          shu-capture-latex-arg-start "arg-convert" shu-capture-latex-arg-end))
        (expected-arg3
         (concat
          shu-capture-latex-keywd-start "\\&optional" shu-capture-latex-keywd-end))
        (expected-arg4
         (concat
          shu-capture-latex-arg-start "other" shu-capture-latex-arg-end))
        (result)
        (lengths)
        (markup-args)
        (actual-arg1)
        (actual-arg2)
        (actual-arg3)
        (actual-arg4))
    (setq result (shu-capture-convert-args-to-markup signature arg-converter keywd-converter))
    (should (consp result))
    (setq lengths (car result))
    (should (listp lengths))
    (setq markup-args (cdr result))
    (should (listp markup-args))
    (should (equal expected-lengths lengths))
    (should (car markup-args))
    (setq actual-arg1 (car markup-args))
    (should (string= expected-arg1 actual-arg1))
    (should (cdr markup-args))
    (setq markup-args (cdr markup-args))
    (should (car markup-args))
    (setq actual-arg2 (car markup-args))
    (should (string= expected-arg2 actual-arg2))
    (should (cdr markup-args))
    (setq markup-args (cdr markup-args))
    (should (car markup-args))
    (setq actual-arg3 (car markup-args))
    (should (string= expected-arg3 actual-arg3))
    (should (cdr markup-args))
    (setq markup-args (cdr markup-args))
    (should (car markup-args))
    (setq actual-arg4 (car markup-args))
    (should (string= expected-arg4 actual-arg4))
    ))





;;
;;  shu-test-shu-capture-convert-func-latex
;;
(ert-deftest shu-test-shu-capture-convert-func-latex ()
  (let* (
         (gb (get-buffer-create "**slp**"))
         (signature
          "some-function-name (arg1 something &optional or-other and more1 and more2 and more3 and more4)")
         (func-type "Function")
         (func-name)
         (args)
         (arg-converter (cdr (assoc shu-capture-a-type-arg shu-capture-latex-converters)))
         (keywd-converter (cdr (assoc shu-capture-a-type-keywd shu-capture-latex-converters)))
         (markups)
         (result)
        )
    (setq markups (shu-capture-convert-args-to-markup signature arg-converter keywd-converter))
    (shu-capture-get-name-and-args signature func-name args)
    (setq result (shu-capture-make-args-latex func-name markups func-type))
    (princ (concat result "\n") gb)
    ))




;;
;;  shu-test-shu-capture-func-type-name-1
;;
(ert-deftest shu-test-shu-capture-func-type-name-1 ()
  (let (
        (macro-attributes1 shu-capture-attr-macro)
        (macro-attributes2 (logior shu-capture-attr-macro shu-capture-attr-alias))
        (interactive-attributes1 shu-capture-attr-inter)
        (interactive-attributes2 (logior shu-capture-attr-inter shu-capture-attr-alias))
        (function-attributes shu-capture-attr-alias)
        (macro-name "Macro")
        (function-name "Function")
        (interactive-name "Command")
        )
    (should (string= macro-name (shu-capture-func-type-name macro-attributes1)))
    (should (string= macro-name (shu-capture-func-type-name macro-attributes2)))
    (should (string= function-name (shu-capture-func-type-name function-attributes)))
    (should (string= interactive-name (shu-capture-func-type-name interactive-attributes1)))
    (should (string= interactive-name (shu-capture-func-type-name interactive-attributes2)))
    ))


;;
;;  shu-test-shu-capture-func-type-name-2
;;
(ert-deftest shu-test-shu-capture-func-type-name-2 ()
  (let (
        (macro-attributes1 shu-capture-attr-macro)
        (macro-attributes2 (logior shu-capture-attr-macro shu-capture-attr-alias))
        (interactive-attributes1 shu-capture-attr-inter)
        (interactive-attributes2 (logior shu-capture-attr-inter shu-capture-attr-alias))
        (function-attributes shu-capture-attr-alias)
        (macro-name "Macro")
        (function-name "Function")
        (interactive-name "Command")
        (constant-name "Constant")
        (variable-name "Variable")
        (custom-name "Custom")
        )
    (should (string= macro-name (shu-capture-func-type-name macro-attributes1)))
    (should (string= macro-name (shu-capture-func-type-name macro-attributes2)))
    (should (string= function-name (shu-capture-func-type-name function-attributes)))
    (should (string= interactive-name (shu-capture-func-type-name interactive-attributes1)))
    (should (string= interactive-name (shu-capture-func-type-name interactive-attributes2)))
    (should (string= function-name (shu-capture-func-type-name 0)))
    (should (string= macro-name (shu-capture-func-type-name shu-capture-attr-macro)))
    (should (string= constant-name (shu-capture-func-type-name shu-capture-attr-const)))
    (should (string= variable-name (shu-capture-func-type-name shu-capture-attr-var)))
    (should (string= custom-name (shu-capture-func-type-name shu-capture-attr-custom)))
    ))



;;
;;  shu-test-shu-capture-commentary-1
;;
(ert-deftest shu-test-shu-capture-commentary-1 ()
  (let ((expected-pkg-name "shu-capture-doc")
        (expected-doc
         (concat
        "Collection of functions used to capture function and variable definitions\n"
        "subsequent publication."
        ))
        (result)
        (actual-pkg-name)
        (actual-doc))
    (with-temp-buffer
      (insert
       (concat
        ";;\n"
        ";; Package: shu-capture-doc\n"
        ";; Author: Stewart L. Palmer <stewart@stewartpalmer.com>\n"
        ";; see <http://www.gnu.org/licenses/>.\n"
        ";;\n"
        "\n"
        ";;; Commentary:\n"
        "\n"
        ";; Collection of functions used to capture function and variable definitions\n"
        ";; subsequent publication.\n"
        "\n"
        ";;; Code:\n"
        "        \n"))
      (setq result (shu-capture-commentary))
      (should result)
      (should (consp result))
      (setq actual-pkg-name (car result))
      (should actual-pkg-name)
      (should (stringp actual-pkg-name))
      (setq actual-doc (cdr result))
      (should actual-doc)
      (should (stringp actual-doc))
      (should (string= expected-pkg-name actual-pkg-name))
      (should (string= expected-doc actual-doc)))
    ))



;;
;;  shu-test-shu-capture-commentary-2
;;
(ert-deftest shu-test-shu-capture-commentary-2 ()
  (let ((expected-pkg-name "shu-cpp-general")
        (expected-doc
         (concat
        "A collection of useful functions for dealing with C++ code"
        ))
        (result)
        (actual-pkg-name)
        (actual-doc))
    (with-temp-buffer
      (insert
       (concat
        ";;\n"
        ";; Package: shu-cpp-general\n"
        ";; Author: Stewart L. Palmer <stewart@stewartpalmer.com>\n"
        ";; see <http://www.gnu.org/licenses/>.\n"
        ";;\n"
        "\n"
        ";;; Commentary:\n"
        "\n"
        ";; A collection of useful functions for dealing with C++ code\n"
        "\n"
        ";;; Code:\n"
        "        \n"))
      (setq result (shu-capture-commentary))
      (should result)
      (should (consp result))
      (setq actual-pkg-name (car result))
      (should actual-pkg-name)
      (should (stringp actual-pkg-name))
      (setq actual-doc (cdr result))
      (should actual-doc)
      (should (stringp actual-doc))
      (should (string= expected-pkg-name actual-pkg-name))
      (should (string= expected-doc actual-doc)))
    ))



;;
;;  shu-test-shu-capture-headers-in-doc-1
;;
(ert-deftest shu-test-shu-capture-headers-in-doc-1 ()
  "Test convert header to LaTex."
  (let* ((section-converter (cdr (assoc shu-capture-a-type-hdr shu-capture-latex-converters)))
         (hdr "This is a heading")
         (prefix "###")
         (sec-hdr (concat prefix " " hdr " " prefix))
         (expected-hdr (concat "\\subsubsection{" hdr "}"))
         (text "Some text and some text\n")
         (data (concat text sec-hdr "\n" text))
         (actual)
         (expected (concat text expected-hdr "\n" text)))
    (with-temp-buffer
      (insert data)
      (shu-capture-headers-in-doc section-converter)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-capture-headers-in-doc-2
;;
(ert-deftest shu-test-shu-capture-headers-in-doc-2 ()
  "Test convert header to markdown."
  (let* ((section-converter (cdr (assoc shu-capture-a-type-hdr shu-capture-md-converters)))
         (hdr "This is a heading")
         (prefix "###")
         (sec-hdr (concat prefix " " hdr " " prefix))
         (expected-hdr sec-hdr)
         (text "Some text and some text\n")
         (data (concat text sec-hdr "\n" text))
         (actual)
         (expected (concat text expected-hdr "\n" text)))
    (with-temp-buffer
      (insert data)
      (shu-capture-headers-in-doc section-converter)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-capture-headers-in-doc-3
;;
(ert-deftest shu-test-shu-capture-headers-in-doc-3 ()
  "Do not convert header if it does not start in column 1."
  (let* ((section-converter (cdr (assoc shu-capture-a-type-hdr shu-capture-latex-converters)))
         (hdr "This is a heading")
         (prefix "###")
         (sec-hdr (concat prefix " " hdr " " prefix))
         (expected-hdr (concat "\\subsubsection{" hdr "}"))
         (text "Some text and some text\n")
         (data (concat text " " sec-hdr "\n" text))
         (actual)
         (expected data))
    (with-temp-buffer
      (insert data)
      (shu-capture-headers-in-doc section-converter)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual)))
    ))

;;; shu-capture-doc.t.el ends here
