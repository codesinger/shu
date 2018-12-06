;;; shu-capture-doc.t.el --- Shu project code unit tests
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

(require 'ert)
(require 'shu-capture-doc)

;;; Code



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
  (let ((signature "foo (a arg-name)")
        (description  "ARG-NAME")
        (expected "arg-name")
        (actual))
    (setq actual (downcase description))
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

;;; shu-capture-doc.t.el ends here
