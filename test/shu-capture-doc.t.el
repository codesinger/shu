;;; shu-capture-doc.t.el --- Shu project code unit tests
;;
;; Copyright (C) 2018 Stewart L. Palmer
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


;;
;;  shu-test-shu-doc-internal-to-md-1
;;
(ert-deftest shu-test-shu-doc-internal-to-md-1 ()
  (let ((x))
    (with-temp-buffer
      (shu-doc-internal-to-md)
      (setq x (buffer-substring-no-properties (point-min) (point-max)))
      (should (= 0 (length x)))
      )))


;;
;;  shu-test-shu-doc-internal-to-md-2
;;
(ert-deftest shu-test-shu-doc-internal-to-md-2 ()
  (let ((x  "This is a doc-string")
        (y))

    (with-temp-buffer
      (insert x)
      (shu-doc-internal-to-md)
      (setq y (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= x y))
      )))


;;
;;  shu-test-shu-doc-internal-to-md-3
;;
(ert-deftest shu-test-shu-doc-internal-to-md-3 ()
  (let ((x  "This is a \\\"doc\\\" string")
        (expected "This is a \"doc\" string")
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-doc-internal-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


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

    (with-temp-buffer
      (insert x)
      (shu-doc-internal-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


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

    (with-temp-buffer
      (insert x)
      (shu-doc-internal-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


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

    (with-temp-buffer
      (insert x)
      (shu-doc-internal-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


;;
;;  shu-test-shu-doc-internal-to-md-8
;;
(ert-deftest shu-test-shu-doc-internal-to-md-8 ()
  (let ((x  "This is an ARG.")
        (expected "This is an `arg`.")
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-doc-internal-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


;;
;;  shu-test-shu-doc-internal-to-md-9
;;
(ert-deftest shu-test-shu-doc-internal-to-md-9 ()
  (let ((x  "This is an ARG, with")
        (expected "This is an `arg`, with")
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-doc-internal-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


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
