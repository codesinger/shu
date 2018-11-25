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
;;  shu-test-shu-internal-doc-to-md-1
;;
(ert-deftest shu-test-shu-internal-doc-to-md-1 ()
  (let ((x))
    (with-temp-buffer
      (shu-internal-doc-to-md)
      (setq x (buffer-substring-no-properties (point-min) (point-max)))
      (should (= 0 (length x)))
      )))


;;
;;  shu-test-shu-internal-doc-to-md-2
;;
(ert-deftest shu-test-shu-internal-doc-to-md-2 ()
  (let ((x  "This is a doc-string")
        (y))

    (with-temp-buffer
      (insert x)
      (shu-internal-doc-to-md)
      (setq y (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= x y))
      )))


;;
;;  shu-test-shu-internal-doc-to-md-3
;;
(ert-deftest shu-test-shu-internal-doc-to-md-3 ()
  (let ((x  "This is a \\\"doc\\\" string")
        (expected "This is a \"doc\" string")
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-internal-doc-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


;;
;;  shu-test-shu-internal-doc-to-md-4
;;
(ert-deftest shu-test-shu-internal-doc-to-md-4 ()
  (let ((x  "This is a **buffer-name**")
        (expected (concat "This is a "
                         shu-capture-md-buf-delimiter
                         "**buffer-name**"
                         shu-capture-md-buf-delimiter))
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-internal-doc-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


;;
;;  shu-test-shu-internal-doc-to-md-5
;;
(ert-deftest shu-test-shu-internal-doc-to-md-5 ()
  (let ((x  "**buffer-name**")
        (expected (concat
                   shu-capture-md-buf-delimiter
                   "**buffer-name**"
                   shu-capture-md-buf-delimiter))
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-internal-doc-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


;;
;;  shu-test-shu-internal-doc-to-md-6
;;
(ert-deftest shu-test-shu-internal-doc-to-md-6 ()
  (let ((x  "ARG-NAME")
        (expected "arg-name")
        (actual))
    (setq actual (downcase x))
    (should (string= expected actual))
      ))


;;
;;  shu-test-shu-internal-doc-to-md-7
;;
(ert-deftest shu-test-shu-internal-doc-to-md-7 ()
  (let ((x  "This is an ARG name.")
        (expected (concat "This is an "
                          shu-capture-md-arg-delimiter
                          "arg" shu-capture-md-arg-delimiter
                          " name."))
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-internal-doc-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


;;
;;  shu-test-shu-internal-doc-to-md-8
;;
(ert-deftest shu-test-shu-internal-doc-to-md-8 ()
  (let ((x  "This is an ARG.")
        (expected "This is an `arg`.")
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-internal-doc-to-md)
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))


;;
;;  shu-test-shu-internal-doc-to-md-9
;;
(ert-deftest shu-test-shu-internal-doc-to-md-9 ()
  (let ((x  "This is an ARG, with")
        (expected "This is an `arg`, with")
        (actual))

    (with-temp-buffer
      (insert x)
      (shu-internal-doc-to-md)
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
