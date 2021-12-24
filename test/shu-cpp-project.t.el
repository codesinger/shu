;;; shu-cpp-project.t.el --- Shu project code unit tests
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
(require 'shu-cpp-project)

;;; Code



;;
;;  shu-test-possible-cpp-file-name-1
;;
(ert-deftest shu-test-possible-cpp-file-name-1 ()
  (let ((result)
        (file "brumble.cpp"))
  (with-temp-buffer
      (insert file)
      (goto-char (point-min))
      (forward-char 2)
      (setq result (shu-possible-cpp-file-name))
      (should result)
      (should (listp result))
      (should (= 1 (length result)))
      (should (string= file (car result))))
))


;;
;;  shu-test-possible-cpp-file-name-2
;;
(ert-deftest shu-test-possible-cpp-file-name-2 ()
  (let* ((result)
        (file "brumble.cpp")
        (target (concat "Hi!\n" file "\nthere")))
  (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 6)
      (setq result (shu-possible-cpp-file-name))
      (should result)
      (should (listp result))
      (should (= 1 (length result)))
      (should (string= file (car result))))
))


;;
;;  shu-test-possible-cpp-file-name-3
;;
(ert-deftest shu-test-possible-cpp-file-name-3 ()
  (let* ((result)
        (file "brumble.cpp")
        (line 1234)
        (target (concat "Hi!\n" file ":"
                        (number-to-string line)
                        "\nthere")))
  (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 6)
      (setq result (shu-possible-cpp-file-name))
      (should result)
      (should (listp result))
      (should (= 2 (length result)))
      (should (string= file (car result)))
      (should (= line (cadr result))))
))


;;
;;  shu-test-possible-cpp-file-name-4
;;
(ert-deftest shu-test-possible-cpp-file-name-4 ()
  (let* ((result)
         (file "brumble.cpp")
         (line 1234)
         (column 52)
         (target
          (concat
           "Hi!\n" file ":"
           (number-to-string line) ":"
           (number-to-string column) ":"
           "\nthere"))
         (colc))
    (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 6)
      (setq result (shu-possible-cpp-file-name))
      (should result)
      (should (listp result))
      (should (= 3 (length result)))
      (should (string= file (car result)))
      (should (= line (cadr result)))
      (setq colc (cddr result))
      (should (= column (car colc))))
    ))


;;
;;  shu-test-possible-cpp-file-name-5
;;
(ert-deftest shu-test-possible-cpp-file-name-5 ()
  (let ((result)
        (file "brumble.chasm"))
  (with-temp-buffer
      (insert file)
      (goto-char (point-min))
      (forward-char 2)
      (setq result (shu-possible-cpp-file-name))
      (should (not result)))
))


;;
;;  shu-test-possible-cpp-file-name-6
;;
(ert-deftest shu-test-possible-cpp-file-name-6 ()
  (let* ((result)
        (file "brumble.cpp")
        (line 4231)
        (target (concat "[file = " file "] "
                        "[line = " (number-to-string line) "] "
                        "\nthere")))
  (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 10)
      (setq result (shu-possible-cpp-file-name))
      (should result)
      (should (listp result))
      (should (= 2 (length result)))
      (should (string= file (car result)))
      (should (= line (cadr result))))
))


;;
;;  shu-test-possible-cpp-file-name-7
;;
(ert-deftest shu-test-possible-cpp-file-name-7 ()
  (let ((result)
        (file "/aa/bb/cc/brumble.cpp"))
  (with-temp-buffer
      (insert file)
      (goto-char (point-min))
      (forward-char 2)
      (setq result (shu-possible-cpp-file-name t))
      (should result)
      (should (listp result))
      (should (= 1 (length result)))
      (should (string= file (car result))))
))


;;
;;  shu-test-possible-cpp-file-name-8
;;
(ert-deftest shu-test-possible-cpp-file-name-8 ()
  (let* ((result)
        (file "aa/bb/cc/brumble.cpp")
        (target (concat "Hi!\n" file "\nthere")))
  (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 6)
      (setq result (shu-possible-cpp-file-name t))
      (should result)
      (should (listp result))
      (should (= 1 (length result)))
      (should (string= file (car result))))
))


;;
;;  shu-test-possible-cpp-file-name-9
;;
(ert-deftest shu-test-possible-cpp-file-name-9 ()
  (let* ((result)
        (file "aa/bb/cc/brumble.cpp")
        (line 1234)
        (target (concat "Hi!\n" file ":"
                        (number-to-string line)
                        "\nthere")))
  (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 6)
      (setq result (shu-possible-cpp-file-name t))
      (should result)
      (should (listp result))
      (should (= 2 (length result)))
      (should (string= file (car result)))
      (should (= line (cadr result))))
))


;;
;;  shu-test-possible-cpp-file-name-10
;;
(ert-deftest shu-test-possible-cpp-file-name-10 ()
  (let* ((result)
         (file "/aaaaaa/bb/cc/dd/ee/ff/brumble.cpp")
         (line 1234)
         (column 52)
         (target
          (concat
           "Hi!\n" file ":"
           (number-to-string line) ":"
           (number-to-string column) ":"
           "\nthere"))
         (colc))
    (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 6)
      (setq result (shu-possible-cpp-file-name t))
      (should result)
      (should (listp result))
      (should (= 3 (length result)))
      (should (string= file (car result)))
      (should (= line (cadr result)))
      (setq colc (cddr result))
      (should (= column (car colc))))
    ))


;;
;;  shu-test-possible-cpp-file-name-11
;;
(ert-deftest shu-test-possible-cpp-file-name-11 ()
  (let ((result)
        (file "/aa/bb/cc/brumble.chasm"))
  (with-temp-buffer
      (insert file)
      (goto-char (point-min))
      (forward-char 2)
      (setq result (shu-possible-cpp-file-name t))
      (should (not result)))
))


;;
;;  shu-test-possible-cpp-file-name-12
;;
(ert-deftest shu-test-possible-cpp-file-name-12 ()
  (let* ((result)
        (file "aa/bb/cc/dddddd/eeeeee/brumble.cpp")
        (line 4231)
        (target (concat "[file = " file "] "
                        "[line = " (number-to-string line) "] "
                        "\nthere")))
  (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 10)
      (setq result (shu-possible-cpp-file-name t))
      (should result)
      (should (listp result))
      (should (= 2 (length result)))
      (should (string= file (car result)))
      (should (= line (cadr result))))
))


;;
;;  shu-test-possible-cpp-file-name-13
;;
(ert-deftest shu-test-possible-cpp-file-name-13 ()
  (let* ((result)
         (file "/aaaaaa/bb/cc/dd/ee/ff/brumble.txt")
         (line 1234)
         (column 52)
         (target
          (concat
           "Hi!\n" file ":"
           (number-to-string line) ":"
           (number-to-string column) ":"
           "\nthere"))
         (colc))
    (with-temp-buffer
      (insert target)
      (goto-char (point-min))
      (forward-char 6)
      (setq result (shu-possible-cpp-file-name t t))
      (should result)
      (should (listp result))
      (should (= 3 (length result)))
      (should (string= file (car result)))
      (should (= line (cadr result)))
      (setq colc (cddr result))
      (should (= column (car colc))))
    ))



;;
;;  shu-test-shu-get-line-column-of-file-1
;;
(ert-deftest shu-test-shu-get-line-column-of-file-1 ()
  (let ((data "thing.cpp:55:30: error:")
        (expected-line   55)
        (expected-col    30)
        (actual)
        (actual-line)
        (actual-col))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (if (search-forward "cpp" nil t)
          (progn
            (setq actual (shu-get-line-column-of-file))
            (should actual)
            (should (listp actual))
            (should (= 2 (length actual)))
            (setq actual-line (car actual))
            (should (= expected-line actual-line))
            (setq actual (cdr actual))
            (setq actual-col (car actual))
            (should (= expected-col actual-col)))
        (should nil)))
    ))



;;
;;  shu-test-shu-get-line-column-of-file-2
;;
(ert-deftest shu-test-shu-get-line-column-of-file-2 ()
  (let ((data "[file=thing.cpp] [line=55] error:")
        (expected-line   55)
        (actual)
        (actual-line))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (if (search-forward "cpp" nil t)
          (progn
            (setq actual (shu-get-line-column-of-file))
            (should actual)
            (should (listp actual))
            (should (= 1 (length actual)))
            (setq actual-line (car actual))
            (should (= expected-line actual-line)))
        (should nil)))
    ))



;;
;;  shu-test-shu-get-line-column-of-file-3
;;
(ert-deftest shu-test-shu-get-line-column-of-file-3 ()
  (let ((data "\"thing.cpp\", line 55.30: 1540-0064 (S) Syntax error:")
        (expected-line   55)
        (expected-col    30)
        (actual)
        (actual-line)
        (actual-col))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (if (search-forward "cpp" nil t)
          (progn
            (setq actual (shu-get-line-column-of-file))
            (should actual)
            (should (listp actual))
            (should (= 2 (length actual)))
            (setq actual-line (car actual))
            (should (= expected-line actual-line))
            (setq actual (cdr actual))
            (setq actual-col (car actual))
            (should (= expected-col actual-col)))
        (should nil)))
    ))



;;
;;  shu-test-shu-get-line-column-of-file-4
;;
(ert-deftest shu-test-shu-get-line-column-of-file-4 ()
  (let ((data "\"thing.cpp\", line 55.0: 1540-0064 (S) Syntax error:")
        (expected-line   55)
        (expected-col    1)
        (actual)
        (actual-line)
        (actual-col))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (if (search-forward "cpp" nil t)
          (progn
            (setq actual (shu-get-line-column-of-file))
            (should actual)
            (should (listp actual))
            (should (= 2 (length actual)))
            (setq actual-line (car actual))
            (should (= expected-line actual-line))
            (setq actual (cdr actual))
            (setq actual-col (car actual))
            (should (= expected-col actual-col)))
        (should nil)))
    ))



;;
;;  shu-test-shu-get-line-column-of-file-5
;;
(ert-deftest shu-test-shu-get-line-column-of-file-5 ()
  (let ((data " \"thing.cpp\", line 55: ")
        (expected-line   55)
        (actual)
        (actual-line))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (if (search-forward "cpp" nil t)
          (progn
            (setq actual (shu-get-line-column-of-file))
            (should actual)
            (should (listp actual))
            (should (= 1 (length actual)))
            (setq actual-line (car actual))
            (should (= expected-line actual-line)))
        (should nil)))
    ))



;;
;;  shu-test-shu-find-line-and-file-1
;;
(ert-deftest shu-test-shu-find-line-and-file-1 ()
  (let ((gb (get-buffer-create "**boo**"))
        (data " filename.cpp:122:14 \n")
        (ret-val)
        (x))
    (with-temp-buffer
      (insert data)
      (goto-char 8)
      (setq ret-val (shu-find-line-and-file))
      (should ret-val)
      (should (listp ret-val))
      (should (= 3 (length ret-val)))
      (setq x (car ret-val))
      (should x)
      (should (stringp x))
      (should (string= "filename.cpp" x))
      (setq ret-val (cdr ret-val))
      (should ret-val)
      (should (listp ret-val))
      (should (= 2 (length ret-val)))
      (setq x (car ret-val))
      (should x)
      (should (numberp x))
      (should (= 122 x))
      (setq ret-val (cdr ret-val))
      (should ret-val)
      (should (listp ret-val))
      (should (= 1 (length ret-val)))
      (setq x (car ret-val))
      (should x)
      (should (numberp x))
      (should (= 14 x)))
    ))




;;
;;  shu-test-shu-cpp-project-collapse-list-1
;;
(ert-deftest shu-test-shu-cpp-project-collapse-list-1 ()
  (let (
        (data
         (list
          (cons "xxx_stumble.h"   "/foo/bar/xxx_stumble.h")
          (cons "xxx_mumble.h"    "/foo/bar/xxx_mumble.h")
          (cons "xxx_stumble.h"   "/boo/baz/xxx_stumble.h")))
        (expected
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))))
        (actual))
    (setq actual (shu-cpp-project-collapse-list data))
    (should actual)
    (should (listp actual))
    (should (equal expected actual))
    (shu-cpp-project-get-list-counts expected)
    ))




;;
;;  shu-test-shu-cpp-project-collapse-list-2
;;
(ert-deftest shu-test-shu-cpp-project-collapse-list-2 ()
  (let (
        (data
         (list
          (cons "stumble.h"   "/foo/bar/stumble.h")
          (cons "mumble.h"    "/foo/bar/mumble.h")
          (cons "stumble.h"   "/boo/baz/stumble.h")))
        (expected
         (list
          (cons "mumble.h"    (list (list "/foo/bar/mumble.h")))
          (cons "stumble.h"   (list (list "/boo/baz/stumble.h"
                                          "/foo/bar/stumble.h")))))
        (actual))
    (setq actual (shu-cpp-project-collapse-list data))
    (should actual)
    (should (listp actual))
    (should (equal expected actual))
    (shu-cpp-project-get-list-counts expected)
    ))



;;
;;  shu-test-shu-cpp-project-collapse-list-3
;;
(ert-deftest shu-test-shu-cpp-project-collapse-list-3 ()
  (let (
        (data
         (list
          (cons "xxx_mumble.h"   "/foo/bar/xxx_mumble.h")))
        (expected
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))))
        (actual))
    (setq actual (shu-cpp-project-collapse-list data))
    (should actual)
    (should (listp actual))
    (should (equal expected actual))
    (shu-cpp-project-get-list-counts expected)
    ))




;;
;;  shu-test-shu-cpp-project-collapse-list-4
;;
(ert-deftest shu-test-shu-cpp-project-collapse-list-4 ()
  (let (
        (data
         (list
          (cons "stumble.h"   "/foo/bar/stumble.h")
          (cons "dumble.h"    "/foo/bar/dumble.h")
          (cons "mumble.h"    "/boo/baz/mumble.h")
          )
         )
        (expected
         (list
          (cons "dumble.h"    (list (list "/foo/bar/dumble.h")))
          (cons "mumble.h"    (list (list "/boo/baz/mumble.h")))
          (cons "stumble.h"   (list (list "/foo/bar/stumble.h")))
          )
         )
        (actual))
    (setq actual (shu-cpp-project-collapse-list data))
    (should actual)
    (should (listp actual))
    (should (equal expected actual))
    (shu-cpp-project-get-list-counts expected)
    ))



;;
;;  shu-test-shu-cpp-project-invert-list-1
;;
(ert-deftest shu-test-shu-cpp-project-invert-list-1 ()
  "Doc string."
  (let (
        (data
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))
          ))
        (expected
         (list
          "/boo/baz/xxx_stumble.h"
          "/foo/bar/xxx_mumble.h"
          "/foo/bar/xxx_stumble.h"
          ))
        (actual)
        )
    (setq actual (shu-cpp-project-invert-list data))
    (should actual)
    (should (listp actual))
    (should (equal actual expected))
    ))



;;
;;  shu-test-shu-cpp-project-get-list-counts-1
;;
(ert-deftest shu-test-shu-cpp-project-get-list-counts-1 ()
  "Doc string."
  (let (
        (data
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))))
        (counts)
        (c-count -1)
        (h-count -1)
        (dup-count -1)
        )
    (setq counts (shu-cpp-project-get-list-counts data))
    (setq c-count (car counts))
    (setq counts (cdr counts))
    (setq h-count (car counts))
    (setq dup-count (cadr counts))
    (should (= 0 c-count))
    (should (= 3 h-count))
    (should (= 1 dup-count))
    ))



;;
;;  shu-test-shu-cpp-project-get-list-counts-2
;;
(ert-deftest shu-test-shu-cpp-project-get-list-counts-2 ()
  "Doc string."
  (let (
        (data
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_mumble.cpp"  (list (list "/foo/bar/xxx_mumble.cpp")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))
          (cons "xxx_stumble.cpp" (list (list "/boo/baz/xxx_stumble.cpp"
                                              "/foo/bar/xxx_stumble.cpp")))))
        (counts)
        (c-count -1)
        (h-count -1)
        (dup-count -1))
    (setq counts (shu-cpp-project-get-list-counts data))
    (setq c-count (car counts))
    (setq counts (cdr counts))
    (setq h-count (car counts))
    (setq dup-count (cadr counts))
    (should (= 3 c-count))
    (should (= 3 h-count))
    (should (= 2 dup-count))
    ))



;;
;;  shu-test-shu-project-split-file-name-1
;;
(ert-deftest shu-test-shu-project-split-file-name-1 ()
  (let ((file-name "Wonderful.cpp")
        (expected-prefix "")
        (expected-short-name "wonderful.cpp")
        (actual-prefix)
        (actual-short-name)
        (ps))
    (setq ps (shu-project-split-file-name file-name))
    (should ps)
    (should (consp ps))
    (setq actual-prefix (car ps))
    (setq actual-short-name (cdr ps))
    (should (string= expected-prefix actual-prefix))
    (should (string= expected-short-name actual-short-name))
    ))



;;
;;  shu-test-shu-project-split-file-name-2
;;
(ert-deftest shu-test-shu-project-split-file-name-2 ()
  (let ((file-name "lovely_Wonderful.cpp")
        (expected-prefix "lovely")
        (expected-short-name "wonderful.cpp")
        (actual-prefix)
        (actual-short-name)
        (ps))
    (setq ps (shu-project-split-file-name file-name))
    (should ps)
    (should (consp ps))
    (setq actual-prefix (car ps))
    (setq actual-short-name (cdr ps))
    (should (string= expected-prefix actual-prefix))
    (should (string= expected-short-name actual-short-name))
    ))



;;
;;  shu-test-shu-project-split-file-name-3
;;
(ert-deftest shu-test-shu-project-split-file-name-3 ()
  (let ((file-name "lovely_AND_Wonderful.cpp")
        (expected-prefix "lovely")
        (expected-short-name "and_wonderful.cpp")
        (actual-prefix)
        (actual-short-name)
        (ps)
        (shu-cpp-project-very-short-names nil))
    (setq ps (shu-project-split-file-name file-name))
    (should ps)
    (should (consp ps))
    (setq actual-prefix (car ps))
    (setq actual-short-name (cdr ps))
    (should (string= expected-prefix actual-prefix))
    (should (string= expected-short-name actual-short-name))
    ))



;;
;;  shu-test-shu-project-split-file-name-4
;;
(ert-deftest shu-test-shu-project-split-file-name-4 ()
  (let ((file-name "x_lovely_Wonderful.cpp")
        (expected-prefix "x_lovely")
        (expected-short-name "wonderful.cpp")
        (actual-prefix)
        (actual-short-name)
        (ps))
    (setq ps (shu-project-split-file-name file-name))
    (should ps)
    (should (consp ps))
    (setq actual-prefix (car ps))
    (setq actual-short-name (cdr ps))
    (should (string= expected-prefix actual-prefix))
    (should (string= expected-short-name actual-short-name))
    ))



;;
;;  shu-test-shu-project-split-file-name-5
;;
(ert-deftest shu-test-shu-project-split-file-name-5 ()
  (let ((file-name "x_lovely_AND_Wonderful.cpp")
        (expected-prefix "x_lovely")
        (expected-short-name "and_wonderful.cpp")
        (actual-prefix)
        (actual-short-name)
        (ps)
        (shu-cpp-project-very-short-names nil))
    (setq ps (shu-project-split-file-name file-name))
    (should ps)
    (should (consp ps))
    (setq actual-prefix (car ps))
    (setq actual-short-name (cdr ps))
    (should (string= expected-prefix actual-prefix))
    (should (string= expected-short-name actual-short-name))
    ))



;;
;;  shu-test-shu-project-split-file-name-6
;;
(ert-deftest shu-test-shu-project-split-file-name-6 ()
  (let ((file-name "x_lovely__AND__Wonderful.cpp")
        (expected-prefix "x_lovely")
        (expected-short-name "and_wonderful.cpp")
        (actual-prefix)
        (actual-short-name)
        (ps)
        (shu-cpp-project-very-short-names nil))
    (setq ps (shu-project-split-file-name file-name))
    (should ps)
    (should (consp ps))
    (setq actual-prefix (car ps))
    (setq actual-short-name (cdr ps))
    (should (string= expected-prefix actual-prefix))
    (should (string= expected-short-name actual-short-name))
    ))



;;
;;  shu-test-shu-project-split-file-name-7
;;
(ert-deftest shu-test-shu-project-split-file-name-7 ()
  (let ((file-name "lovely_AND_Wonderful.cpp")
        (expected-prefix "lovely_AND")
        (expected-short-name "wonderful.cpp")
        (actual-prefix)
        (actual-short-name)
        (ps)
        (shu-cpp-project-very-short-names t))
    (setq ps (shu-project-split-file-name file-name))
    (should ps)
    (should (consp ps))
    (setq actual-prefix (car ps))
    (setq actual-short-name (cdr ps))
    (should (string= expected-prefix actual-prefix))
    (should (string= expected-short-name actual-short-name))
    ))



;;
;;  shu-test-shu-project-split-file-name-8
;;
(ert-deftest shu-test-shu-project-split-file-name-8 ()
  (let ((file-name "lovely_and_truly_very_much_wonderful.cpp")
        (expected-prefix "lovely_and_truly_very_much")
        (expected-short-name "wonderful.cpp")
        (actual-prefix)
        (actual-short-name)
        (ps)
        (shu-cpp-project-very-short-names t))
    (setq ps (shu-project-split-file-name file-name))
    (should ps)
    (should (consp ps))
    (setq actual-prefix (car ps))
    (setq actual-short-name (cdr ps))
    (should (string= expected-prefix actual-prefix))
    (should (string= expected-short-name actual-short-name))
    ))





;;
;;  shu-test-shu-project-make-short-key-list-1
;;
(ert-deftest shu-test-shu-project-make-short-key-list-1 ()
  (let (
        (data
         (list
          (cons "xxx_mumble.h"    (list "/foo/bar/xxx_mumble.h"))
          (cons "xxx_stumble.h"   (list "/boo/baz/xxx_stumble.h"
                                        "/foo/bar/xxx_stumble.h"))
          ))
        (expected-shorts
         (list
          (cons "mumble.h"    (list "/foo/bar/xxx_mumble.h"))
          (cons "stumble.h"   (list "/boo/baz/xxx_stumble.h"
                                    "/foo/bar/xxx_stumble.h"))))
        (expected-prefixes
         (list
          (cons "xxx" 2)))
        (ps)
        (actual-shorts)
        (actual-prefixes)
        )
    (setq ps (shu-project-make-short-key-list data))
    (should ps)
    (should (consp ps))
    (setq actual-prefixes (car ps))
    (setq actual-shorts (cdr ps))
    (should actual-prefixes)
    (should (listp actual-prefixes))
    (should (listp actual-prefixes))
    (should (equal expected-prefixes actual-prefixes))
    (should (equal expected-shorts actual-shorts))
    ))




;;
;;  shu-test-shu-project-make-short-key-list-2
;;
(ert-deftest shu-test-shu-project-make-short-key-list-2 ()
  (let ((data
         (list
          (cons "xxx_mumble.h"    (list "/foo/bar/xxx_mumble.h"))
          (cons "xxx_mumble.cpp"  (list "/foo/bar/xxx_mumble.cpp"))
          (cons "zzz_stumble.h"   (list "/boo/baz/zzz_stumble.h"
                                        "/foo/bar/zzz_stumble.h"))
          (cons "zzz_stumble.cpp" (list "/boo/baz/zzz_stumble.cpp"
                                        "/foo/bar/zzz_stumble.cpp"))))
        (expected-shorts
         (list
          (cons "mumble.cpp"      (list "/foo/bar/xxx_mumble.cpp"))
          (cons "mumble.h"        (list "/foo/bar/xxx_mumble.h"))
          (cons "stumble.cpp"     (list "/boo/baz/zzz_stumble.cpp"
                                        "/foo/bar/zzz_stumble.cpp"))
          (cons "stumble.h"       (list "/boo/baz/zzz_stumble.h"
                                        "/foo/bar/zzz_stumble.h"))))
        (expected-prefixes
         (list
          (cons "xxx" 2)
          (cons "zzz" 2)))
        (ps)
        (actual-shorts)
        (actual-prefixes))
    (setq ps (shu-project-make-short-key-list data))
    (should ps)
    (should (consp ps))
    (setq actual-prefixes (car ps))
    (setq actual-shorts (cdr ps))
    (should actual-prefixes)
    (should (listp actual-prefixes))
    (should (listp actual-prefixes))
    (should (equal expected-prefixes actual-prefixes))
    (should (equal expected-shorts actual-shorts))
    ))




;;
;;  shu-test-shu-project-make-short-key-list-3
;;
(ert-deftest shu-test-shu-project-make-short-key-list-3 ()
  (let ((data
         (list
          (cons "mumble.h"    (list "/foo/bar/mumble.h"))
          (cons "mumble.cpp"  (list "/foo/bar/mumble.cpp"))
          (cons "stumble.h"   (list "/boo/baz/stumble.h"
                                    "/foo/bar/stumble.h"))
          (cons "stumble.cpp" (list "/boo/baz/stumble.cpp"
                                    "/foo/bar/stumble.cpp"))))
        (ps)
        (actual-shorts)
        (actual-prefixes))
    (setq ps (shu-project-make-short-key-list data))
    (should ps)
    (should (consp ps))
    (setq actual-prefixes (car ps))
    (setq actual-shorts (cdr ps))
    (should (not actual-prefixes))
    (should (not actual-shorts))
    ))


;;
;;  shu-test-shu-cpp-project-extract-base-name-1
;;
(ert-deftest shu-test-shu-cpp-project-extract-base-name-1 ()
  (let (
        (base-name "brumble_mumble.h")
        (actual)
        (expected "brumble_mumble")
        )
    (setq actual (shu-cpp-project-extract-base-name base-name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;
;;  shu-test-shu-cpp-project-extract-base-name-2
;;
(ert-deftest shu-test-shu-cpp-project-extract-base-name-2 ()
  (let (
        (base-name "brumble_mumble.cpp")
        (actual)
        (expected "brumble_mumble")
        )
    (setq actual (shu-cpp-project-extract-base-name base-name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;
;;  shu-test-shu-cpp-project-extract-base-name-3
;;
(ert-deftest shu-test-shu-cpp-project-extract-base-name-3 ()
  (let (
        (base-name "brumble_mumble.i.cpp")
        (actual)
        (expected "brumble_mumble")
        )
    (setq actual (shu-cpp-project-extract-base-name base-name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))


;;
;;  shu-test-shu-cpp-project-extract-base-name-4
;;
(ert-deftest shu-test-shu-cpp-project-extract-base-name-4 ()
  (let (
        (base-name "brumble_mumble.t.cpp")
        (actual)
        (expected "brumble_mumble")
        )
    (setq actual (shu-cpp-project-extract-base-name base-name))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))

;;; shu-cpp-project.t.el ends here
