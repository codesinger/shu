;;; shu-new-cpp-project.t.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-project
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

;;; Commentary:

;; Experimental code that may someday be added to shu-cpp-project

;;; Code:

(require 'ert)
(require 'shu-cpp-project)





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
;;  shu-test-shu-project-split-file-name-6
;;
(ert-deftest shu-test-shu-project-split-file-name-6 ()
  (let ((file-name "x_lovely__AND__Wonderful.cpp")
        (expected-prefix "x_lovely")
        (expected-short-name "and_wonderful.cpp")
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
;;  shu-test-shu-project-make-short-key-list-1
;;
(ert-deftest shu-test-shu-project-make-short-key-list-1 ()
  (let ((data
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))))
        (expected-shorts
         (list
          (cons "mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "stumble.h"   (list (list "/boo/baz/xxx_stumble.h"
                                              "/foo/bar/xxx_stumble.h")))))
        (expected-prefixes
         (list
          (cons "xxx" 2)))
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
;;  shu-test-shu-project-make-short-key-list-2
;;
(ert-deftest shu-test-shu-project-make-short-key-list-2 ()
  (let ((data
         (list
          (cons "xxx_mumble.h"    (list (list "/foo/bar/xxx_mumble.h")))
          (cons "xxx_mumble.cpp"  (list (list "/foo/bar/xxx_mumble.cpp")))
          (cons "zzz_stumble.h"   (list (list "/boo/baz/zzz_stumble.h"
                                              "/foo/bar/zzz_stumble.h")))
          (cons "zzz_stumble.cpp" (list (list "/boo/baz/zzz_stumble.cpp"
                                              "/foo/bar/zzz_stumble.cpp")))))
        (expected-shorts
         (list
          (cons "mumble.cpp"      (list (list "/foo/bar/xxx_mumble.cpp")))
          (cons "mumble.h"        (list (list "/foo/bar/xxx_mumble.h")))
          (cons "stumble.cpp"     (list (list "/boo/baz/zzz_stumble.cpp"
                                              "/foo/bar/zzz_stumble.cpp")))
          (cons "stumble.h"       (list (list "/boo/baz/zzz_stumble.h"
                                              "/foo/bar/zzz_stumble.h")))))
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


;;; shu-new-cpp-project.t.el ends here
