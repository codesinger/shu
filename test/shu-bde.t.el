;;; shu-bde.t.el --- Shu project unit tests for code in shu-bde.el
;;
;; Copyright (C) 2022 Stewart L. Palmer
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
;;  shu-bde.t.el
;;
;;  unit tests for code in shu-bde.el
;;


(require 'ert)
(require 'shu-bde)

;;; Code


;;
;;  shu-test-shu-cpp-insert-template-decl-1
;;
(ert-deftest shu-test-shu-cpp-insert-template-decl-1 ()
  (let ((template-list)
        (actual)
        (expected ""))
    (with-temp-buffer
      (shu-cpp-insert-template-decl template-list)
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-insert-template-decl-2
;;
(ert-deftest shu-test-shu-cpp-insert-template-decl-2 ()
  (let ((template-list (list "S" "T"))
        (actual "")
        (expected "template<typename S, typename T>\ninline\n"))
    (with-temp-buffer
      (shu-cpp-insert-template-decl template-list)
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-1
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-1 ()
  (let ((template-list (list "T"))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar<T>"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-2
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-2 ()
  (let ((template-list (list "T" "S" "Q"))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar<T, S, Q>"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-3
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-3 ()
  (let ((template-list (list))
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-make-qualified-class-name-4
;;
(ert-deftest shu-test-shu-cpp-make-qualified-class-name-4 ()
  (let ((template-list)
        (class-name "MumbleBar")
        (actual)
        (expected "MumbleBar"))
    (setq actual (shu-cpp-make-qualified-class-name class-name template-list))
    (should actual)
    (should (stringp actual))
    (should (string= actual expected))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-1
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-1 ()
  (let ((tp-string "a")
        (tp-list))
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should tp-list)
    (should (listp tp-list))
    (should (= 1 (length tp-list)))
    (should (string= "a" (car tp-list)))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-2
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-2 ()
  (let (
        (tp-string "a, b")
        (tp-list)
        )
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should tp-list)
    (should (listp tp-list))
    (should (= 2 (length tp-list)))
    (should (string= "a" (car tp-list)))
    (should (string= "b" (cadr tp-list)))
    ))



;;
;;  shu-test-shu-cpp-split-template-parameter-list-3
;;
(ert-deftest shu-test-shu-cpp-split-template-parameter-list-3 ()
  (let ((tp-string "")
        (tp-list))
    (setq tp-list (shu-cpp-split-template-parameter-list tp-string))
    (should (not tp-list))
    ))





;;
;;  shu-test-shu-cpp-make-decl-template-1
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-1 ()
  (let ((template-list (list "T"))
        (actual)
        (expected "template<typename T>"))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-decl-template-2
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-2 ()
  (let ((template-list (list "T" "P" "Q"))
        (actual)
        (expected "template<typename T, typename P, typename Q>"))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;
;;  shu-test-shu-cpp-make-decl-template-3
;;
(ert-deftest shu-test-shu-cpp-make-decl-template-3 ()
  (let ((template-list)
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-decl-template template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-1
;;
(ert-deftest shu-test-shu-cpp-make-template-list-1 ()
  (let ((template-list (list "T"))
        (actual)
        (expected "<T>"))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-2
;;
(ert-deftest shu-test-shu-cpp-make-template-list-2 ()
  (let ((template-list (list "T" "P" "Q"))
        (actual)
        (expected "<T, P, Q>"))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-3
;;
(ert-deftest shu-test-shu-cpp-make-template-list-3 ()
  (let ((template-list (list))
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-cpp-make-template-list-4
;;
(ert-deftest shu-test-shu-cpp-make-template-list-4 ()
  (let ((template-list)
        (actual)
        (expected ""))
    (setq actual (shu-cpp-make-template-list template-list))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))



;;; shu-bde.t.el ends here
