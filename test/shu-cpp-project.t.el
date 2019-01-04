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
      (should (= 2 (length result)))
      (should (string= file (car result)))
      (should (= line (cadr result))))
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

;;; shu-cpp-project.t.el ends here
