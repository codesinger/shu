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
