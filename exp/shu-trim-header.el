;;; shu-trim-header.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2022 Stewart L. Palmer
;;
;; Package: shu-date
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


;;; Code:



;;
;;  shu-fix-header-line
;;
(defun shu-fix-header-line ()
  "If the first line of the buffer contains the sentinel \"-*-C++-*-\", adjust
the line length to be SHU-CPP-COMMENT-END in length, adding or removing
internal space as necessary.

If the first line of the buffer does not contain the sentinel \"-*-C++-*-\",
do nothing.

Return the number of spaces actually adjusted.  0 means no adjustment made.
A positive number represents the number of spaces added.  A negative number
represents the number of spaces removed."
  (let ((right-end (1+ shu-cpp-comment-end))
        (sentinel (concat " " shu-cpp-edit-sentinel))
        (count 0)
        (end-pos 0)
        (diff 0))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward sentinel (line-end-position) t)
        (setq end-pos (match-end 0))
        (if (< end-pos right-end)
            (progn
              (setq diff (- right-end end-pos))
              (setq count (shu-expand-header-line diff)))
          (when (> end-pos right-end)
            (setq diff (- end-pos right-end))
            (setq count (- (shu-trim-header-line diff)))))))
    count
    ))



;;
;;  shu-test-shu-fix-header-line-1
;;
(ert-deftest shu-test-shu-fix-header-line-1 ()
  (let* ((file-name "something_or0ther.h")
         (open-line (concat (shu-make-padded-line
                             (concat "// " file-name) (- shu-cpp-comment-end (length shu-cpp-edit-sentinel)))
                            shu-cpp-edit-sentinel))
         (data
          (concat
           open-line "\n"
           "\n"
           "/*!\n"
           " * \file something_orother.h\n"
           " *\n"
           " * \brief Declaration of SomethingOrOther\n"
           " */\n"))
         (end-pos 0)
         (new-end-pos 0)
         (count 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq count (shu-fix-header-line))
      (should count)
      (should (numberp count))
      (should (= count 0))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (= new-end-pos end-pos)))
    ))



;;
;;  shu-test-shu-fix-header-line-2
;;
(ert-deftest shu-test-shu-fix-header-line-2 ()
  (let* ((file-name "something_or0ther.h")
         (open-line (concat (shu-make-padded-line
                             (concat "// " file-name)
                             (- shu-cpp-comment-end (- (length shu-cpp-edit-sentinel) 3)))
                            shu-cpp-edit-sentinel))
         (data
          (concat
           open-line "\n"
           "\n"
           "/*!\n"
           " * \file something_orother.h\n"
           " *\n"
           " * \brief Declaration of SomethingOrOther\n"
           " */\n"))
         (end-pos 0)
         (new-end-pos 0)
         (count 0)
         (diff 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq count (shu-fix-header-line))
      (should count)
      (should (numberp count))
      (should (= count -3))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> end-pos new-end-pos))
      (setq diff (- end-pos new-end-pos))
      (should (= diff 3)))
    ))



;;
;;  shu-test-shu-fix-header-line-3
;;
(ert-deftest shu-test-shu-fix-header-line-3 ()
  (let* ((file-name "something_or0ther.h")
         (open-line (concat (shu-make-padded-line
                             (concat "// " file-name)
                             (- shu-cpp-comment-end (+ (length shu-cpp-edit-sentinel) 3)))
                            shu-cpp-edit-sentinel))
         (data
          (concat
           open-line "\n"
           "\n"
           "/*!\n"
           " * \file something_orother.h\n"
           " *\n"
           " * \brief Declaration of SomethingOrOther\n"
           " */\n"))
         (end-pos 0)
         (new-end-pos 0)
         (count 0)
         (diff 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq count (shu-fix-header-line))
      (should count)
      (should (numberp count))
      (should (= count 3))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> new-end-pos end-pos))
      (setq diff (- new-end-pos end-pos))
      (should (= diff 3)))
    ))



;;
;;  shu-trim-header-line
;;
(defun shu-trim-header-line (trim-count)
  "If the first line of the buffer contains the sentinel \"-*-C++-*-\", remove
TRIM-COUNT number of spaces from in front of the sentinel.

If the first line of the buffer does not contain the sentinel \"-*-C++-*-\",
do nothing.

If there do not exist enough spaces to remove TRIM-COUNT of them, remove
as many as possible.

Return the number of spaces actually removed."
  (let* ((sentinel (concat " " shu-cpp-edit-sentinel))
         (new-sentinel (concat " " sentinel))
         (end-pos)
         (something t)
         (count 0))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward sentinel (line-end-position) t)
        (while something
          (if (= count trim-count)
              (setq something nil)
            (goto-char (point-min))
            (if (not (search-forward new-sentinel (line-end-position) t))
                (setq something nil)
              (replace-match sentinel t t)
              (setq count (1+ count)))))))
    count
    ))




;;
;;  shu-expand-header-line
;;
(defun shu-expand-header-line (expand-count)
  "If the first line of the buffer contains the sentinel \"-*-C++-*-\", add
EXPAND-COUnt spaces in front of it.

If the first line of the buffer does not contain the sentinel \"-*-C++-*-\",
do nothing.

Return the number of spaces actually added."
  (let* ((pad (make-string expand-count ? ))
         (sentinel (concat " " shu-cpp-edit-sentinel))
         (new-sentinel (concat pad sentinel))
         (count 0))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward sentinel (line-end-position) t)
        (replace-match new-sentinel t t)
        (setq count expand-count)))
    count
    ))



;;
;;  shu-test-shu-expand-header-line-1
;;
(ert-deftest shu-test-shu-expand-header-line-1 ()
  (let ((data
         (concat
          "// something_orother.h                                      -*-C++-*-\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (end-pos 0)
        (new-end-pos 0)
        (expand-count 0)
        (diff))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq expand-count (shu-expand-header-line 8))
      (should expand-count)
      (should (numberp expand-count))
      (should (= expand-count 8))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> new-end-pos end-pos))
      (setq diff (- new-end-pos end-pos))
      (should (= diff expand-count)))
    ))



;;
;;  shu-test-shu-expand-header-line-2
;;
(ert-deftest shu-test-shu-expand-header-line-2 ()
  (let ((data
         (concat
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (expand-count 0))
    (with-temp-buffer
      (insert data)
      (setq expand-count (shu-expand-header-line 8))
      (should expand-count)
      (should (numberp expand-count))
      (should (= expand-count 0)))
    ))




;;
;;  shu-test-shu-trim-header-line-1
;;
(ert-deftest shu-test-shu-trim-header-line-1 ()
  (let ((data
         (concat
          "// something_orother.h                                      -*-C++-*-\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (end-pos 0)
        (new-end-pos 0)
        (trim-count 0)
        (diff 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq trim-count (shu-trim-header-line 8))
      (should trim-count)
      (should (numberp trim-count))
      (should (= trim-count 8))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> end-pos new-end-pos))
      (setq diff (- end-pos new-end-pos))
      (should (= diff trim-count)))
    ))



;;
;;  shu-test-shu-trim-header-line-2
;;
(ert-deftest shu-test-shu-trim-header-line-2 ()
  (let ((data
         (concat
          "// something_orother.h                               aaa   -*-C++-*-\n"
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (end-pos 0)
        (new-end-pos 0)
        (trim-count 0)
        (diff 0))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (setq end-pos (line-end-position))
      (goto-char (point-max))
      (setq trim-count (shu-trim-header-line 8))
      (should trim-count)
      (should (numberp trim-count))
      (should (= trim-count 2))
      (goto-char (point-min))
      (setq new-end-pos (line-end-position))
      (should (> end-pos new-end-pos))
      (setq diff (- end-pos new-end-pos))
      (should (= diff trim-count)))
    ))



;;
;;  shu-test-shu-trim-header-line-3
;;
(ert-deftest shu-test-shu-trim-header-line-3 ()
  (let ((data
         (concat
          "\n"
          "/*!\n"
          " * \file something_orother.h\n"
          " *\n"
          " * \brief Declaration of SomethingOrOther\n"
          " */\n"))
        (trim-count 0))
    (with-temp-buffer
      (insert data)
      (setq trim-count (shu-trim-header-line 8))
      (should trim-count)
      (should (numberp trim-count))
      (should (= trim-count 0)))
    ))


;;; shu-trim-header.el ends here
