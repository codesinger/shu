;;; shu-git.t.el --- Shu project unit tests for code in shu-git.el
;;
;; Copyright (C) 2023 Stewart L. Palmer
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
;;  shu-git.t.el
;;
;;  unit tests for code in shu-git.el
;;


(require 'ert)
(require 'shu-git)

;;; Code



;;
;;  shu-test-shu-git-number-commits
;;
(ert-deftest shu-test-shu-git-number-commits ()
  (let ((data
         (concat
          "commit 545267aaca37b309196cd6aeacf4a0d0c17af17c\n"
          "Some text here\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "This is a short commit\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591\n\n"
          "The following does not start at bol:\n"
          " commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "Another commit:\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"))
        (expected
         (concat
          "     0. commit 545267aaca37b309196cd6aeacf4a0d0c17af17c\n"
          "Some text here\n"
          "     1. commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "This is a short commit\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591\n\n"
          "The following does not start at bol:\n"
          " commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "Another commit:\n"
          "     2. commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-git-number-commits))
      (should (= 3 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))



;;
;;  shu-test-shu-git-move-string-1
;;
(ert-deftest shu-test-shu-git-move-string-1 ()
  (let* (
        (old-file "input_something.h")
        (new-file "newer_something.h")
        (expected (concat "git mv " old-file " " new-file))
        (actual)
        )
    (setq actual (shu-git-move-string old-file new-file))
    (should actual)
    (should (stringp actual))
    (should (string= expected actual))
    ))




;;
;;  shu-test-shu-git-internal-pop-branch-1
;;
(ert-deftest shu-test-shu-git-internal-pop-branch-1 ()
  (let ((repo-branch))
    (setq shu-git-branch-stack nil)
    (setq repo-branch (shu-git-internal-pop-branch))
    (should (not repo-branch))
    ))




;;
;;  shu-test-shu-git-internal-pop-branch-2
;;
(ert-deftest shu-test-shu-git-internal-pop-branch-2()
  (let* ((repo "Bob")
         (branch "Alice")
         (expected (cons repo branch))
         (actual))
    (shu-git-internal-push-branch repo branch)
    (setq actual (shu-git-internal-pop-branch))
    (should actual)
    (should (consp actual))
    (should (equal actual expected))
    ))



;;
;;  shu-test-shu-git-internal-get-top-branch-1
;;
(ert-deftest shu-test-shu-git-internal-get-top-branch-1 ()
  (let* ((repo "Bob")
         (branch "Alice")
         (expected (cons repo branch))
         (actual))
    (shu-git-internal-push-branch repo branch)
    (setq actual (shu-git-internal-get-top-branch))
    (should actual)
    (should (consp actual))
    (should (equal actual expected))
    ))

;;; shu-git.t.el ends here
