;;; shu-misc.t.el --- Shu project unit tests for code in shu-misc.el
;;
;; Copyright (C) 2018 Stewart L. Palmer
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
;;  shu-misc.t.el
;;
;;  unit tests for code in shu-misc.el
;;


(require 'ert)
(require 'shu-misc)


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
          "     1. commit 545267aaca37b309196cd6aeacf4a0d0c17af17c\n"
          "Some text here\n"
          "     2. commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "This is a short commit\n"
          "commit 03fd2e4f90676e7b91f156de9986d6b08e4591\n\n"
          "The following does not start at bol:\n"
          " commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"
          "Another commit:\n"
          "     3. commit 03fd2e4f90676e7b91f156de9986d6b08e4591d6\n\n"))
        (actual)
        (count 0))
    (with-temp-buffer
      (insert data)
      (setq count (shu-git-number-commits))
      (should (= 3 count))
      (setq actual (buffer-substring-no-properties (point-min) (point-max)))
      (should (string= expected actual))
      )))
