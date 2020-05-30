;;; shu-cpp-misc.t.el --- Shu project unit tests for code in shu-misc.el
;;
;; Copyright (C) 2020 Stewart L. Palmer
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
;;  shu-cpp-misc.t.el
;;
;;  unit tests for code in shu-cpp-misc.el
;;


(require 'ert)
(require 'shu-cpp-misc)

;;; Code




;;
;;  shu-test-shu-cpp-decl-cpp-class-name
;;
(ert-deftest shu-test-shu-cpp-decl-cpp-class-name ()
  (let* ((class-name "MumbleFrotz")
        (expected
         (concat
          "                        // -----------------\n"
          "                        // class MumbleFrotz\n"
          "                        // -----------------\n"))
        (actual))
    (with-temp-buffer
     (goto-char (point-min))
     (shu-cpp-decl-cpp-class-name class-name)
     (goto-char (point-min))
     (search-forward "----" nil t)
     (beginning-of-line)
     (setq actual (buffer-substring-no-properties (point) (point-max)))
     (should (string= expected actual)))
    ))



;;
;;  shu-test-shu-cpp-decl-h-class-name
;;
(ert-deftest shu-test-shu-cpp-decl-h-class-name ()
  (let* ((class-name "MumbleFrotz")
        (expected
         (concat
          "                        // =================\n"
          "                        // class MumbleFrotz\n"
          "                        // =================\n"))
        (actual))
    (with-temp-buffer
     (goto-char (point-min))
     (shu-cpp-decl-h-class-name class-name)
     (goto-char (point-min))
     (search-forward "====" nil t)
     (beginning-of-line)
     (setq actual (buffer-substring-no-properties (point) (point-max)))
     (should (string= expected actual)))
    ))




;;; shu-cpp-misc.t.el ends here
