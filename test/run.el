;;; run.el --- Shu project unit tests for code in shu-misc.el
;;
;; Copyright (C) 2018 Stewart L. Palmer
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

;;; Commentary:

;;
;;  run.el
;;
;;  Run all unit tests for code in shu elisp
;;

;;; Code:


(defun jj ()
  (interactive)
  (shu-run-all-unit-tests)
  )

;;
;;  shu-run-all-unit-tests
;;
(defun shu-run-all-unit-tests ()
  "Compile and run all of the unit tests.  This must be run from the test directory."
  (interactive)
  (let
      ((test-files
        (list
;;;         "find.t.elc"
;;;         "macros.t.elc"
;;;         "s-mode.t.elc"
         "shu-base.t.elc"
         "shu-bde.t.el"
         "shu-capture-doc.t.elc"
         "shu-cpp-general.t.elc"
         "shu-cpp-match.t.elc"
         "shu-cpp-misc.t.elc"
         "shu-cpp-project.t.elc"
         "shu-cpp-token.t.elc"
         "shu-git.t.elc"
         "shu-match.t.elc"
         "shu-misc.t.elc"
         "shu-nvplist.t.elc"
         "shu-xref.t.elc"))
       (file-name)
       (tlist))
    (byte-force-recompile ".")
    (setq tlist test-files)
    (while tlist
      (setq file-name (car tlist))
      (load-file file-name)
      (setq tlist (cdr tlist)))
    (ert t)
    ))

;;; run.el ends here
