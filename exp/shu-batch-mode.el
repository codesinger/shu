;;; shu-batch-mode.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-mch-funs
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

;; A startup script for running the Shu elisp package in batch mode


;;; Code:


;;
;;  shu-batch-init
;;
(defun shu-batch-init ()
  "Doc string."
  (let (
        (path-to-libs (getenv "SHU_EMACS_LOAD_PATH"))
        (shu-libs
         (list
          "shu-base.elc"
          "shu-misc.elc"
          "shu-cpp-token.elc"
          "shu-cpp-general.elc"
          "shu-cpp-misc.elc"
          "shu-cpp-match.elc"
          "shu-match.elc"
          "shu-bde.elc"
          "shu-bde-cpp.elc"
          "shu-cpp-project.elc"
          "shu-nvplist.elc"
          "slp-comment-hooks.elc"
          "slp-bb-comment-hooks.elc"
          ))
        (shu-conditional-libs
         (list
          "rmv-using.elc"
          ))
        (libs)
        (lib)
        (ln)
        )
    (when (not path-to-libs)
      (setq path-to-libs "~/emacs")
      )
    (setq libs shu-libs)
    (while libs
      (setq lib (car libs))
      (setq ln (concat path-to-libs "/" lib))
      (load-file ln)
      (setq libs (cdr libs))
      )
    (setq libs shu-conditional-libs)
    (while libs
      (setq lib (car libs))
      (setq ln (concat path-to-libs "/" lib))
      (when (file-readable-p ln)
        (load-file ln)
        )
      (setq libs (cdr libs))
      )
    ))


;;
;;  hello
;;
(defun hello ()
  "Doc string."
  (interactive)
  (let (
        )
    (shu-batch-init)
    (message "%s" "Hello")
    ))


;;; shu-batch-mode.el ends here
