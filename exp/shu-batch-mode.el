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
;;  try
;;
(defun try ()
  "Doc string."
  (interactive)
  (let (
        (xx "abcdef/")
        (new-string)
        )
    (setq new-string (shu-delete-last-char-if xx "/"))
    (message "'%s'" new-string)
    ))



;;
;;  shu-load-library-files
;;
(defun shu-load-library-files (path-to-libs)
  "Load all of the library files listed in SHU-LIBRARY-FILES using the path
PATH-TO-LIBS.  Return true if all of the files were successfully loaded."
  (let (
        (libs shu-library-files)
        (lib)
        (ln)
        (no-error nil)
        (no-message t)
        (no-suffix t)
        (loaded)
        (did-load t)
        )
    (while libs
      (setq lib (car libs))
      (setq ln (concat path-to-libs "/" lib))
      (setq loaded (load ln no-error no-message no-suffix))
      (when (not loaded)
        (setq did-load nil)
        )
      (setq libs (cdr libs))
      )
    did-load
    ))


;;
;;  shu-batch-init
;;
(defun shu-batch-init ()
  "Load all of the .elc files from the Shu elisp package.  This is used to allow
Shu elisp functions to be used in batch mode.  This function searches for the
functions in the path specified by the environment variable
\"SHU_EMACS_LOAD_PATH\".  If that environment variable does not exist, then it
searches in the local \"~/emacs\" directory."
  (let ((path-to-libs (getenv "SHU_EMACS_LOAD_PATH"))
        (shu-libs
         (list
          "shu-base.elc"
          ))
        (shu-conditional-libs
         (list
          "rmv-using.elc"
          ))
        (libs)
        (lib)
        (ln)
        (no-error nil)
        (no-message t)
        (no-suffix t)
        (loaded)
        (load-errors))
    (when (not path-to-libs)
      (setq path-to-libs "~/emacs"))
    (setq libs shu-libs)
    (while libs
      (setq lib (car libs))
      (setq ln (concat path-to-libs "/" lib))
      (setq loaded (load ln no-error no-message no-suffix))
      (when (not loaded)
        (setq load-errors t))
      (setq libs (cdr libs)))
    (setq libs shu-library-files)
    (while libs
      (setq lib (car libs))
      (setq ln (concat path-to-libs "/" lib))
      (setq loaded (load ln no-error no-message no-suffix))
      (when (not loaded)
        (setq load-errors t))
      (setq libs (cdr libs)))
    (setq libs shu-conditional-libs)
    (while libs
      (setq lib (car libs))
      (setq ln (concat path-to-libs "/" lib))
      (when (file-readable-p ln)
        (setq loaded (load ln no-error no-message no-suffix))
        (when (not loaded)
          (setq load-errors t)))
      (setq libs (cdr libs)))
    (when load-errors
      (message "%s" "Quitting due to load errors")
      (kill-emacs))
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
    (message "%s" shu-cpp-operators-three)
    ))


;;; shu-batch-mode.el ends here
