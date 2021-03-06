
;;; shu-batch-mode.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-batch-mode
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

;; A startup script and other functions for running the Shu elisp package
;; in batch mode.  Some of the functions in this package have been
;; enabled to run as stand alone shell scripts.  This allows these
;; functions to be used by non-emacs users and in automated build
;; work flows.
;;
;; The function shu-batch-init loads all of the Shu elisp libraries.
;; Other functions in this file invoke various functions in the Shu
;; elisp libraries and are set up such that they can be invoked from
;; the --eval comment line option of emacs running in batch.


;;; Code:


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
        (base-lib "shu-base.elc")
        (conditional-libs
         (list
          "rmv-using.elc"
          "slp-comment-hooks.elc"
          "slp-bb-comment-hooks.elc"))
        (ln)
        (no-error nil)
        (no-message t)
        (no-suffix t)
        (loaded)
        (did-load)
        (load-errors))
    (when (not path-to-libs)
      (setq path-to-libs "~/emacs"))
    (setq ln (concat path-to-libs "/" base-lib))
    (setq loaded (load ln no-error no-message no-suffix))
    (if (not loaded)
        (setq load-errors t)
      (setq did-load (shu-load-library-files path-to-libs))
      (setq load-errors (not did-load))
      (setq did-load (shu-conditional-load-library-files path-to-libs conditional-libs))
      (setq load-errors (not did-load)))
    (when load-errors
      (message "%s" "Quitting due to load errors")
      (kill-emacs))
    (message "emacs: %s, Shu: %s" emacs-version shu-version)
    ))




;;
;;  shu-batch-hello
;;
(defun shu-batch-hello ()
  "A test function for batch mode."
  (interactive)
  (let (
        (log-buf nil)
        )
    (shu-batch-init)
    (message "%s" "Hello")
    (message "%s" shu-cpp-operators-three)
    (princ "princ output\n" log-buf)
    ))


(defconst shu-local-class-list
  (list
   (cons "bsl"
         (list
               "boolalpha"
               "cout"
               "dec"
               "endl"
               "exception"
               "fixed"
               "hex"
               "ifstream"
               "ios_base"
               "map"
               "noboolalpha"
               "ostream"
               "ostringstream"
               "pair"
               "runtime_error"
               "set"
               "setfill"
               "setprecision"
               "setw"
               "sort"
               "string"
               "stringstream"
               "vector"))
   (cons "std"
         (list
               "boolalpha"
               "cout"
               "dec"
               "endl"
               "exception"
               "fixed"
               "hex"
               "ifstream"
               "ios_base"
               "map"
               "noboolalpha"
               "ostream"
               "ostringstream"
               "pair"
               "runtime_error"
               "set"
               "setfill"
               "setprecision"
               "setw"
               "sort"
               "string"
               "stringstream"
               "vector"))
   )
  "List of standard namespaces and their associated classes")



;;
;;  shu-batch-rmv-using
;;
(defun shu-batch-rmv-using ()
  "Call rmv-using in batch mode."
  (let ((class-path (getenv "SHU_RMV_USING_PATH"))
        (file-name (getenv "SHU_RMV_USING_FILE"))
        (class-load-path)
        (class-load-name)
        (top-name "BloombergLP")
        (log-buf nil))
    (if (not class-path)
        (message "%s" "SHU_RMV_USING_PATH environment variable not set.")
      (if (not (file-readable-p file-name))
          (message "Cannot file file: %s" file-name)
        (shu-batch-init)
        (setq class-load-path (shu-delete-last-char-if class-path "/"))
        (setq class-load-name (concat class-load-path "/rmv-using.el"))
        (load-file class-load-name)
        (find-file file-name)
        (if buffer-read-only
            (message "File %s is read only." file-name)
          (shu-match-internal-rmv-using shu-local-class-list log-buf top-name)
          (when (buffer-modified-p)
            (basic-save-buffer)))))
    ))



;;
;;  shu-generate-component
;;
(defun shu-generate-component ()
  "Fetch the arguments from environment variables and then call
SHU-INTERNAL-GEN-BDE-COMPONENT to generate a set of three BDE component
files."
  (let ((class-name (getenv "SHU_CLASS_NAME"))
        (author (getenv "SHU_AUTHOR"))
        (namespace (getenv "SHU_NAMESPACE"))
        (global-namespace (getenv "SHU_GLOBAL_NAMESPACE"))
        (file-prefix (getenv "SHU_FILE_PREFIX")))
    (shu-batch-init)
    (slp-bb-set-comment-hooks)
    (when (not class-name)
      (message "%s" "SHU_CLASS_NAME is not set."))
    (when (not author)
      (message "%s" "SHU_AUTHOR is not set."))
    (when (not namespace)
      (message "%s" "SHU_NAMESPACE is not set."))
    (when (not global-namespace)
      (message "%s" "SHU_GLOBAL_NAMESPACE is not set."))
    (when (not file-prefix)
      (setq file-prefix (concat namespace "_")))
    (setq shu-cpp-default-global-namespace global-namespace)
    (setq shu-cpp-use-bde-library t)
    (shu-internal-gen-bde-component class-name author namespace file-prefix)
    ))


;;; shu-batch-mode.el ends here
