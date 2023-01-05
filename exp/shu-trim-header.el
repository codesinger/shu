;;; shu-trim-header.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2022 Stewart L. Palmer
;;
;; Package: shu-trim-header
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
;;  try
;;
(defun try ()
  "Doc string."
  (interactive)
  (let* (
         (old-namespace "oldnamespace")
         (new-namespace "newernamespace")
         (file-name (concat old-namespace "_mumblebar.t.cpp"))
         (ss (concat old-namespace "_"))

         )
    (re-search-forward ss nil t)
    ))

;; oldnamespace_mumblebar.t.cpp



;;
;;  dry
;;
(defun dry ()
  "Doc string."
  (interactive)
  (let (
        (in-git (shu-git-is-file-in-git "try.el"))
        )
    (message "in-git: %s" (shu-bool-to-string in-git))
    ))



;;
;;  rename-something
;;
(defun rename-something ()
  "Doc string."
  (interactive)
  (let* (
         (log-buffer-name "**shu-rename-log**")
         (log-buffer (get-buffer-create log-buffer-name))
         (root "testdata")
         (pattern "fxcrossbase*")
         (cpp-type t)
         (old-namespace "fxcrossbase")
         (new-namespace "fxnewersomething")
         (files)
         (file)
         (ff)
         (newfiles)
         (newfile)
         (cf)
         (did)
         (debug-on-error t)
         )
    (setq files (shu-project-get-specific-files root pattern cpp-type))
    (if (not files)
        (progn
          (ding)
          (message "%s" "No files found to modify")
          )
      (princ (concat "\n\nChange namespace from '" old-namespace "' to '" new-namespace "'\n"
                     "for the following files:\n\n") log-buffer)
      (dump-list files log-buffer)
      (setq ff files)
      (while ff
        (setq file (car ff))
        (setq newfile (shu-replace-namespace-in-file-name file old-namespace new-namespace))
        (setq cf (cons file newfile))
        (push cf newfiles)
        (setq ff (cdr ff))
        )
      (dump-clist newfiles log-buffer)
      (setq did (rename-files newfiles log-buffer))
      (if (not did)
          (progn
            (ding)
            (message "Rename failed.  See buffer %s" log-buffer-name)
            )
        (setq did (edit-files newfiles old-namespace new-namespace log-buffer))
        (if (not did)
            (progn
              (ding)
              (message "Edits failed.  See buffer %s" log-buffer-name)
              )
          (message "Namespace replacement complete.  See buffer %s." log-buffer-name)
            )
        )


      )
    ))



;;
;;  rename-files
;;
(defun rename-files (newfiles log-buffer)
  "Doc string."
  (interactive)
  (let (
        (nf newfiles)
        (cf)
        (cmd)
        (moved)
        (result)
        (old-file)
        (new-file)
        )
    (setq moved t)
    (princ "\n" log-buffer)
    (while (and nf moved)
      (setq cf (car nf))
      (setq old-file (car cf))
      (setq new-file (cdr cf))
      (setq cmd (shu-move-string old-file new-file))
      (if (not (shu-git-is-file-in-git old-file))
          (setq cf (shu-move-file old-file new-file))
        (setq cmd (shu-git-move-string old-file new-file))
        (setq cf (shu-git-move-file old-file new-file))
        )
      (setq moved (car cf))
      (setq result (cdr cf))
      (princ (concat cmd "\n") log-buffer)
      (when (not moved)
        (princ (concat "Move failed\n" result "\n") log-buffer)
        )
      (setq nf (cdr nf))
      )
    moved
    ))



;;
;;  edit-files
;;
(defun edit-files  (newfiles old-namespace new-namespace log-buffer)
  "Return true if all of the edits work."
  (let (
        (nf newfiles)
        (cf)
        (old-file)
        (new-file)
        (failed)
        (fbuf)
        (file-buf)
        (count 0)
        (total-count 0)
        (directory-names)
        (directory-part)
        )
    (princ "\n" log-buffer)
    (while (and nf (not failed))
      (setq cf (car nf))
      (setq new-file (cdr cf))
      (setq directory-part (file-name-directory new-file))
      (when (not (member-ignore-case directory-part directory-names))
        (push directory-part directory-names)
        )
      (setq fbuf (get-file-buffer new-file))
      (if fbuf
          (setq file-buf fbuf)
        (setq file-buf (find-file-noselect new-file))
        )
      (set-buffer file-buf)
      (when (not fbuf)
        (make-local-variable 'backup-inhibited)
        (setq backup-inhibited t)
        )
      (princ (concat "Replacing namespace in '" new-file "'\n") log-buffer)
      (setq count (shu-replace-namespace-in-buffer old-namespace new-namespace))
      (setq total-count (+ total-count count))
      (when (= count 0)
        (princ "***Edit failed.  No strings replaced.***\n" log-buffer)
        (setq failed t)
        )
      (when (buffer-modified-p)
        (basic-save-buffer)
        )
      (when (not fbuf)
        (kill-buffer file-buf)
        )
      (setq nf (cdr nf))
      )
    (princ (format "\n%s items changed in %s files in %s directories.\n"
                   (shu-group-number total-count)
                   (shu-group-number (length newfiles))
                   (shu-group-number (length directory-names))) log-buffer)
    (not failed)
    ))



;;
;;  dump-clist
;;
(defun dump-clist (files log-buffer)
  "Doc string."
  (let (
        (ff files)
        (longest-name 0)
        (cf)
        (file)
        (dfile)
        (newfile)
        )
    (setq longest-name (shu-longest-car-length files))
    (princ "\nFiles will be renamed as follows:\n" log-buffer)
    (setq ff files)
    (while ff
      (setq cf (car ff))
      (setq file (car cf))
      (setq newfile (cdr cf))
      (setq dfile (shu-make-padded-line file longest-name))
      (princ (concat dfile " --> " newfile "\n") log-buffer)
      (setq ff (cdr ff))
      )
    ))



;;
;;  dump-list
;;
(defun dump-list (files log-buffer)
  "Doc string."
  (interactive)
  (let (
        (file)
        )
    (while files
      (setq file (car files))
      (princ (concat file "\n") log-buffer)
      (setq files (cdr files))
      )


    ))



;;
;;  dump-file
;;
(defun dump-file (log-buffer)
  "Doc string."
  (interactive)
  (let (
        (count 0)
        (line)
        )
    (goto-char (point-min))
    (princ (concat "\n" (buffer-file-name) "\n") log-buffer)
    (while (< count 4)
      (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      (princ line log-buffer)
      (forward-line 1)
      (setq count (1+ count))
      )
    ))


;;; shu-trim-header.el ends here
