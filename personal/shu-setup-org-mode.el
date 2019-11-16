;;; shu-setup-org-mode.el --- Set up org mode
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-misc
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

;;
;; This file contains functions that I use to set up org mode in the
;; varioius environments in which I work and play.
;;
;; It is not part of the Shu elisp package

;;; Code:



;;
;;  shu-setup-org-mode
;;
(defun shu-setup-org-mode ()
  "This is a function that sets up org mode in the various environments in which
I work and play.  It is not part of the Shu elisp package."
  (interactive)
  (let (
        (gb (get-buffer-create "**shu-org**"))
        (enable-org-mode nil)
        )
    (if (shu-system-type-is-mac-osx)
        (progn
          (princ "On Mac OSX\n" gb)
          (setq shu-org-home "~/data/org")
          (when (file-readable-p (concat shu-org-home "/home.org"))
            (princ (concat "Found file: " shu-org-home "/home.org" "\n") gb)
            (setq enable-org-mode t)
            ;; Allow links from org files to Apple Mail messages
            (add-to-list 'org-modules 'org-mac-message)
            (add-to-list 'org-modules 'org-habit)
            (setq org-mac-mail-account "mail.stewartpalmer.com")
            ;; Alias to insert link to currently selected Apple Mail message
            (defalias 'imail 'org-mac-message-insert-selected)
            )
          )
      (when (shu-system-type-is-unix)
        (progn
          (princ "On Unix\n" gb)
          (setq shu-org-home "~/mbig/2019-11-16-work")
          (when (file-readable-p (concat shu-org-home "/work.org"))
            (princ (concat "Found file: " shu-org-home "/work.org" "\n") gb)
            (setq enable-org-mode t)
            )
          )
        )
      )
    (when (not enable-org-mode)
      (princ "enable-org-mode is false\n" gb)
      )
    (when enable-org-mode
      (princ "enable-org-mode is true\n" gb)
      (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (setq org-log-done t)
      (setq org-agenda-files (concat shu-org-home "/files.txt"))
      (setq org-directory shu-org-home)
      (setq org-default-notes-file (concat org-directory "/notes.org"))
      (global-set-key "\C-c\C-c" 'org-capture)

      (setq org-todo-keywords
            '((sequence "TODO(t!)" "WAITING(w@)" "|" "CANCELLED(c!)" "DONE(d!)")))
      ;; These are the keywords that represent the " notdone" state in the above list
      (setq shu-org-todo-keywords
            (list "TODO" "WAITING"))
      ;; These are the keywords that represent the "done" state in the above list
      ;; Used to identify "stuck" projects, which are ones which are not done
      (setq shu-org-done-keywords
            (list "CANCELLED" "DONE"))

      ;; Parent cannot be marked DONE until all children are DONE
      (setq org-enforce-todo-dependencies t)

      (setq org-outline-path-complete-in-steps t)
      (setq org-refile-use-outline-path 'file)
      (setq org-refile-targets '((org-agenda-files . (:level . 2))))

      (setq org-agenda-start-with-follow-mode t)

      ;; Stuck projects are those that are not marked DONE and have neither
      ;;  deadline nor scheduled time
      (setq org-stuck-projects (list (shu-org-done-projects-string) nil nil "SCHEDULED:\\|DEADLINE:"))

      ;; Number of elapsed days before a closed TODO item is automatically archived.
      (setq shu-org-archive-expiry-days 7)
      (setq safe-local-variable-values (quote ((after-save-hook archive-done-tasks))))
      (defalias 'archive-done-tasks 'shu-org-archive-done-tasks)

      (setq org-mobile-use-encryption t)
      (setq org-mobile-encryption-password "Mrs67GreenGenes")
      (setq org-mobile-directory "~/Dropbox/MobileOrg")
      (setq org-mobile-inbox-for-pull (concat org-directory "/" "from-mobile.org"))
      (setq org-mobile-force-id-on-agenda-items nil)
      (setq shu-org-mode-is-set t)
      )
    ))


;;; shu-misc.el ends here


;;; shu-system-type-is-unix

;;; ~/mbig/2019-11-16-work
