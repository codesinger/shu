;;; .emacs - My personal .emacs file
;;
;; Copyright (C) 201D85 Stewart L. Palmer
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

;; This is my oersonal .emacs file.
;;
;; It is not part of the shu package


(setq home (getenv "HOME"))
  (setq load-path
        (append (list (concat home "/emacs"))
          (append load-path))
  )

(setq slp-org-location "~/emacs/org-7.8.09")
(when (file-readable-p (concat slp-org-location "/lisp/org-install.el"))
  (setq load-path (cons (concat slp-org-location "/lisp/") load-path))
  (setq load-path (cons (concat slp-org-location "/contrib/lisp/") load-path))
  (load-file (concat slp-org-location "/lisp/org.elc"))
  (load-file (concat slp-org-location "/lisp/org-agenda.elc"))
)
(tool-bar-mode 0)

;;;
(global-set-key "" 'buffer-menu)
(global-set-key "" 'shell)
(global-set-key "\C-x\C-c" 'shu-disabled-quit)
(global-set-key "\C-x\C-j" 'shu-vj)
(global-set-key "\C-x\C-h" 'shu-vh)
(global-set-key "\C-x\C-i" 'overwrite-mode)
(global-set-key [(control ?2)] 'set-mark-command)
(global-set-key [(control ?-)] 'undo)
(when (> emacs-major-version 22)
  (global-set-key [?\C--] 'undo)
  (global-set-key [?\C-2] 'set-mark-command))
(setq dired-listing-switches "-alt")
(setq list-directory-verbose-switches "-alt")
(setq auto-save-default nil)
(setq backup-by-copying nil)
(setq backup-by-copying-when-linked t)
(setq inhibit-startup-message t)
;;
;;
(setq default-major-mode 'text-mode)
(setq text-mode-hook 'turn-on-auto-fill)
(setq auto-mode-alist (cons '("\\.md\\'" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.gpg\\'" . text-mode) auto-mode-alist))
;;
(setq kept-new-versions 6)
(setq kept-old-versions 0)
(setq list-matching-lines-default-context-lines 2)
(setq require-final-newline t)
(setq version-control t)
(setq display-time-day-and-date t)
(setq eol-mnemonic-undecided "(?)")
(setq eol-mnemonic-unix "(unix)")
(setq eol-mnemonic-dos "(dos)")
(setq eol-mnemonic-max "(mac)")
;;

;;
;;(setq default-frame-alist '((width . 115) (height . 66) (tool-bar-lines . 0) (menu-bar-lines . 0)))
;;(setq default-frame-alist '((fullscreen . fullheight) (width . 115) (tool-bar-lines . 0) (menu-bar-lines . 0)))
(setq default-frame-alist '((fullscreen . fullheight) (tool-bar-lines . 0) (menu-bar-lines . 0) (width . 115)))
;(set-frame-parameter nil 'width '115)
;;
;;'(fullscreen . fullheight))
;
(setq shell-pushd-regexp "pu")
(setq shell-popd-regexp "po")
;
; Set ispell
(if (eq system-type 'darwin)
  (setq ispell-program-name "/opt/local/bin/ispell")   ;; For Mac OS X
  (setq ispell-program-name "/opt/swt/bin/aspell"))    ;; For all others


(put 'eval-expression 'disabled nil)
(load-library "cc-mode")
(load-file "~/emacs/shu-base.elc")
(load-file "~/emacs/shu-misc.elc")
;;(load-file "~/emacs/s-mode.elc")
(when (file-readable-p "~/emacs/useful.elc")
  (load-file "~/emacs/useful.elc"))
(load-file "~/emacs/macros.elc")
;;(load-file "~/emacs/clearcase.elc");;Incompatible with Emacs 24
;;(load-file "~/emacs/visible.elc")
;;(load-file "~/emacs/slp-java-base.elc")
(load-file "~/emacs/shu-cpp-general.elc")
(load-file "~/emacs/shu-cpp-misc.elc")
(when (file-readable-p "~/emacs/slpbb.elc")
  (load-file "~/emacs/slpbb.elc"))
(when (file-readable-p "~/emacs/rmv-using.elc")
  (load-file "~/emacs/rmv-using.elc"))
(load-file "~/emacs/shu-cpp-token.elc")
(load-file "~/emacs/shu-bde.elc")
(load-file "~/emacs/shu-bde-cpp.elc")
(load-file "~/emacs/shu-cpp-project.elc")
(load-file "~/emacs/shu-org-extensions.elc")
(load-file "~/emacs/shu-nvplist.elc")
(load-file "~/emacs/shu-keyring.elc")
(load-file "~/emacs/shu-capture-doc.elc")
(load-file "~/emacs/shu-xref.elc")
(load-file "~/emacs/slp-comment-hooks.elc")
(load-file "~/emacs/slp-bb-comment-hooks.elc")
(when (file-readable-p "~/emacs/trust.elc")
  (load-file "~/emacs/trust.elc"))


;;
;;  Aliases for shu
;;
(shu-cpp-general-set-alias)
(shu-cpp-token-set-alias)
(shu-bde-set-alias)
(shu-cpp-project-set-alias)
(shu-misc-set-alias)
(when (file-readable-p "~/emacs/shu-bb-cpp.elc")
  (shu-bb-cpp-set-alias))
(shu-cpp-misc-set-alias)
(shu-keyring-set-alias)

(defalias 'sc 'shu-shift-region-of-text)
(defalias 'sli 'shu-shift-line)
(defalias 'quit 'shu-quit)

(if (eq system-type 'darwin)
    (progn
      (shu-set-author "Stewart L. Palmer (stewart@stewartpalmer.com)")
      (slp-set-comment-hooks))
  (shu-set-author "Stewart Palmer (spalmer62@bloomberg.net)")
  (shu-set-default-global-namespace "BloombergLP")
  (setq shu-cpp-use-bde-library t)
  (setq shu-cpp-include-user-brackets t)
  (slp-bb-set-comment-hooks))

;;
;; Change some of the s-mode defaults
;;
(shu-add-cpp-c-extensions  "f")
(shu-add-cpp-h-extensions  "inc")
(shu-add-cpp-base-types (list
              "bbint16_t"
              "bbint32_t"
              "bbint64_t"
              "bbuint16_t"
              "bbuint32_t"
              "bbuint64_t"))

;; Set the common prefix for member variables in a C++ class
(setq shu-cpp-member-prefix "d_")

(when (not (eq system-type 'darwin))
  (setq s-standard-indent 4)
  (setq s-undent-on-close nil))


;;(defadvice set-auto-mode (around s-mode-around-auto-mode activate)
;;  "Keep set-auto-mode from using file local variables to put c and c++ files into
;;a mode other than s-mode.  Normally, auto-mode-alist is used to choose the major
;;mode based on file extension.  But if a file starts with a local variable that looks
;;like // -*-C++-*-, then set-auto-mode ignores auto-mode-alist and sets the major
;;mode from the comment at the beginning of the file unless enable-local-variables has
;;been set to nil.  This piece of advice looks to see if the file extension is in the
;;list of those normally handled by s-mode.  If so, it runs set-auto-mode inside of a
;;let in which enable-local-variables is set to nil.  If the file extension is not in
;;the list, it runs set-auto-mode inside of a let in which enable-local-variables has
;;not been changed."
;;  (let (
;;    (extension (downcase (file-name-extension (buffer-file-name) t)))
;;    (enable-local-vars enable-local-variables)
;;        )
;;  (when (member extension s-mode-normal-extensions)
;;    (setq enable-local-vars nil))
;;  (let ((enable-local-variables enable-local-vars))
;;    ad-do-it)
;;))
;;

;; Define PF keys, etc.
(setup-terminal-keys)

(setq shu-base-jar-file-name "c:/apps/JDKs/8.4.2/src.jar")
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(global-set-key "" 'shu-move-down)
(global-set-key "" 'goto-line)

;(when (display-color-p)
;  (set-cursor-color 'steel))

;; Define the loction of the keyring file
(setq shu-keyring-file "~/projects/stuff/plist.ctx.gpg")

;; Clear the keyring index whenever the keyring file is saved
(add-hook 'after-save-hook 'shu-keyring-clear-index)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'find-file-hook 'slp-record-visited-file)

(setq shu-org-home "~/data/org")
(when (file-readable-p (concat shu-org-home "/home.org"))
  (require 'org-install)
  ;; Allow links from org files to Apple Mail messages
  (add-to-list 'org-modules 'org-mac-message)
  (add-to-list 'org-modules 'org-habit)
  (setq org-mac-mail-account "mail.stewartpalmer.com")
  ;; Alias to insert link to currently selected Apple Mail message
  (defalias 'imail 'org-mac-message-insert-selected)
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
)


(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(menu-bar-mode -1)

(setq explicit-shell-file-name "/bin/bash")

(fset 'rs 'replace-string)            ; Make rs a synonym for replace-string

(fset 'xyank 'shu-yank-x-primary)     ; Similar to middle mouse button

(setq mouse-yank-at-point t)  ; Mouse yank is at point instead of at click position

;; Set larger font when not on OS X
(when (not (eq system-type 'darwin))
  (set-face-attribute 'default nil :height 95)
  (set-face-attribute 'region nil :background "khaki"))

(when (not (eq system-type 'darwin))
  (setq x-gtk-use-system-tooltips nil))

;;
;;  This hook is invoked after each file is loaded.
;;  If the file is a .ctx file (Clear Text File)
;;  then backups of the file are inhibited.  This is
;;  because the file is normally encrypted and we
;;  do not wish to create extra clear text backups
;
;;(add-hook 'find-file-hooks
;;  (function (lambda ()
;;
;;  (let
;;     ((name (upcase (if buffer-file-name
;;                     (file-name-sans-versions buffer-file-name)
;;                   (buffer-name))))
;;      (ctx "\\.CTX"))
;;   (if (not (string-match ctx name))
;;     ()
;;     (make-local-variable 'backup-inhibited)
;;     (setq backup-inhibited t))
;;)
;;
;;)))


(defun shu-ignore-shell-process-on-shell-exit()
"Function to clear the query-on-exit flag for the shell.  This keeps emacs from prompting us
each time we exit the buffer that holds the shell process."
(delete-other-windows)
(set-process-query-on-exit-flag (get-process "shell") nil)
)

(add-hook 'shell-mode-hook 'shu-ignore-shell-process-on-shell-exit)


(when (< emacs-major-version 24)
;; Don't do this for emacs 24.  This breaks yank (20 July 2012)
;; Hence the above when clause.

;; === Fix the "copy-paste from MS Word" issue on Mac OS X ===
;; Allow pasting of text (instead of images) from Microsoft Office programs
;; prohibit pasting of TIFFs
;; This overrides the original definition of this function as found in
;; /Applications/Emacs.app/Contents/Resources/lisp/term/mac-win.el
;; See http://superuser.com/questions/68328/carbon-emacs-yank-results-in-images
(defun x-selection-value (type)
  (let ((data-types '(public.file-url
                       public.utf16-plain-text
                       com.apple.traditional-mac-plain-text))
    text)
    (while (and (null text) data-types)
      (setq text (condition-case nil
    	     (x-get-selection type (car data-types))
    	   (error nil)))
      (setq data-types (cdr data-types)))
    (if text
    (remove-text-properties 0 (length text) '(foreign-selection nil)
text))
    text))
)

(setq shu-git-path "C:/Program Files/Git/usr/bin/sh.exe")
(setq shu-git-windows-shell-file (concat shu-git-path "/sh.exe"))
(when (and (eq system-type 'windows-nt) (file-readable-p shu-git-windows-shell-file))
  (setq explicit-shell-file-name shu-git-windows-shell-file)
  ;; shell.el forms a variable name from the shell name.  In this case the shell
  ;; name sans-directory is sh.exe.  So the variable name is explicit-sh.exe-args.
  (setq explicit-sh.exe-args '("--login" "-i"))
  (add-to-list 'exec-path shu-git-path))

;; Projects enable short names
(setq shu-cpp-project-short-names t)

(put 'upcase-region 'disabled nil)
;; Enable show-paren-mode
(show-paren-mode)

(add-hook 'latex-mode-hook (lambda () (set-fill-column 84)))
(add-hook 'text-mode-hook (lambda () (set-fill-column 84)))

 (setq ls-lisp-use-insert-directory-program nil)
 (require 'ls-lisp)

(setq default-directory "~/")

(setq c-basic-offset 4)
(setq-default c-electric-flag nil)
(defun slp-disable-c-electric ()
  (c-toggle-electric-state -1)
  )
(add-hook 'c-initialization-hook 'slp-disable-c-electric)

;; Make a non-standard key binding.  We can put this in
;; c-mode-base-map because c-mode-map, c++-mode-map, and so on,
;; inherit from it.
;;(defun my-c-initialization-hook ()
;;  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
;;(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; offset customizations not in my-c-style
;; This will take precedence over any setting of the syntactic symbol
;; made by a style.
(setq c-offsets-alist '((member-init-intro . ++)))

;; Create my personal style.
(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (substatement      . +)
                                   (case-label        . +)
                                   (comment-intro     . 0)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "My C Programming Style")
(c-add-style "PERSONAL" my-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline, but not hungry-delete
  (c-toggle-auto-newline -1)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inextern-lang 0)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
