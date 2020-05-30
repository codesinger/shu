;;; .emacs - My personal .emacs file
;;
;; Copyright (C) 2015 Stewart L. Palmer
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

;; This is my personal .emacs file.
;;
;; It is not part of the shu package




;;
;;  shu-show-load-path
;;
(defun shu-show-load-path (ident)
  "Doc string."
  (let (
        (lp load-path)
        (path)
        (gb (get-buffer-create "**shu-load-path**"))
        )
    (princ (concat "\n\n" ident ":\n") gb)
    (princ load-path gb)
    (princ "\n" gb)
    ))

(shu-show-load-path "INITIAL")

;;
;;  shu-system-type-is-unix
;;
(defun shu-system-type-is-unix ()
  "Return true if we are running on some sort of Unix operating system."
  (let ((type
         (if (or
              (eq system-type 'aix)
              (eq system-type 'berkeley-unix)
              (eq system-type 'gnu/kfreebsd)
              (eq system-type 'gnu/linux)
              (eq system-type 'hpux)
              (eq system-type 'usg-unix-v))
             t
           nil)))
    type
    ))


;;
;;  shu-system-type-is-mac-osx
;;
(defun shu-system-type-is-mac-osx ()
  "Return true if we are running on a Max OSX operating system."
  (let ((type
         (if (eq system-type 'darwin)
             t
           nil)))
    type
    ))


;;
;;  shu-system-type-is-windows
;;
(defun shu-system-type-is-windows ()
  "Return true if we are running on a Windows operating system."
  (let ((type
         (if (eq system-type 'windows-nt)
             t
           nil)))
    type
    ))

(setq shu-home (getenv "HOME"))
  (setq load-path
        (append (list (concat shu-home "/emacs"))
          (append load-path))
  )

(shu-show-load-path "AFTER-HOME")


(if (shu-system-type-is-mac-osx)
    (progn
      (setq slp-org-location "~/emacs/org-7.8.09")
      (when (file-readable-p (concat slp-org-location "/lisp/org-install.el"))
        (setq load-path (cons (concat slp-org-location "/lisp/") load-path))
        (setq load-path (cons (concat slp-org-location "/contrib/lisp/") load-path))
        (load-file (concat slp-org-location "/lisp/org.elc"))
        (load-file (concat slp-org-location "/lisp/org-agenda.elc"))
        )
      )
  (require 'org)
  )

(shu-show-load-path "AFTER-ORG")


(defvar shu-org-home nil
  "Home directory of the org data files.")

(defvar shu-org-home-file nil
  "Main aganda org data file.")


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
;;(setq default-frame-alist '((fullscreen . fullheight) (tool-bar-lines . 0) (menu-bar-lines . 0) (width . 115)))

;;
;;  shu-set-frame-size-full-height
;;
(defun shu-set-frame-size-full-height ()
  "Set to full screen height"
  (let* ((lost-lines (if (not (shu-system-type-is-windows)) 2 5))
         (hpx (- (x-display-pixel-height) (* lost-lines (frame-char-height))))
         (hpl (/ hpx (frame-char-height))))
    (when (display-graphic-p)
      (setq default-frame-alist '((tool-bar-lines . 0) (menu-bar-lines . 0) (width . 115)))
      (add-to-list 'default-frame-alist (cons 'height hpl)))
    ))
(shu-set-frame-size-full-height)

;(set-frame-parameter nil 'width '115)
;;
;;'(fullscreen . fullheight))
;
(setq shell-pushd-regexp "pu")
(setq shell-popd-regexp "po")
;
; Set ispell
(if (shu-system-type-is-mac-osx)
  (setq ispell-program-name "/opt/local/bin/ispell")   ;; For Mac OS X
  (setq ispell-program-name "/opt/swt/bin/aspell"))    ;; For all others


(put 'eval-expression 'disabled nil)
(load-library "cc-mode")
(load-file "~/emacs/shu-base.elc")
(shu-load-library-files "~/emacs")
;;(load-file "~/emacs/s-mode.elc")
(when (file-readable-p "~/emacs/useful.elc")
  (load-file "~/emacs/useful.elc"))
(when (file-readable-p "~/emacs/shu-attributes.elc")
  (load-file "~/emacs/shu-attributes.elc"))
(load-file "~/emacs/macros.elc")
;;(load-file "~/emacs/clearcase.elc");;Incompatible with Emacs 24
;;(load-file "~/emacs/visible.elc")
;;(load-file "~/emacs/slp-java-base.elc")
(when (file-readable-p "~/emacs/slpbb.elc")
  (load-file "~/emacs/slpbb.elc"))
(when (file-readable-p "~/emacs/rmv-using.elc")
  (load-file "~/emacs/rmv-using.elc"))
(load-file "~/emacs/shu-org-extensions.elc")
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
(shu-cpp-misc-set-alias)
(shu-keyring-set-alias)
(shu-match-set-alias)
(shu-org-extensions-set-alias)

(defalias 'sc 'shu-shift-region-of-text)
(defalias 'sli 'shu-shift-line)
(defalias 'quit 'shu-quit)

(if (shu-system-type-is-mac-osx)
    (progn
      (shu-set-author "Stewart L. Palmer (stewart@stewartpalmer.com)")
      (slp-set-comment-hooks))
  (shu-set-author "Stewart Palmer (spalmer62@bloomberg.net)")
  (shu-set-default-global-namespace "BloombergLP")
  (setq shu-cpp-use-bde-library t)
  (setq shu-cpp-include-user-brackets t)
  (setq shu-cpp-std-namespace "bsl")
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

(when (not (shu-system-type-is-mac-osx))
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
(add-hook 'before-save-hook 'shu-trim-file-hook)
(add-hook 'find-file-hook 'slp-record-visited-file)

(defvar shu-org-mode-is-set nil
  "set to true if org mode has been set up")

(when (file-readable-p "~/emacs/shu-setup-org-mode.elc")
  (load-file "~/emacs/shu-setup-org-mode.elc")
  (when (fboundp 'shu-setup-org-mode)
    (shu-setup-org-mode)))

(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(menu-bar-mode -1)

(setq explicit-shell-file-name "/bin/bash")

(fset 'rs 'replace-string)            ; Make rs a synonym for replace-string

(fset 'xyank 'shu-yank-x-primary)     ; Similar to middle mouse button

(setq mouse-yank-at-point t)  ; Mouse yank is at point instead of at click position

;; Set larger font when not on OS X
(when (not (shu-system-type-is-mac-osx))
  (set-face-attribute 'default nil :height 95)
  (set-face-attribute 'region nil :background "khaki"))

(when (not (shu-system-type-is-mac-osx))
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

(setq shu-git-path "C:/Program Files/Git/usr/bin")
(setq shu-git-windows-shell-file (concat shu-git-path "/sh.exe"))
(when (and (shu-system-type-is-windows) (file-readable-p shu-git-windows-shell-file))
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
