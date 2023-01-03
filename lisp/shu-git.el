;;; shu-git.el --- Shu functions for interacting with git
;;
;; Copyright (C) 2023 Stewart L. Palmer
;;
;; Package: shu-base
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
;;

;;; Commentary:

;;
;; A collection of functions for interacting with git

;;; Code:





;;
;;  shu-git-number-commits
;;
(defun shu-git-number-commits ()
  "In a git log buffer, number all of the commits with zero being the most
recent.

It is possible to refer to commits by their SHA-1 hash.  If you want to see the
difference between two commits you can ask git to show you the difference by
specifying the commit hash of each one.  But this is cumbersome.  It involves
copying and pasting two SHA-1 hashes.  Once the commits are numbered, then
SHU-GIT-DIFF-COMMITS may be used to diff two commits by number.  See the
documentation for SHU-GIT-DIFF-COMMITS for further information.

This function counts as a commit any instance of \"commit\" that starts at the
beginning of a line and is followed by some white space and a forty character
hexadecimal number.  Returns the count of the number of commits found."
  (interactive)
  (let ((ss "^\\(commit\\)\\s-+[0-9a-f]\\{40\\}")
        (count 0)
        (cn))
    (goto-char (point-min))
    (while (re-search-forward ss nil t)
      (setq cn (shu-format-num count 6))
      (replace-match (concat cn ". " (match-string 0)))
      (setq count (1+ count)))
    (goto-char (point-min))
    (message "Numnbered %d commits" count)
    count
    ))




;;
;;  shu-find-numbered-commit
;;
(defun shu-find-numbered-commit (commit-number)
  "Search through a numbered git commit log looking for the commit whose number is
COMMIT-NUMBER.  Return the SHA-1 hash of the commit if the commit number is found.
Return nil if no commit with the given number is found.
The commit log is assume to have been numbered by shu-git-number-commits."
  (let ((ss-1 "\\s-*")
        (ss-2 "\\.\\s-*commit\\s-+\\([0-9a-f]\\{40\\}\\)")
        (sss)
        (commit-hash))
    (save-excursion
      (goto-char (point-min))
      (setq sss (concat ss-1 (number-to-string commit-number) ss-2))
      (when (re-search-forward sss nil t)
        (setq commit-hash (match-string 1))))
    commit-hash
    ))



;;
;;  shu-git-diff-commits
;;
(defun shu-git-diff-commits (commit-range)
  "In a buffer that is a numbered git log, query for a range string, find the two
commits, and put into the kill ring a git diff command specifying the two commits.

For example, given the following two numbered commits:

    31. commit 38f25b6769385dbc3526f32a75b97218cb4a6754
    33. commit 052ee7f4297206f08d44466934f1a52678da6ec9

if the commit range specified is either \"31.33\" or \"31+2\", then the following
is put into the kill ring:

    \"git diff -b 38f25b6769385dbc3526f32a75b97218cb4a6754..052ee7f4297206f08d44466934f1a52678da6ec9 \""
  (interactive "sCommits in the form x.y or x+y or x-y?: ")
  (let ((range)
        (start)
        (end)
        (commit-1)
        (commit-2)
        (scommit-1)
        (scommit-2)
        (diff))
    (setq range (shu-split-range-string commit-range))
    (setq start (car range))
    (setq end (cdr range))
    (if (or (not start)
            (not end))
        (progn
          (message "%s" "Invalid range specified")
          (ding))
      (setq commit-1 (shu-find-numbered-commit start))
      (setq commit-2 (shu-find-numbered-commit end))
      (setq scommit-1 (shu-git-find-short-hash commit-1))
      (when scommit-1
        (setq commit-1 scommit-1))
      (setq scommit-2 (shu-git-find-short-hash commit-2))
      (when scommit-2
        (setq commit-2 scommit-2))
      (setq diff (concat "git diff -b " commit-1 ".." commit-2 " "))
      (shu-kill-new diff))
    ))



;;
;;  shu-git-find-short-hash
;;
(defun shu-git-find-short-hash (hash)
  "Return the git short hash for the HASH supplied as an argument.  Return nil
if the given HASH is not a valid git revision."
  (let ((short-hash)
        (ss "^[a-fA-F0-9]\\{2,40\\}"))
    (with-temp-buffer
      (call-process "git" nil (current-buffer) nil "rev-parse" "--short" hash)
      (goto-char (point-min))
      (when (re-search-forward ss nil t)
        (setq short-hash (match-string 0))))
    short-hash
    ))



;;
;;  shu-git-add-file
;;
(defun shu-git-add-file (filename)
  "Do a \"git add\" for FILENAME.  Return empty string if add succeeds.  Otherwise,
return git error message."
  (let ((result))
    (with-temp-buffer
      (call-process "git" nil (current-buffer) nil "add" filename)
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    (setq result (shu-trim-trailing result))
    (when (not (string= result ""))
      (setq result (concat "After git add " filename ":\n" result)))
    result
    ))




;;
;;  shu-git-find-branch
;;
(defun shu-git-find-branch ()
  "Return the name of the current branch in a git repository."
  (let ((branch))
    (with-temp-buffer
      (call-process "git" nil (current-buffer) nil "rev-parse" "--abbrev-ref" "HEAD")
      (setq branch (buffer-substring-no-properties (point-min) (1- (point-max)))))
    branch
    ))




;;
;;  shu-git-find-default-branch
;;
(defun shu-git-find-default-branch ()
  "Return the name of the default branch in a git repository.  The default
branch is the one branch that is created with a new repository."
  (let ((branch)
        (fset (length "origin/")))
    (with-temp-buffer
      (call-process "git" nil (current-buffer) nil "rev-parse" "--abbrev-ref" "origin/HEAD")
      (setq branch (buffer-substring-no-properties (+ (point-min) fset) (1- (point-max)))))
    branch
    ))



;;
;;  shu-git-get-pr-url
;;
(defun shu-git-get-pr-url ()
  "Put into the kill ring the path required to create a new pull request for
the current branch of the current repository."
  (interactive)
  (let ((url-repo (shu-get-url-repo))
        (branch (shu-git-find-branch))
        (url))
    (setq url (concat url-repo "/pull/new/" branch))
    (shu-kill-new url)
    ))




;;
;;  shu-git-show-branch
;;
(defun shu-git-show-branch ()
  "Display the name of the current branch in a git repository."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (message "%s" branch)
    ))




;;
;;  shu-git-copy-branch
;;
(defun shu-git-copy-branch ()
  "Put the name of the current branch in a git repository into the kill ring."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (shu-kill-new branch)
    (message "%s" branch)
    ))




;;
;;  shu-git-insert-branch
;;
(defun shu-git-insert-branch ()
  "Insert at point the name of the current branch in a git repository"
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (insert branch)
    ))




;;
;;  shu-git-insert-origin-branch
;;
(defun shu-git-insert-origin-branch ()
  "Insert at point the name of the current branch in a git repository preceded by the
word \"origin\"..  This can be used as part of git push or pull."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (insert (concat "origin " branch))
    ))




;;
;;  shu-git-insert-push-origin-branch
;;
(defun shu-git-insert-push-origin-branch ()
  "Insert at point the git command to push the current branch out to origin.  If
the current branch is the default branch (fka \"master\"), you are prompted to
see if you want to proceed.  This is to prevent an accidental push to the
default branch."
  (interactive)
  (let ((branch (shu-git-find-branch))
        (insert-push t)
        (default-branch (shu-git-find-default-branch)))
    (when (string= branch default-branch)
      (setq insert-push (yes-or-no-p (concat "Really push to " branch " branch? "))))
    (when insert-push
      (insert (concat "git push origin " branch)))
    ))




;;
;;  shu-git-insert-pull-origin-branch
;;
(defun shu-git-insert-pull-origin-branch ()
  "Insert at point the name the git command to pull the current branch from
origin."
  (interactive)
  (let ((branch (shu-git-find-branch)))
    (insert (concat "git pull origin " branch))
    ))




;;
;;  shu-git-insert-checkout-default
;;
(defun shu-git-insert-checkout-default ()
  "Insert at point the git command to check out the current default branch."
  (interactive)
  (let ((default-branch (shu-git-find-default-branch)))
      (insert (concat "git checkout " default-branch))
    ))




;;
;;  shu-git-insert-git-commit
;;
(defun shu-git-insert-git-commit ()
  "Insert at point the name the git command to commit with the commentary held
in a file called \"why.txt\"."
  (interactive)
  (insert "git commit -F why.txt")
  )



;;
;;  shu-git-is-file-in-git
;;
(defun shu-git-is-file-in-git (filename)
  "Return t if FILENAME is in a git repository and the file is registered with git."
  (let ((in-git)
        (rc))
    (with-temp-buffer
      (setq rc (call-process "git" nil (current-buffer) nil "ls-files" filename "--error-unmatch")))
    (= rc 0)
    ))



;;
;;  shu-git-are-files-in-git
;;
(defun shu-git-are-files-in-git (files)
  "Given a list of file names in FILES, return a cons cell whose CAR holds the
number of files that are under git control and whose CDR holds the number of
files that are not under git control."
  (let ((ff files)
        (file)
        (in-git)
        (in-git-count 0)
        (not-git-count 0))
    (while ff
      (setq file (car ff))
      (setq in-git (shu-git-is-file-in-git file))
      (if in-git
          (setq in-git-count (1+ in-git-count))
        (setq not-git-count (1+ not-git-count)))
      (setq ff (cdr ff)))
    (cons in-git-count not-git-count)
    ))



;;
;;  shu-git-move-file
;;
(defun shu-git-move-file (old-file new-file)
  "Issue \"git mv old-file new-file\".

Return a cons cell whose CAR is t if the move succeeded and whose CDR is the
output of the git move command."
  (let ((rc)
        (result))
    (with-temp-buffer
      (setq rc (call-process "git" nil (current-buffer) nil "mv" old-file new-file))
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    (cons (= rc 0) result)
    ))




;;
;;  shu-git-set-alias
;;
(defun shu-git-set-alias ()
  "Set the common alias names for the functions in shu-git.
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'number-commits 'shu-git-number-commits)
  (defalias 'diff-commits 'shu-git-diff-commits)
  (defalias 'get-pr-url 'shu-git-get-pr-url)
  (defalias 'show-branch 'shu-git-show-branch)
  (defalias 'copy-branch 'shu-git-copy-branch)
  (defalias 'insb 'shu-git-insert-branch)
  (defalias 'inso 'shu-git-insert-origin-branch)
  (defalias 'gco 'shu-git-insert-checkout-default)
  (defalias 'gpl 'shu-git-insert-pull-origin-branch)
  (defalias 'gps 'shu-git-insert-push-origin-branch)
  (defalias 'gcm 'shu-git-insert-git-commit)
  )



(provide 'shu-git)

;;; shu-git.el ends here
