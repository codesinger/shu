;;; shu-cpp-token-new.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2015 Stewart L. Palmer
;;
;; Package: shu-cpp-token-new
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

;; This is experimental code for manipulating and using sections of C++
;; code in tokenized form.  It may or may not eventually become part of
;; the shu elisp package.

;;; Code:

(require 'shu-cpp-token)




;;
;;  jjj
;;
(defun jjj (start end)
  "For each unquoted token from a reverse scan, show the four tokens immediately
before and after."
  (interactive "r")
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (tlist)
        (token-info)
        (token-type)
        (last-token-info)
        (this)
        (prev)
        (nlist)
        (next)
        (ntoken-info)
        (olist)
        (plist)
        (pad "      ")
        (count)
        (limit 4)
        )
    (setq debug-on-error t)
    (setq token-list (shu-cpp-reverse-tokenize-region-for-command start end))
    (princ "tokenized\n\n" gb)
    (setq tlist token-list)
    (setq tlist (shu-cpp-token-first-non-comment tlist))
    (while tlist
      (setq token-info (car tlist))
      (push token-info olist)
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq this (shu-cpp-token-string-token-info token-info))
        (princ "\n\n" gb)
        ;;
        ;; The tokens in front are the next in the list
        ;; but in reverse order
        (setq nlist (shu-cpp-token-next-non-comment tlist))
        (setq count 0)
        (setq plist nil)
        (while (and nlist (< count limit))
          (setq count (1+ count))
          (setq ntoken-info (car nlist))
          (push ntoken-info plist)
          (setq nlist (shu-cpp-token-next-non-comment nlist))
          )
        (while plist
          (setq ntoken-info (car plist))
          (setq prev (shu-cpp-token-string-token-info ntoken-info))
          (princ (concat prev "\n") gb)
          (setq plist (cdr plist))
          )
        (princ (concat pad this "\n") gb)
        ;;
        ;; The tokens following are the first on olist
        (setq nlist (cdr olist))
        (setq count 0)
        (while (and nlist (< count limit))
          (setq count (1+ count))
          (setq ntoken-info (car nlist))
          (setq prev (shu-cpp-token-string-token-info ntoken-info))
          (princ (concat prev "\n") gb)
          (setq nlist (cdr nlist))
          )
        )
      (setq last-token-info token-info)
      (setq tlist (shu-cpp-token-next-non-comment tlist))
      )
    ))



;;
;;  ppp
;;
(defun ppp (start end)
  "Scan through a list of all non-comment tokens looking for unquoted tokens.
When an unquoted token is found, show it and the one immediately before and after.
Note that the very first one might be a comment because it is only at the
bottom of the loop that we invoke shu-cpp-token-next-non-comment."
  (interactive "r")
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (tlist)
        (token-info)
        (token-type)
        (last-token-info)
        (this)
        (prev)
        (nlist)
        (next)
        (ntoken-info)
        )
    (setq debug-on-error t)
    (setq token-list (shu-cpp-tokenize-region-for-command start end))
    (princ "tokenized\n\n" gb)
    (setq tlist token-list)
    (setq tlist (shu-cpp-token-first-non-comment tlist))
    (while tlist
      (setq token-info (car tlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq this (shu-cpp-token-string-token-info token-info))
        (if last-token-info
            (setq prev (shu-cpp-token-string-token-info last-token-info))
          (setq prev "")
          )
        (setq nlist (shu-cpp-token-next-non-comment tlist))
        (if (not nlist)
            (setq next "")
          (setq ntoken-info (car nlist))
          (setq next (shu-cpp-token-string-token-info ntoken-info))
          )
        (princ (format "prev: %s, this: %s, next: %s\n" prev this next) gb)
        )
      (setq last-token-info token-info)
      (setq tlist (shu-cpp-token-next-non-comment tlist))
      )
    ))



;;
;;  qqq
;;
(defun qqq (start end)
  "Doc string."
  (interactive "r")
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (tlist)
        (token-info)
        (token-type)
        (last-token-info)
        (this)
        (prev)
        (nlist)
        (next)
        (ntoken-info)
        )
    (setq debug-on-error t)
    (setq token-list (shu-cpp-reverse-tokenize-region-for-command start end))
    (princ "tokenized\n\n" gb)
    (setq tlist token-list)
    (setq tlist (shu-cpp-token-first-non-comment tlist))
    (while tlist
      (setq token-info (car tlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq this (shu-cpp-token-string-token-info token-info))
        (if last-token-info
            (setq next (shu-cpp-token-string-token-info last-token-info))
          (setq next "")
          )
        (setq nlist (shu-cpp-token-next-non-comment tlist))
        (if (not nlist)
            (setq prev "")
          (setq ntoken-info (car nlist))
          (setq prev (shu-cpp-token-string-token-info ntoken-info))
          )
        (princ (format "prev: %s, this: %s, next: %s\n" prev this next) gb)
        )
      (setq last-token-info token-info)
      (setq tlist (shu-cpp-token-next-non-comment tlist))
      )
    ))



;;
;;  zzz
;;
(defun zzz (start end)
  "Doc string."
  (interactive "r")
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (tlist)
        (token-info)
        (token-type)
        (last-token-info)
        (this)
        (prev)
        (nlist)
        (next)
        (count 0)
        (ntoken-info)
        )
    (setq debug-on-error t)
    (setq token-list (shu-cpp-reverse-tokenize-region-for-command start end))
    (princ "tokenized\n\n" gb)
    (setq tlist token-list)
    (setq tlist (shu-cpp-token-first-non-comment tlist))
    (while tlist
      (setq count (1+ count))
      (setq token-info (car tlist))
      (setq this (shu-cpp-token-string-token-info token-info))
      (princ (format "%d: %s\n" count this) gb)
      (setq tlist (shu-cpp-token-next-non-comment tlist))
      )
    ))




;;; shu-cpp-token-new.el ends here
