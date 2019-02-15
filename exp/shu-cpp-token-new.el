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
;;  shu-cpp-token-string-token-info
;;
(defun shu-cpp-token-string-token-info (token-info)
  "Doc string."
  (interactive)
  (let (
        (token)
        (token-type)
        (token-type-name)
        (spoint)
        (epoint)
        (error-message)
        (emsg "")
        (name)
        )
    (if (not token-info)
        (setq name "**none**")
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (setq token-type-name (shu-cpp-token-token-type-name token-type))
      (when error-message
        (setq emsg (concat " (" error-message ")")))
      (setq name (format "(%s) \"%s\" %d  %s" token-type-name token spoint emsg)))
    name
    ))






;;
;;  zzz
;;
(defun zzz (start end)
  "Doc string."
  (interactive "r")
  (let (
        (token-list)
        (tlist)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message)
        )
    (setq debug-on-error t)
    (setq token-list (shu-cpp-reverse-tokenize-region-for-command start end))
    (setq tlist token-list)
    (while tlist
      (setq token-info (car tlist))
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (when (and (= token-type shu-cpp-token-type-op)
                 (stringp token)
                 (string= token ";"))
        (setq tlist (sub-zzz tlist))
        )
      (setq tlist (cdr tlist))
      )

    ))



;;
;;  rrr
;;
(defun rrr (start end)
  "Doc string."
  (interactive "r")
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (tlist)
        (token-info)
        (last-token-info)
        (token)
        (token-type)
        (token-type-name)
        (spoint)
        (epoint)
        (error-message)
        (prev)
        (next)
        (this)
        (ntoken-info)
        (nlist)
        (count 0)
        )
    (setq debug-on-error t)
    (setq token-list (shu-cpp-reverse-tokenize-region-for-command start end))
    (princ "tokenized\n\n" gb)
    (setq tlist token-list)
    (while tlist
      (setq token-info (car tlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (setq token-type-name (shu-cpp-token-token-type-name token-type))
      (princ (format "type: %d %s %s %d\n" token-type token-type-name token spoint) gb)
      (when (= token-type shu-cpp-token-type-uq)
        (setq prev (shu-cpp-token-string-token-info last-token-info))
        (setq next "")
;;;        (setq nlist (shu-cpp-token-next-non-comment tlist))
        (setq nlist (cdr tlist))
        (when nlist
          (setq ntoken-info (car nlist))
          (setq next (shu-cpp-token-string-token-info ntoken-info))
          )
        (setq this (shu-cpp-token-string-token-info ntoken-info))
        (princ (format "%s => %s => %s\n" prev this next) gb)

        )
      (setq last-token-info token-info)
      (setq tlist (cdr tlist))
;;;      (setq tlist (shu-cpp-token-next-non-comment tlist))
      )


    ))



;;
;;  sub-zzz
;;
(defun sub-zzz (tlist)
  "Enter with the current token being a \";\""
  (let (
        (gb (get-buffer-create "**boo**"))
        (old-tlist tlist)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message)
        (done)
        (is-commet)
        )


    (while (not done)
      (setq token-info (car tlist))
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (when (= token-type shu-cpp-token-type-uq)
        (princ (format "%d: %s\n" (shu-the-line-at spoint) token) gb)
        (setq done t)
        )
      (setq old-tlist tlist)
      (setq tlist (cdr tlist))
      )
     tlist

    ))



;;
;;  skip-comments
;;
(defsubst skip-comments (tlist)
  "Return tlist pointing to next non-comment."
  (interactive)
  (let (
        (token-info)
        (token-type)
        (in-comment t)
        )
    (while in-comment
      (setq token-info (car tlist))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (if (= token-type shu-cpp-token-type-ct)
          (setq tlist (cdr tlist))
        (setq in-comment nil)
        )
      )
    tlist
    ))



;;; shu-cpp-token-new.el ends here
