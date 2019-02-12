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

(require 'shu-cpp-token-new)



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
(defun skip-comments (tlist)
  "Return tlist pointing to next non-comment."
  (interactive)
  (let (
        (old-tlist tlist)
        (token-info)
        (token)
        (token-type)
        (spoint)
        (epoint)
        (error-message)
        (in-commet t)
        )
    (while in-comment
      (setq token-info (car tlist))
      (shu-cpp-token-extract-info token-info token token-type spoint epoint error-message)
      (if (= (token-type shu-cpp-token-type-ct))
          (setq tlist (cdr tlist))
        (setq in-comment nil)
        )
      )
    tlist
    ))



;;; shu-cpp-token-new.el ends here
