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
;;  shu-cpp-token-first-non-comment
;;
(defun shu-cpp-token-first-non-comment (tlist)
  "TLIST points to a list of token-info.  Return TLIST pointing to the next
token-info that does not hold a comment.  If you are scanning through a list
of tokens, it is not uncommon to want to skip all of the comments.  Use this
at the bottom of the loop in place of the usual \"setq tlist (cdr tlist))\".

i.e.,

     (while tlist
        ...
       (setq tlist (cdr tlist)))

becomes

     (setq tlist (shu-cpp-token-first-non-comment tlist))
     (while tlist
        ...
       (setq tlist (shu-cpp-token-first-non-comment tlist)))

and you will scan through the liwt without seeing any comments."
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-info)
        (in-comment)
        )
    (when tlist
      (princ "\n\n1:\n" gb) (princ tlist gb) (princ "/n/n" gb)
      (setq token-info (car tlist))
      (princ (concat (shu-cpp-token-string-token-info token-info) "\n") gb)
      (setq in-comment (shu-cpp-token-is-comment token-info))
      (princ "2\n" gb)
      (while (and in-comment tlist)
      (princ "3\n" gb)
        (setq tlist (cdr tlist))
        (setq token-info (car tlist))
        (princ (concat (shu-cpp-token-string-token-info token-info) "\n") gb)
        (setq in-comment (shu-cpp-token-is-comment token-info))
        )
      )
      (princ "\n\n2:\n" gb) (princ tlist gb) (princ "/n/n" gb)
    tlist
    ))




;;
;;  shu-test-shu-cpp-token-first-non-comment-1
;;
(ert-deftest shu-test-shu-cpp-token-first-non-comment-1 ()
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (token-info)
        (token-type)
        (tlist)
        (count 0)
        (ncount 0)
        (comment-count 0)
        (limit)
        (this)
        (first-non-comment-token-info)
        (data
         (concat
          "    // This is a comment\n"
          "  /* and yet another comment */\n"
          "    x =\"This is a fine kettle of fish is it not?\" // Again\n"
          "    int  j; /* again */\n"
          "    j++;\n")))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      )
    (setq tlist token-list)
    (while (and tlist (not first-non-comment-token-info))
      (setq count (1+ count))
      (setq token-info (car tlist))
      (when (not (shu-cpp-token-is-comment token-info))
        (setq first-non-comment-token-info token-info)
      (princ (concat "XX: " (shu-cpp-token-string-token-info first-non-comment-token-info) "\n") gb)
        )
      (setq tlist (cdr tlist))
      )
    (setq tlist token-list)
    (setq tlist (shu-cpp-token-first-non-comment tlist))
    (princ "\n\n3:\n" gb) (princ tlist gb) (princ "/n/n" gb)
    (should tlist)
    (should (consp tlist))
    (setq token-info (car tlist))
      (princ (concat "ZZ: " (shu-cpp-token-string-token-info token-info) "\n") gb)
    (should (equal first-non-comment-token-info token-info))
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




;;; shu-cpp-token-new.el ends here
