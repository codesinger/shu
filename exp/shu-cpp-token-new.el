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
;;  match-list:
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> Next in list
;;        |
;;        +-------------> match-info
;;
;;
;;
;;
;;  match-info
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-ext
;;        |
;;        +-------------> op-code
;;
;;
;;
;;
;;  match-ext
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-type
;;        |
;;        +-------------> match-func
;;
;;
;;
;;
;;  match-func
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-eval-func
;;        |
;;        +-------------> match-ret-ind
;;
;;
;;
;;
;;  match-type
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> match-token-type
;;        |
;;        +-------------> match-token-value
;;
;;
;;
;; op-code saya what the matching operation is
;; match-eval-func - If non-nil, call this function to do tbe evaluation
;; match-ret-ind - If true, return this token-info as the return value
;;                 iff the evaluation succeeds
;; match-token-type - Is a token type to compare against the token-type from
;;                    the token-info
;; match-token-value - Is a possible value or regular expresson to be compared
;;                     with or applied to the token-value from the token-info
;;
;;



;;
;;  shu-cpp-match-extract-info
;;
(defmacro shu-cpp-match-extract-info (match-info op-code match-type match-eval-func
                                      match-ret-ind match-token-type match-token-value)
  "Extract the information out of a match-info"
  (let (
        (tmatch-ext (make-symbol "match-ext"))
        (tmatch-func (make-symbol "match-func"))
        (tmatch-type (make-symbol "match-type"))
        )
    `(let (
           (,tmatch-ext)
           (,tmatch-func)
           (,tmatch-type)
           )
       (setq ,op-code (car ,match-info))
       (setq ,tmatch-ext (cdr ,tmatch-info))
       (setq ,tmatch-func (car ,tmatch-ext))
       (setq ,tmatch-type (cdr ,tmatch-ext))
       (setq ,match-eval-func (cdr ,tmatch-func))
       (setq ,match-ret-ind (car ,tmatch-func))
       (setq ,match-token-type (cdr ,tmatch-type))
       (setq ,match-token-value (car ,tmatch-type))
       )
    ))


;;
;;  shu-cpp-match-extract-type
;;
(defsubst shu-cpp-match-extract-type (match-info)
  "Doc string."
  (interactive)
  (let (
        (match-ext)
        )
    (setq match-ext (cdr match-info))
    (cddr match-ext)
    ))


;;
;;  shu-cpp-match-extract-token
;;
(defsubst shu-cpp-match-extract-token (match-info)
  "Doc string."
  (interactive)
  (let (
        (match-ext)
        )
    (setq match-ext (cdr match-info))
    (cadr match-ext)
    ))




(defconst shu-cpp-token-match-type-skip-1 1
  "The match type constant that indicates skip one input cell.")


(defconst shu-cpp-token-match-type-same 2
  "The match type constant that indicates that the token type and token value
must both match.")


;;
;;  shu-cpp-token-match-skip-1
;;
(defun shu-cpp-token-match-skip-1 (tlist)
  "Skip one cell in the input list."
  (if tlist
      (cdr tlist)
    tlist)
  )


;;
;;  shu-cpp-token-match-same
;;
(defun shu-cpp-token-match-same (match-info token-info)
  "Doc string."
  (interactive)
  (let (
        (match-token-type (shu-cpp-match-extract-type match-info))
        (match-token (shu-cpp-match-extract-token match-info))
        (token-type (shu-cpp-token-extract-type token-info))
        (token (shu-cpp-token-extract-token))
        )
    (and (= token-type match-token-type)
         (string= token match-token))
    ))



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
        (pad "        ")
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
;;  kkk
;;
(defun kkk (start end)
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
        (pad "        ")
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
        (while (and nlist (< count limit))
          (setq count (1+ count))
          (setq ntoken-info (car nlist))
          (setq prev (shu-cpp-token-string-token-info ntoken-info))
          (princ (concat prev "\n") gb)
          (setq nlist (shu-cpp-token-next-non-comment nlist))
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
;;  www
;;
(defun www (start end)
  "For each ; operator from a reverse scan, show the six tokens immediately
before."
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
        (token)
        (olist)
        (plist)
        (pad "        ")
        (count)
        (limit 6)
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
      (setq token (shu-cpp-token-extract-token token-info))
      (when (and (= token-type shu-cpp-token-type-op)
                 (string= token ";"))
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



;;
;;  yyy
;;
(defun yyy (start end)
  "Look for using namespace xxx"
  (interactive "r")
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (tlist)
        (token)
        (token-info)
        (token-type)
        (nlist)
        (nsname)
        (nslist)
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
        (setq token (shu-cpp-token-extract-token token-info))
        (if (not (string= token "namespace"))
            (setq nsname token)
          (setq nlist (shu-cpp-token-next-non-comment tlist))
          (when nlist
            (setq token-info (car nlist))
            (setq token-type (shu-cpp-token-extract-type token-info))
            (setq token (shu-cpp-token-extract-token token-info))
            (when (and (= token-type shu-cpp-token-type-uq)
                       (string= token "using"))
              (when nsname
                (push nsname nslist)
                (setq nsname nil)
                )
              )
            )
          )
        ;;      (setq this (shu-cpp-token-string-token-info token-info))

        )
        (setq tlist (shu-cpp-token-next-non-comment tlist))
      )
    (princ nslist gb)
    ))




;;
;;  mmm
;;
(defun mmm (start end)
  "Look for using namespace xxx"
  (interactive "r")
  (let (
        (gb (get-buffer-create "**boo**"))
        (token-list)
        (tlist)
        (olist)
        (token)
        (token-info)
        (token-type)
        (nlist)
        (nsname)
        (nslist)
        )
    (setq debug-on-error t)
    (setq token-list (shu-cpp-reverse-tokenize-region-for-command start end))
    (princ "tokenized\n\n" gb)
    (setq tlist token-list)
    (setq tlist (shu-cpp-token-first-non-comment tlist))
    (while tlist
      (setq token-info (car tlist))
;;      (token-info olist)
      (setq token-type (shu-cpp-token-extract-type token-info))
      (when (= token-type shu-cpp-token-type-uq)
        (setq token (shu-cpp-token-extract-token token-info))
        (when (string= token "using")
          (setq nlist olist)
          (when nlist
            (setq nlist (cdr nlist))
            (when nlist
              (setq ntoken-info (car nlist))
              (setq token-type
              )
            )
          )
        )
        )
      )
    ))



;;; shu-cpp-token-new.el ends here
