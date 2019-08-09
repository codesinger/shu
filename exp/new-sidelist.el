;;; new-sidelist.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-cpp-match
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

;; Experimental code for dealing with side-lists of match-lists
;;

;;; Code:

(provide 'shu-cpp-match)
(require 'shu-cpp-token)


;;
;;
;; If op-code indicates that this is a side list, then we have the structure
;; shown below, with side-list pointing to the first match-info in the side
;; list.
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
;;        |       +-----> match-side
;;        |
;;        +-------------> op-code
;;
;;
;;
;;  match-side
;;
;;   -------------------
;;   |        |        |
;;   |    o   |   o    |
;;   |    |   |   |    |
;;   -----|-------|-----
;;        |       |
;;        |       +-----> side-list
;;        |
;;        +-------------> side-parameter
;;
;;
;;  side-parameter is whatever the side list requires
;;





;;
;;  shu-cpp-match-repeat-sub-list
;;
(defun shu-cpp-match-repeat-sub-list (rlist token-list match-list)
  "Go through one iteration of the repeating list.  The iteration is considered
a success if either of the following are true: 1. The first match fails, or
2. All matches succeed.  If all matches succeed, the updated RLIST and
TOKEN-LIST are returned.  If the first match fails, the RLIST and TOKEN-LIST are
returned unaltered.  It is as though no match was ever attempted.  If some match
other than the first fails, nil is returned."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (looking t)
        (ret-val (cons token-list rlist))
        (orig-rlist rlist)
        (orig-token-list token-list)
        (mcount 0)
        (match-info)
        (token-info)
        (op-code)
        (match-eval-func)
        (match-ret-ind)
        (match-token-type)
        (match-token-value)
        (did-match)
        )
    (while looking
      (setq mcount (1+ mcount))
      (setq token-info (car token-list))
      (setq match-info (car match-list))
      (shu-cpp-match-extract-info match-info op-code match-eval-func
                                  match-ret-ind match-token-type match-token-value)
      (princ "token-info: " gb)(princ token-info gb)(princ "\n" gb)
      (princ "match-info: " gb)(princ match-info gb)(princ "\n" gb)
      (princ "match-eval-func: " gb)(princ match-eval-func gb)(princ "\n" gb)
      (setq did-match (funcall match-eval-func match-info token-info))
      (princ "did-match: " gb)(princ did-match gb)(princ "\n" gb)
      (if (not did-match)
          (progn
            (princ "not branch\n" gb)
            (setq looking nil)
            (when (/= mcount 1)
              (setq ret-val nil)
              )
            )
        (princ "yes branch\n" gb)
        (when match-ret-ind
          (push token-info rlist)
          (princ "rlist: " gb)(princ rlist gb)(princ "\n" gb)
          )
        (setq token-list (shu-cpp-token-next-non-comment token-list))
        (setq match-list (cdr match-list))
        (princ "token-list: " gb)(princ token-list gb)(princ "\n" gb)
        (princ "match-list: " gb)(princ match-list gb)(princ "\n" gb)
        (if match-list
            (progn
              (princ "have match-list\n" gb)
              (when (not token-list)
                (setq ret-val nil)
                (setq looking nil)
                )
              )
          (setq ret-val (cons token-list rlist))
          (setq looking nil)
          )
        )
      )
    ret-val
    ))





;;
;;  shu-test-shu-cpp-match-repeat-sub-list-1
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-1 ()
  "Whole list matches, updated rlist and tlist returned."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " :: bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")
          )
         )
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (shu-cpp-token-show-token-info new-rlist "NEW_RLIST 8889")
    (shu-cpp-token-show-token-info new-token-list "NEW_TOKEN-LIST 8889")
    (princ "new-rlist: " gb)(princ new-rlist gb)(princ "\n" gb)
    (princ "new-token-list: " gb)(princ new-token-list gb)(princ "\n" gb)

))





;;
;;  shu-test-shu-cpp-match-repeat-sub-list-2
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-2 ()
  "First item does not match.  Original and rlist returned."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " ; bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")
          )
         )
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list))
    (should ret-val)
    (should (consp ret-val))
    (setq new-token-list (car ret-val))
    (setq new-rlist (cdr ret-val))
    (shu-cpp-token-show-token-info new-rlist "NEW_RLIST 8889")
    (shu-cpp-token-show-token-info new-token-list "NEW_TOKEN-LIST 8889")
    (princ "new-rlist-2: " gb)(princ new-rlist gb)(princ "\n" gb)
    (princ "new-token-list-2: " gb)(princ new-token-list gb)(princ "\n" gb)

))





;;
;;  shu-test-shu-cpp-match-repeat-sub-list-3
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-3 ()
  "Second item does not match, nil returned."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " :: bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    ";")
          )
         )
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list))
    (should (not ret-val))

))




;;
;;  shu-test-shu-cpp-match-repeat-sub-list-4
;;
(ert-deftest shu-test-shu-cpp-match-repeat-sub-list-4 ()
  "Third item does not match, nil returned."
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (data
         (concat
          " :: bbbb  ;"
          ))
        (match-list
         (list
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    t shu-cpp-token-type-op
                                    "::")
          )
         )
        (rlist)
        (token-list)
        (match-info)
        (ret-val)
        (new-token-list)
        (new-rlist)
        (token)
        (token-type)
        (token-info))
    (with-temp-buffer
      (insert data)
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max))))
    (setq ret-val (shu-cpp-match-repeat-sub-list rlist token-list match-list))
    (should (not ret-val))

))

;;; new-sidelist.el ends here
