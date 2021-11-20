;;; shu-find-classes.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2021 Stewart L. Palmer
;;
;; Package: shu-find-classes
;; Author: Stewart L. Palmer <stewart@stewartpalmer.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This is also not part of the shu elisp pckage.                                      ;
;; This is experimental code that is unlikely to become part of the
;; shu slisp package in its current form.
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
;; Some experimental code for extracting fully qualified class names
;; from a tokenized list
;;


;;
;;  zzz
;;
(defun zzz ()
  "Open a C++ file and invoke this function.  Results in **shu-unit-tests**"
  (interactive)
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (token-list)
        (rlist)
        (ret-val)
        (glist)
        )
      (setq token-list (shu-cpp-tokenize-region-for-command (point-min) (point-max)))
      (shu-cpp-tokenize-show-list token-list)
      (setq ret-val (shu-cpp-all-search-match-tokens rlist shu-cpp-match-classname-forms token-list))
      (if (not ret-val)
          (message "%s" "No qualified names found.")
        (setq rlist (cdr ret-val))
        (if (not rlist)
            (message "Returned name list is empty.")
          (setq rlist (nreverse rlist))
          (princ "\n\n====================================================\n\n" gb)
          (shu-cpp-tokenize-show-list rlist)
          (setq glist (shu-gather-other rlist))
          (shu-show-glist glist)
            )
          )

    ))



;;
;;  shu-show-glist
;;
(defun shu-show-glist (glist)
  "Doc string."
  (interactive)
  (let (
        (gb      (get-buffer-create shu-unit-test-buffer))
        (gather)
        (token)
        (name)
        )
    (princ "\n\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n" gb)
    (setq glist (sort glist (lambda(lgather rgather)
                             (let (
                                    (less)
                                    )
                              (if (string= (car lgather) (car rgather))
                                  (setq less (string< (cadr lgather) (cadr rgather)))
                                (setq less (string< (car lgather) (car rgather)))
                                  )
                             less
                              )
                              )
                      )
          )
    (while glist
      (setq gather (car glist))
      (setq name "")
      (while gather
        (setq token (car gather))
        (setq name (concat name token))
        (when (cdr gather)
          (setq name (concat name "::"))
          )
        (setq gather (cdr gather))
        )
      (princ (concat name "\n") gb)
      (setq glist (cdr glist))
      )
    ))



;;
;;  shu-gather-other
;;
(defun shu-gather-other (rlist)
  "Doc string."
  (let (
        (token-info)
        (token)
        (token-type)
        (last-token)
        (last-token-type)
        (gathered)
        (glist)
        )
    (when rlist
      (setq token-info (car rlist))
      (setq token (shu-cpp-token-extract-token token-info))
      (setq token-type (shu-cpp-token-extract-type token-info))
      (setq last-token token)
      (setq last-token-type token-type)
      (push token gathered)
      (setq rlist (cdr rlist))
      (while rlist
        (setq token-info (car rlist))
        (setq token (shu-cpp-token-extract-token token-info))
        (setq token-type (shu-cpp-token-extract-type token-info))
        (when (= token-type shu-cpp-token-type-uq)
          (if (string= last-token "::")
              (push token gathered)
            (setq gathered (nreverse gathered))
            (push gathered glist)
            (setq gathered nil)
            (push token gathered)
            )
          )
        (setq last-token token)
        (setq last-token-type token-type)
        (setq rlist (cdr rlist))
        )
      )
    glist
    ))




;;; shu-find-classes.el ends here
