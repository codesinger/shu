;;; shu-match.el --- Shu project code for dealing wth C++ in Emacs
;;
;; Copyright (C) 2019 Stewart L. Palmer
;;
;; Package: shu-match
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

;; Functions that use functions in shu-match.el


;;; Code:

(provide 'shu-match)
(require 'shu-cpp-match)




;;
;;  shu-match-find-variables
;;
(defun shu-match-find-variables ()
  "Find what might be all of the variable declarations in a header file by doing
a reverse tokenized scan looking for all occurrences of operator \";\" followed
by something that matches the regular expression for a C++ name.  Then take each
line that matches and put it in the buffer \"**shu-vars**\"."
  (interactive)
  (let ((token-list)
        (ret-val))
    (setq token-list (shu-cpp-reverse-tokenize-region-for-command (point-min) (point-max)))
    (setq ret-val (shu-match-find-semi-names token-list))
    ))




;;
;;  shu-match-find-semi-names
;;
(defun shu-match-find-semi-names (token-list)
  "With a match list that is a semi-colon followed by the regular expression for
a C++ name, do a reverse tokenized match for all occurrences, then take each line
that holds a match and put it into the buffer \"**shu-vars**\","
  (let ((find-name-list
         (list  ;; semicolon followed by name
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same
                                    'shu-cpp-token-match-same
                                    nil shu-cpp-token-type-op
                                    ";")
          (shu-cpp-make-match-info  shu-cpp-token-match-type-same-rx
                                    'shu-cpp-token-match-same-rx
                                    t shu-cpp-token-type-uq
                                    (concat shu-cpp-name "+"))))
        (tlist token-list)
        (rlist)
        (ret-val)
        (pret-val)
        (something t)
        (gb (get-buffer-create "**shu-vars**"))
        (token-info)
        (spoint)
        (bol)
        (eol)
        (line-no)
        (line))
    (setq ret-val (shu-cpp-all-search-match-tokens rlist find-name-list tlist))
    (setq tlist (car ret-val))
    (setq rlist (cdr ret-val))
    (while rlist
      (setq token-info (car rlist))
      (setq spoint (shu-cpp-token-extract-spoint token-info))
      (goto-char spoint)
      (setq bol (line-beginning-position))
      (setq eol (line-end-position))
      (setq line-no (shu-format-num (line-number-at-pos) 5))
      (setq line (buffer-substring-no-properties bol eol))
      (princ (concat line-no ". " line "\n") gb)
      (setq rlist (cdr rlist)))
    ))



;;
;;  shu-match-set-alias
;;
(defun shu-match-set-alias ()
  "Set the common alias names for the functions in shu-match,
These are generally the same as the function names with the leading
shu- prefix removed."
  (defalias 'find-all-variables 'shu-match-find-variables)
  )


;;; shu-match.el ends here
